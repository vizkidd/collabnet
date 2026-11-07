#!/usr/bin/env node
// collabnet-mcp.mjs: zero-dependency MCP bridge between local MCP clients and ONE CollabNET browser page.
//   MCP clients: stdio (newline-delimited JSON-RPC, --stdio) and Streamable HTTP (POST/GET/DELETE /mcp).
//   Page:        WebSocket ws://127.0.0.1:PORT/page?token=TOKEN (the page is the MCP server).
// The bridge answers initialize / ping / notifications/initialized itself and forwards everything else
// with ids rewritten to "b<N>" (multiplexing many clients onto one page). See PROTOCOL.md section 4.
// Run `node collabnet-mcp.mjs --help` for usage.
import http from 'node:http';
import crypto from 'node:crypto';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { spawn } from 'node:child_process';
import { fileURLToPath } from 'node:url';

const VERSION = '1.0.0';
const SELF = fileURLToPath(import.meta.url);
const SUPPORTED_VERSIONS = ['2025-11-25', '2025-06-18', '2025-03-26'];
const DEFAULT_VERSION = '2025-06-18';
const MAX_BODY = 1 << 20, MAX_FRAME = 4 << 20, MAX_DRAIN = 16 << 20;
const MAX_SESSIONS = 8, SESSION_IDLE_MS = 30 * 60 * 1000;
const DEFAULT_ORIGINS = ['https://vizkidd.github.io', 'http://localhost:*', 'http://127.0.0.1:*'];

const HELP = `collabnet-mcp ${VERSION}: local MCP bridge for the CollabNET web app

Usage: node collabnet-mcp.mjs [options]

  --stdio               Also serve MCP over stdin/stdout (use this from Claude Desktop & co.)
  --port N              HTTP/WebSocket port on 127.0.0.1 (default 8765; 0 = random)
  --token T             Pairing token (else env COLLABNET_MCP_TOKEN, else the token file)
  --token-file F        Token file (default ~/.collabnet-mcp/token, created with mode 0600)
  --new-token           Generate a new token and overwrite the token file
  --timeout S           Seconds to wait for the page to answer a request (default 200; mutating tools wait for two consent prompts)
  --allow-origin O      Extra allowed page Origin (repeatable or comma-separated; "http://host:*" = any port)
  --app-url URL         CollabNET URL used for the one-click link (default https://vizkidd.github.io/collabnet/)
  --open                Open the one-click link in the default browser (best effort)
  --rate-burst N        Per-client rate limit burst (default 20)
  --rate-per-sec N      Per-client sustained rate (default 10)
  --print-config        Print an MCP client config (JSON) to stdout and exit
  --help                Show this help

Endpoints: POST/GET/DELETE /mcp (Authorization: Bearer TOKEN), GET /health, WS /page?token=TOKEN.
`;

// ------------------------------------------------------------------ CLI
function parseArgs(argv) {
  const o = { stdio: false, port: 8765, token: null, tokenFile: null, newToken: false, timeout: 200, origins: [],
    appUrl: 'https://vizkidd.github.io/collabnet/', open: false, printConfig: false, help: false, burst: 20, rate: 10 };
  const need = (i, k) => { if (i + 1 >= argv.length) die(`missing value for ${k}`); return argv[i + 1]; };
  for (let i = 0; i < argv.length; i++) {
    let k = argv[i], v;
    const eq = k.indexOf('=');
    if (k.startsWith('--') && eq > 0) { v = k.slice(eq + 1); k = k.slice(0, eq); }
    const val = () => (v !== undefined ? v : need(i++, k));
    switch (k) {
      case '--stdio': o.stdio = true; break;
      case '--port': o.port = num(val(), k, 0, 65535); break;
      case '--token': o.token = val(); break;
      case '--token-file': o.tokenFile = val(); break;
      case '--new-token': o.newToken = true; break;
      case '--timeout': o.timeout = num(val(), k, 0.1, 86400); break;
      case '--allow-origin': o.origins.push(...val().split(',').map((s) => s.trim()).filter(Boolean)); break;
      case '--app-url': o.appUrl = val(); break;
      case '--open': o.open = true; break;
      case '--rate-burst': o.burst = num(val(), k, 1, 1e6); break;
      case '--rate-per-sec': o.rate = num(val(), k, 0.001, 1e6); break;
      case '--print-config': o.printConfig = true; break;
      case '--help': case '-h': o.help = true; break;
      default: die(`unknown option ${k} (see --help)`);
    }
  }
  return o;
}
function num(s, k, lo, hi) { const n = Number(s); if (!Number.isFinite(n) || n < lo || n > hi) die(`bad value for ${k}: ${s}`); return n; }
function die(msg) { process.stderr.write(`collabnet-mcp: ${msg}\n`); process.exit(2); }
const log = (...a) => process.stderr.write('[collabnet-mcp] ' + a.join(' ') + '\n');

const opts = parseArgs(process.argv.slice(2));
if (opts.help) { process.stdout.write(HELP); process.exit(0); }

// ------------------------------------------------------------------ token
function resolveToken() {
  if (opts.token) return { token: opts.token, source: '--token' };
  if (process.env.COLLABNET_MCP_TOKEN) return { token: process.env.COLLABNET_MCP_TOKEN, source: 'env COLLABNET_MCP_TOKEN' };
  const file = path.resolve(opts.tokenFile || path.join(os.homedir(), '.collabnet-mcp', 'token'));
  if (!opts.newToken) {
    try {
      const st = fs.statSync(file);
      const t = fs.readFileSync(file, 'utf8').trim();
      if (/^[A-Za-z0-9_-]{16,256}$/.test(t)) {
        if (process.platform !== 'win32' && (st.mode & 0o077)) { try { fs.chmodSync(file, 0o600); log('tightened permissions on', file); } catch {} }
        return { token: t, source: file };
      }
      log('token file is malformed, generating a new token:', file);
    } catch (e) { if (e.code !== 'ENOENT') log('cannot read token file', file, e.code || e.message); }
  }
  const t = crypto.randomBytes(24).toString('base64url');
  try {
    fs.mkdirSync(path.dirname(file), { recursive: true, mode: 0o700 });
    const tmp = `${file}.${process.pid}.tmp`;
    fs.writeFileSync(tmp, t + '\n', { mode: 0o600, flag: 'w' });
    fs.renameSync(tmp, file);
    try { fs.chmodSync(file, 0o600); } catch {}
    return { token: t, source: file + ' (new)' };
  } catch (e) { log('cannot write token file', file, e.message, '- using an ephemeral token'); return { token: t, source: 'ephemeral' }; }
}
const { token: TOKEN, source: TOKEN_SOURCE } = resolveToken();
const TOKEN_BUF = Buffer.from(TOKEN);
function tokenOk(t) {
  if (typeof t !== 'string') return false;
  const a = crypto.createHash('sha256').update(t).digest(), b = crypto.createHash('sha256').update(TOKEN_BUF).digest();
  return crypto.timingSafeEqual(a, b); // hashing first makes the comparison length-independent
}

let PORT = opts.port;
const wsUrl = () => `ws://127.0.0.1:${PORT}`;
const oneClickLink = () => `${opts.appUrl}#mcp=${encodeURIComponent(wsUrl())}&token=${TOKEN}`;

if (opts.printConfig) {
  const cfg = {
    mcpServers: { collabnet: { command: 'node', args: [SELF, '--stdio', ...(opts.port !== 8765 ? ['--port', String(opts.port)] : [])] } },
    httpClientConfig: { collabnet: { type: 'http', url: `http://127.0.0.1:${opts.port}/mcp`, headers: { Authorization: `Bearer ${TOKEN}` } } },
  };
  process.stdout.write(JSON.stringify(cfg, null, 2) + '\n');
  process.exit(0);
}

// ------------------------------------------------------------------ origin / host checks
function originMatches(origin, pattern) {
  if (pattern.endsWith(':*')) {
    const base = pattern.slice(0, -2);
    return origin === base || (origin.startsWith(base + ':') && /^\d{1,5}$/.test(origin.slice(base.length + 1)));
  }
  return origin === pattern;
}
const PAGE_ORIGINS = [...DEFAULT_ORIGINS, ...opts.origins];
const pageOriginOk = (o) => typeof o === 'string' && PAGE_ORIGINS.some((p) => originMatches(o, p));
const LOCAL_ORIGIN = /^https?:\/\/(localhost|127\.0\.0\.1)(:\d{1,5})?$/;
const httpOriginOk = (o) => o === undefined || LOCAL_ORIGIN.test(o) || opts.origins.some((p) => originMatches(o, p));
const hostOk = (h) => h === `127.0.0.1:${PORT}` || h === `localhost:${PORT}`;

// ------------------------------------------------------------------ JSON-RPC helpers
const rpcErr = (id, code, message, data) => ({ jsonrpc: '2.0', id: id ?? null, error: data ? { code, message, data } : { code, message } });
const rpcRes = (id, result) => ({ jsonrpc: '2.0', id, result });
const isRequest = (m) => m && typeof m === 'object' && typeof m.method === 'string' && m.id !== undefined && m.id !== null;
const noPageMsg = () => `No CollabNET page is connected to the bridge. Open this link in your browser to connect it: ${oneClickLink()}`;

function makeBucket() { return { tokens: opts.burst, t: Date.now() }; }
function refill(b) { const now = Date.now(); b.tokens = Math.min(opts.burst, b.tokens + ((now - b.t) / 1000) * opts.rate); b.t = now; return b.tokens >= 1; }
function take(b) {
  if (!refill(b)) return false;
  b.tokens -= 1; return true;
}

// ------------------------------------------------------------------ page state & forwarding
let page = null;            // {send, close, socket}
let seq = 0;
const pending = new Map();  // rid -> {client, origId, resolve, timer, method}
let toolsCache = null;      // last tools/list result from the page
let toolsCacheAt = null;

function initializeResult(params) {
  const req = params && params.protocolVersion;
  return {
    protocolVersion: SUPPORTED_VERSIONS.includes(req) ? req : DEFAULT_VERSION,
    capabilities: { tools: { listChanged: true }, resources: {} },
    serverInfo: { name: 'collabnet-mcp', title: 'CollabNET (browser bridge)', version: VERSION },
    instructions: 'Tools are served by a CollabNET page open in the user\'s browser. If calls fail with "No CollabNET page", ask the user to open the link in the error message.',
  };
}

function sendToPage(msg) { page.send(JSON.stringify(msg)); }
// P3-5: tell the page how many MCP clients (stdio + HTTP sessions) are connected, whenever the number changes.
let lastClientCount = -1;
const clientCount = () => (stdioClient ? 1 : 0) + sessions.size;
function notifyClients(force) {
  const n = clientCount();
  if (!page || (!force && n === lastClientCount)) { if (!page) lastClientCount = -1; return; }
  lastClientCount = n;
  try { sendToPage({ jsonrpc: '2.0', method: 'notifications/collabnet/clients', params: { count: n } }); } catch {}
}
setInterval(() => notifyClients(false), 2000).unref();

function pageRequest(client, msg) {
  const rid = 'b' + (++seq);
  return new Promise((resolve) => {
    const timer = setTimeout(() => {
      pending.delete(rid);
      resolve(rpcErr(msg.id, -32001, `CollabNET page did not answer within ${opts.timeout} s (is a consent prompt waiting in the browser?)`));
    }, opts.timeout * 1000);
    pending.set(rid, { client, origId: msg.id, resolve, timer, method: msg.method });
    try { sendToPage({ ...msg, id: rid }); } catch { clearTimeout(timer); pending.delete(rid); resolve(rpcErr(msg.id, -32000, noPageMsg())); }
  });
}

// Handle one client message; resolves to a response object or null (notifications).
async function handle(client, msg) {
  if (!msg || typeof msg !== 'object' || Array.isArray(msg) || msg.jsonrpc !== '2.0') return rpcErr(msg && msg.id, -32600, 'Invalid Request');
  if (typeof msg.method !== 'string') return null; // a response from the client (we never send requests): ignore
  if (!take(client.bucket)) return isRequest(msg) ? rpcErr(msg.id, -32029, `Rate limit exceeded (${opts.burst} burst, ${opts.rate}/s)`) : null;
  if (!isRequest(msg)) {
    if (msg.method === 'notifications/initialized') { client.initialized = true; return null; }
    if (page) {
      if (msg.method === 'notifications/cancelled' && msg.params && msg.params.requestId !== undefined) {
        for (const [rid, p] of pending) if (p.client === client && p.origId === msg.params.requestId) { sendToPage({ ...msg, params: { ...msg.params, requestId: rid } }); break; }
      } else sendToPage(msg);
    }
    return null;
  }
  if (msg.method === 'initialize') return rpcRes(msg.id, initializeResult(msg.params));
  if (msg.method === 'ping') return rpcRes(msg.id, {});
  if (!page) {
    if (msg.method === 'tools/list') {
      if (!toolsCache) return rpcErr(msg.id, -32000, noPageMsg());
      const meta = { ...(toolsCache._meta || {}), collabnet: { page_connected: false, cached_at: toolsCacheAt, connect_link: oneClickLink() } };
      return rpcRes(msg.id, { ...toolsCache, _meta: meta });
    }
    return rpcErr(msg.id, -32000, noPageMsg());
  }
  return pageRequest(client, msg);
}

async function handleBatch(client, parsed) {
  if (Array.isArray(parsed)) {
    if (!parsed.length) return rpcErr(null, -32600, 'Invalid Request: empty batch');
    const out = (await Promise.all(parsed.map((m) => handle(client, m)))).filter(Boolean);
    return out.length ? out : null;
  }
  return handle(client, parsed);
}

function refreshToolsCache() {
  if (!page) return;
  pageRequest(null, { jsonrpc: '2.0', id: '_internal', method: 'tools/list', params: {} }).then(() => {});
}

function broadcast(msg) {
  const line = JSON.stringify(msg);
  if (stdioClient) writeStdout(line);
  for (const s of sessions.values()) for (const res of s.streams) { try { res.write(`event: message\ndata: ${line}\n\n`); } catch {} }
}

function onPageMessage(text) {
  let m; try { m = JSON.parse(text); } catch { log('ignoring non-JSON frame from page'); return; }
  if (!m || typeof m !== 'object') return;
  if (Array.isArray(m)) { m.forEach((x) => onPageObject(x)); return; }
  onPageObject(m);
}
function onPageObject(m) {
  if (typeof m.method === 'string') {
    if (isRequest(m)) { // the page asking the bridge something
      sendToPage(m.method === 'ping' ? rpcRes(m.id, {}) : rpcErr(m.id, -32601, `Method not found: ${m.method}`));
      return;
    }
    if (m.method === 'notifications/tools/list_changed') refreshToolsCache();
    broadcast(m);
    return;
  }
  const p = pending.get(m.id); if (!p) return;
  pending.delete(m.id); clearTimeout(p.timer);
  if (p.method === 'tools/list' && m.result && Array.isArray(m.result.tools)) { toolsCache = m.result; toolsCacheAt = new Date().toISOString(); }
  p.resolve({ ...m, id: p.origId });
}
function failAllPending(reason) {
  for (const [, p] of pending) { clearTimeout(p.timer); p.resolve(rpcErr(p.origId, -32000, `${reason}. ${noPageMsg()}`)); }
  pending.clear();
}

// ------------------------------------------------------------------ minimal RFC 6455 server side
function wsFrame(op, payload) {
  const len = payload.length; let hdr;
  if (len < 126) hdr = Buffer.from([0x80 | op, len]);
  else if (len < 65536) { hdr = Buffer.alloc(4); hdr[0] = 0x80 | op; hdr[1] = 126; hdr.writeUInt16BE(len, 2); }
  else { hdr = Buffer.alloc(10); hdr[0] = 0x80 | op; hdr[1] = 127; hdr.writeBigUInt64BE(BigInt(len), 2); }
  return Buffer.concat([hdr, payload]);
}
function attachWS(socket, head, onText, onClose) {
  let buf = head && head.length ? Buffer.from(head) : Buffer.alloc(0), frag = [], fragLen = 0, fragOp = 0, closed = false, alive = true;
  const send = (s) => { if (!socket.destroyed) socket.write(wsFrame(1, Buffer.from(s))); };
  const close = (code = 1000) => {
    if (closed) return; closed = true;
    const b = Buffer.alloc(2); b.writeUInt16BE(code);
    try { socket.write(wsFrame(8, b)); } catch {}
    socket.end(); setTimeout(() => socket.destroy(), 1000).unref();
  };
  const parse = () => {
    while (!closed) {
      if (buf.length < 2) return;
      const fin = buf[0] & 0x80, op = buf[0] & 0x0f, masked = buf[1] & 0x80; let len = buf[1] & 0x7f, off = 2;
      if (!masked) return close(1002);
      if (len === 126) { if (buf.length < 4) return; len = buf.readUInt16BE(2); off = 4; }
      else if (len === 127) { if (buf.length < 10) return; const L = buf.readBigUInt64BE(2); if (L > BigInt(MAX_FRAME)) return close(1009); len = Number(L); off = 10; }
      if (len > MAX_FRAME) return close(1009);
      if (buf.length < off + 4 + len) return;
      const mask = buf.subarray(off, off + 4), data = Buffer.from(buf.subarray(off + 4, off + 4 + len));
      for (let i = 0; i < data.length; i++) data[i] ^= mask[i & 3];
      buf = buf.subarray(off + 4 + len);
      alive = true;
      if (op === 8) return close(1000);
      if (op === 9) { socket.write(wsFrame(10, data)); continue; }
      if (op === 10) continue;
      if (op === 1 || op === 2) { frag = [data]; fragLen = data.length; fragOp = op; }
      else if (op === 0) { frag.push(data); fragLen += data.length; }
      else return close(1003);
      if (fragLen > MAX_FRAME) return close(1009);
      if (fin) { const d = Buffer.concat(frag); frag = []; fragLen = 0; if (fragOp === 1) onText(d.toString('utf8')); }
    }
  };
  const hb = setInterval(() => { if (!alive) { socket.destroy(); return; } alive = false; try { socket.write(wsFrame(9, Buffer.alloc(0))); } catch {} }, 30000);
  hb.unref();
  socket.setNoDelay(true);
  socket.on('data', (d) => { buf = buf.length ? Buffer.concat([buf, d]) : d; if (buf.length > MAX_FRAME + 16) { /* parse checks len */ } parse(); });
  socket.on('close', () => { clearInterval(hb); closed = true; onClose(); });
  socket.on('error', () => {});
  if (buf.length) parse();
  return { send, close, socket };
}

// ------------------------------------------------------------------ HTTP (Streamable HTTP) server
const sessions = new Map(); // sid -> {id, bucket, streams:Set<res>, last, initialized}
const anonBucket = makeBucket();
function reply(res, status, body, headers = {}) {
  if (res.headersSent) return;
  const h = { 'Cache-Control': 'no-store', 'X-Content-Type-Options': 'nosniff', ...headers };
  if (body !== undefined) { const s = JSON.stringify(body); h['Content-Type'] = 'application/json'; h['Content-Length'] = Buffer.byteLength(s); res.writeHead(status, h); res.end(s); }
  else { res.writeHead(status, h); res.end(); }
}
function endSession(s) { for (const r of s.streams) { try { r.end(); } catch {} } s.streams.clear(); sessions.delete(s.id); notifyClients(false); }
function evictIdle() { const now = Date.now(); for (const s of sessions.values()) if (!s.streams.size && now - s.last > SESSION_IDLE_MS) endSession(s); }

function readBody(req, res, cb) {
  const declared = Number(req.headers['content-length'] || 0);
  if (declared > MAX_DRAIN) { reply(res, 413, rpcErr(null, -32600, 'Payload too large'), { Connection: 'close' }); req.destroy(); return; }
  const chunks = []; let size = 0, tooBig = false;
  req.on('data', (c) => {
    size += c.length;
    if (size > MAX_BODY) { // discard (do not buffer) up to MAX_DRAIN so the client can read the 413
      if (!tooBig) { tooBig = true; chunks.length = 0; }
      if (size > MAX_DRAIN) { reply(res, 413, rpcErr(null, -32600, 'Payload too large'), { Connection: 'close' }); req.destroy(); }
    } else chunks.push(c);
  });
  req.on('end', () => { if (tooBig) return reply(res, 413, rpcErr(null, -32600, 'Payload too large')); cb(Buffer.concat(chunks).toString('utf8')); });
  req.on('error', () => {});
}

const server = http.createServer((req, res) => {
  if (!hostOk(req.headers.host)) return reply(res, 421, { error: 'Misdirected Request: Host must be 127.0.0.1:PORT or localhost:PORT' });
  if (!httpOriginOk(req.headers.origin)) return reply(res, 403, { error: 'Origin not allowed' });
  const url = new URL(req.url, 'http://127.0.0.1');
  if (url.pathname === '/health' && req.method === 'GET') {
    return reply(res, 200, { ok: true, page_connected: !!page, clients: (stdioClient ? 1 : 0) + sessions.size, sessions: sessions.size, stdio: !!stdioClient, version: VERSION });
  }
  if (url.pathname !== '/mcp') return reply(res, 404, { error: 'not found' });
  const auth = req.headers.authorization || '';
  if (!/^Bearer /i.test(auth) || !tokenOk(auth.slice(7).trim())) return reply(res, 401, rpcErr(null, -32001, 'Unauthorized: missing or wrong Bearer token'), { 'WWW-Authenticate': 'Bearer' });
  const sidHdr = req.headers['mcp-session-id'];
  const sess = sidHdr ? sessions.get(sidHdr) : undefined;
  if (sess) sess.last = Date.now();

  if (req.method === 'GET') {
    if (!/text\/event-stream/.test(req.headers.accept || '')) return reply(res, 406, { error: 'Accept: text/event-stream required' });
    if (!sidHdr) return reply(res, 400, rpcErr(null, -32600, 'Missing Mcp-Session-Id'));
    if (!sess) return reply(res, 404, rpcErr(null, -32600, 'Unknown session'));
    if (sess.streams.size >= 2) return reply(res, 409, { error: 'too many streams for this session' });
    res.writeHead(200, { 'Content-Type': 'text/event-stream', 'Cache-Control': 'no-store', Connection: 'keep-alive', 'Mcp-Session-Id': sess.id });
    res.write(': collabnet-mcp stream open\n\n');
    sess.streams.add(res);
    const ka = setInterval(() => { try { res.write(': keepalive\n\n'); } catch {} }, 25000); ka.unref();
    const done = () => { clearInterval(ka); sess.streams.delete(res); };
    req.on('close', done); res.on('close', done);
    return;
  }
  if (req.method === 'DELETE') {
    if (!sess) return reply(res, 404, rpcErr(null, -32600, 'Unknown session'));
    endSession(sess); return reply(res, 204);
  }
  if (req.method !== 'POST') return reply(res, 405, { error: 'method not allowed' }, { Allow: 'GET, POST, DELETE' });

  readBody(req, res, async (text) => {
    let parsed; try { parsed = JSON.parse(text); } catch { return reply(res, 400, rpcErr(null, -32700, 'Parse error')); }
    const list = Array.isArray(parsed) ? parsed : [parsed];
    const isInit = list.some((m) => m && m.method === 'initialize');
    const headers = {};
    let s = sess;
    if (isInit) {
      if (!take(anonBucket)) return reply(res, 429, rpcErr(null, -32029, 'Rate limit exceeded'), { 'Retry-After': '1' });
      evictIdle();
      if (sessions.size >= MAX_SESSIONS) return reply(res, 503, rpcErr(null, -32000, `Too many sessions (max ${MAX_SESSIONS}); DELETE an old one`));
      s = { id: crypto.randomUUID(), bucket: makeBucket(), streams: new Set(), last: Date.now(), initialized: false };
      sessions.set(s.id, s); notifyClients(false);
    } else if (!sidHdr) return reply(res, 400, rpcErr(null, -32600, 'Missing Mcp-Session-Id (send initialize first)'));
    else if (!s) return reply(res, 404, rpcErr(null, -32600, 'Unknown or expired session; re-initialize'));
    headers['Mcp-Session-Id'] = s.id;
    if (!refill(s.bucket)) return reply(res, 429, rpcErr(list.length === 1 && list[0] ? list[0].id : null, -32029, 'Rate limit exceeded'), { ...headers, 'Retry-After': '1' });
    const out = await handleBatch(s, parsed);
    if (!out) return reply(res, 202, undefined, headers);
    reply(res, 200, out, headers);
  });
});
server.headersTimeout = 15000;
server.requestTimeout = 0; // SSE streams and slow page calls are long-lived

server.on('upgrade', (req, socket, head) => {
  socket.on('error', () => {});
  const deny = (code, text) => { socket.end(`HTTP/1.1 ${code} ${text}\r\nConnection: close\r\nContent-Length: 0\r\n\r\n`); };
  if (!hostOk(req.headers.host)) return deny(421, 'Misdirected Request');
  const url = new URL(req.url, 'http://127.0.0.1');
  if (url.pathname !== '/page' && url.pathname !== '/page/') return deny(404, 'Not Found');
  const key = req.headers['sec-websocket-key'];
  if (!key || (req.headers.upgrade || '').toLowerCase() !== 'websocket') return deny(400, 'Bad Request');
  const accept = crypto.createHash('sha1').update(key + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11').digest('base64');
  const handshake = () => socket.write(`HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: ${accept}\r\n\r\n`);
  // Browsers hide HTTP status codes of failed upgrades (every failure looks like close code 1006). To let the page
  // tell "wrong token / origin" apart from "bridge not running", complete the handshake and immediately send a close
  // frame with a distinct code (4403 origin, 4401 token). No message is read from such a socket.
  const refuse = (code, reason) => {
    handshake();
    const r = Buffer.from(reason, 'utf8'), b = Buffer.alloc(2 + r.length); b.writeUInt16BE(code); r.copy(b, 2);
    try { socket.write(wsFrame(8, b)); } catch {}
    socket.end(); setTimeout(() => socket.destroy(), 1000).unref();
  };
  if (!pageOriginOk(req.headers.origin)) { log('rejected page: Origin not allowed:', JSON.stringify(req.headers.origin || null)); return refuse(4403, 'origin not allowed'); }
  if (!tokenOk(url.searchParams.get('token'))) { log('rejected page: wrong token'); return refuse(4401, 'pairing token rejected'); }
  handshake();
  if (page) { log('a new page connected; replacing the previous one'); const old = page; page = null; failAllPending('Page replaced'); old.close(1001); }
  const conn = attachWS(socket, head, onPageMessage, () => {
    if (page === conn) { page = null; failAllPending('Page disconnected'); log('page disconnected'); }
  });
  page = conn;
  log('page connected from', req.headers.origin);
  notifyClients(true);
  refreshToolsCache();
});

// ------------------------------------------------------------------ stdio
let stdioClient = null;
function writeStdout(line) { process.stdout.write(line + '\n'); }
function startStdio() {
  stdioClient = { id: 'stdio', bucket: makeBucket(), initialized: false };
  notifyClients(false);
  let parts = [], len = 0, discarding = false;
  const onLine = async (line) => {
    if (!line.trim()) return;
    let parsed; try { parsed = JSON.parse(line); } catch { writeStdout(JSON.stringify(rpcErr(null, -32700, 'Parse error'))); return; }
    const out = await handleBatch(stdioClient, parsed);
    if (out) writeStdout(JSON.stringify(out));
  };
  process.stdin.on('data', (chunk) => {
    let start = 0;
    for (;;) {
      const nl = chunk.indexOf(10, start);
      const piece = chunk.subarray(start, nl < 0 ? chunk.length : nl);
      if (!discarding) {
        len += piece.length;
        if (len > MAX_BODY) { discarding = true; parts = []; } else parts.push(piece);
      }
      if (nl < 0) break;
      if (discarding) writeStdout(JSON.stringify(rpcErr(null, -32600, `Message too large (max ${MAX_BODY} bytes)`)));
      else onLine(Buffer.concat(parts).toString('utf8'));
      parts = []; len = 0; discarding = false; start = nl + 1;
    }
  });
  process.stdin.on('end', () => { if (len && !discarding) onLine(Buffer.concat(parts).toString('utf8')); log('stdin closed, exiting'); setTimeout(shutdown, 50); });
  process.stdout.on('error', () => shutdown());
}

// ------------------------------------------------------------------ startup
function openBrowser(url) {
  try {
    const [cmd, args] = process.platform === 'darwin' ? ['open', [url]] : process.platform === 'win32' ? ['cmd', ['/c', 'start', '""', url.replace(/&/g, '^&')]] : ['xdg-open', [url]];
    const p = spawn(cmd, args, { stdio: 'ignore', detached: true });
    p.on('error', () => log('could not open a browser; open the link above manually'));
    p.unref();
  } catch { log('could not open a browser; open the link above manually'); }
}
function shutdown() {
  for (const s of [...sessions.values()]) endSession(s);
  if (page) page.close(1001);
  server.close();
  setTimeout(() => process.exit(0), 100).unref();
}
process.on('SIGINT', shutdown);
process.on('SIGTERM', shutdown);

server.on('error', (e) => {
  if (e.code === 'EADDRINUSE') log(`port ${PORT} is already in use (another collabnet-mcp running?). Use --port N or --port 0.`);
  else log('server error:', e.message);
  process.exit(1);
});
server.listen(PORT, '127.0.0.1', () => {
  PORT = server.address().port;
  log(`collabnet-mcp ${VERSION}`);
  log(`PAIRING TOKEN: ${TOKEN}  (from ${TOKEN_SOURCE})`);
  log(`LISTENING 127.0.0.1:${PORT}  (HTTP MCP: http://127.0.0.1:${PORT}/mcp, page WS: ${wsUrl()}/page)`);
  log(`ONE-CLICK LINK (open in your browser to connect CollabNET): ${oneClickLink()}`);
  if (opts.stdio) log('stdio MCP transport enabled');
  if (opts.open) openBrowser(oneClickLink());
});
if (opts.stdio) startStdio();
