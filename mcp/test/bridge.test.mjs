// bridge.test.mjs: end-to-end tests for collabnet-mcp.mjs (node:test, zero dependencies, Node >= 22).
// Run: node --test test/   (from the mcp/ directory)  or  npm test
import test from 'node:test';
import assert from 'node:assert/strict';
import { spawn, execFileSync } from 'node:child_process';
import http from 'node:http';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { connectMockPage } from './mock_page.mjs';

const here = path.dirname(fileURLToPath(import.meta.url));
const BRIDGE = path.join(here, '..', 'collabnet-mcp.mjs');
const TOKEN = 'test-token-' + Math.random().toString(16).slice(2);
const APP = 'http://localhost:8000/';
const tmpHome = fs.mkdtempSync(path.join(os.tmpdir(), 'cnmcp-'));
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

function startBridge(args, env = {}) {
  const p = spawn(process.execPath, [BRIDGE, ...args], { stdio: ['pipe', 'pipe', 'pipe'], env: { ...process.env, HOME: tmpHome, COLLABNET_MCP_TOKEN: '', ...env } });
  const b = { p, stderr: '', lines: [], waiters: [] };
  let out = '';
  p.stdout.on('data', (d) => {
    out += d; let i;
    while ((i = out.indexOf('\n')) >= 0) { const l = out.slice(0, i); out = out.slice(i + 1); if (!l) continue; const m = JSON.parse(l); b.lines.push(m); b.waiters = b.waiters.filter((w) => !w(m)); }
  });
  b.ready = new Promise((res, rej) => {
    p.stderr.on('data', (d) => { b.stderr += d; const m = b.stderr.match(/LISTENING 127\.0\.0\.1:(\d+)/); if (m) { b.port = +m[1]; res(b); } });
    p.on('exit', (c) => rej(new Error('bridge exited ' + c + ': ' + b.stderr)));
    setTimeout(() => rej(new Error('bridge start timeout: ' + b.stderr)), 8000);
  });
  b.send = (o) => p.stdin.write(JSON.stringify(o) + '\n');
  b.waitFor = (pred, ms = 5000) => new Promise((res, rej) => {
    const hit = b.lines.find(pred); if (hit) return res(hit);
    const t = setTimeout(() => rej(new Error('stdio wait timeout')), ms);
    b.waiters.push((m) => { if (pred(m)) { clearTimeout(t); res(m); return true; } return false; });
  });
  b.rpc = (id, method, params) => { b.send({ jsonrpc: '2.0', id, method, ...(params ? { params } : {}) }); return b.waitFor((m) => m.id === id); };
  b.stop = () => new Promise((r) => { if (p.exitCode !== null) return r(); p.on('exit', () => r()); p.kill('SIGTERM'); });
  return b;
}

function req(port, { method = 'POST', path: p = '/mcp', body, token = TOKEN, sid, origin, host, accept = 'application/json, text/event-stream', raw } = {}) {
  return new Promise((resolve, reject) => {
    const h = { Accept: accept, Host: host || `127.0.0.1:${port}` };
    if (body !== undefined || raw) h['Content-Type'] = 'application/json';
    if (token) h.Authorization = 'Bearer ' + token;
    if (sid) h['Mcp-Session-Id'] = sid;
    if (origin) h.Origin = origin;
    const r = http.request({ host: '127.0.0.1', port, method, path: p, headers: h }, (res) => {
      let t = ''; res.on('data', (d) => (t += d)); res.on('end', () => { let json = null; try { json = t ? JSON.parse(t) : null; } catch {} resolve({ status: res.statusCode, sid: res.headers['mcp-session-id'], json, text: t }); });
    });
    r.on('error', reject);
    if (raw) r.end(raw); else r.end(body !== undefined ? JSON.stringify(body) : undefined);
  });
}
function sse(port, sid) {
  const events = []; const waiters = [];
  const s = { events };
  s.ready = new Promise((resolve, reject) => {
    const r = http.request({ host: '127.0.0.1', port, method: 'GET', path: '/mcp', headers: { Accept: 'text/event-stream', Authorization: 'Bearer ' + TOKEN, 'Mcp-Session-Id': sid, Host: `127.0.0.1:${port}` } }, (res) => {
      s.status = res.statusCode; resolve(s);
      let buf = '';
      res.on('data', (d) => {
        buf += d; let i;
        while ((i = buf.indexOf('\n\n')) >= 0) {
          const block = buf.slice(0, i); buf = buf.slice(i + 2);
          const data = block.split('\n').filter((l) => l.startsWith('data:')).map((l) => l.slice(5).trim()).join('\n');
          if (data) { const m = JSON.parse(data); events.push(m); for (const w of [...waiters]) w(m); }
        }
      });
    });
    r.on('error', reject); r.end(); s.req = r;
  });
  s.waitFor = (pred, ms = 5000) => new Promise((res, rej) => {
    const hit = events.find(pred); if (hit) return res(hit);
    const t = setTimeout(() => rej(new Error('sse wait timeout')), ms);
    const w = (m) => { if (pred(m)) { clearTimeout(t); waiters.splice(waiters.indexOf(w), 1); res(m); } }; waiters.push(w);
  });
  s.close = () => s.req.destroy();
  return s;
}
const INIT = (id) => ({ jsonrpc: '2.0', id, method: 'initialize', params: { protocolVersion: '2025-06-18', capabilities: {}, clientInfo: { name: 't', version: '1' } } });
const call = (id, name, args) => ({ jsonrpc: '2.0', id, method: 'tools/call', params: { name, arguments: args } });
const wsClose = (url, origin) => new Promise((resolve) => {
  const ws = new WebSocket(url, { headers: { Origin: origin } }); let opened = false;
  ws.addEventListener('open', () => { opened = true; });
  ws.addEventListener('close', (ev) => resolve({ opened, code: ev.code, reason: ev.reason }));
});
const wsOk = async (url, origin) => { const pg = connectMockPage(url, { origin }); const ok = await pg.opened; if (ok) await pg.close(); return ok; };

test('collabnet-mcp bridge end-to-end', async (t) => {
  const b = startBridge(['--port', '0', '--token', TOKEN, '--stdio', '--app-url', APP, '--timeout', '1.5']);
  await b.ready; const port = b.port;
  const pageUrl = `ws://127.0.0.1:${port}/page?token=${TOKEN}`;
  let page, sid, stream;
  try {
    await t.test('startup prints token, address and one-click link', () => {
      assert.match(b.stderr, new RegExp(`PAIRING TOKEN: ${TOKEN}`));
      assert.ok(b.stderr.includes(`${APP}#mcp=${encodeURIComponent('ws://127.0.0.1:' + port)}&token=${TOKEN}`));
    });
    await t.test('health before page', async () => { const r = await req(port, { method: 'GET', path: '/health', token: null }); assert.equal(r.status, 200); assert.equal(r.json.ok, true); assert.equal(r.json.page_connected, false); });
    await t.test('no page + empty cache: tools/list -> -32000 with link', async () => {
      await b.rpc('i0', 'initialize', { protocolVersion: '2025-03-26' }).then((m) => assert.equal(m.result.protocolVersion, '2025-03-26'));
      const m = await b.rpc('l0', 'tools/list'); assert.equal(m.error.code, -32000); assert.ok(m.error.message.includes('#mcp='));
    });
    await t.test('WS: bad token -> close 4401 "pairing token rejected" (P3-3)', async () => {
      const c = await wsClose(`ws://127.0.0.1:${port}/page?token=nope`, 'http://localhost:8000');
      assert.equal(c.code, 4401); assert.equal(c.reason, 'pairing token rejected');
      const h = await req(port, { method: 'GET', path: '/health' }); assert.equal(h.json.page_connected, false);
    });
    await t.test('WS: bad Origin -> close 4403 (P3-3)', async () => {
      const c = await wsClose(pageUrl, 'https://evil.example'); assert.equal(c.code, 4403); assert.equal(c.reason, 'origin not allowed');
    });
    await t.test('WS: missing Origin rejected', async () => {
      const r = await new Promise((res) => { const q = http.request({ host: '127.0.0.1', port, path: `/page?token=${TOKEN}`, headers: { Connection: 'Upgrade', Upgrade: 'websocket', 'Sec-WebSocket-Key': 'dGhlIHNhbXBsZSBub25jZQ==', 'Sec-WebSocket-Version': '13' } });
        q.on('response', (rs) => res(rs.statusCode)); q.on('error', () => res(-1));
        q.on('upgrade', (rs, sock, head) => { let buf = Buffer.from(head || []);
          const done = () => { if (buf.length >= 4) { sock.destroy(); res(buf[0] === 0x88 ? buf.readUInt16BE(2) : -2); return true; } return false; };
          if (!done()) sock.on('data', (d) => { buf = Buffer.concat([buf, d]); done(); }); });
        q.end(); });
      assert.equal(r, 4403);
    });
    await t.test('WS: allowed origins accepted (github pages)', async () => { assert.equal(await wsOk(pageUrl, 'https://vizkidd.github.io'), true); });

    page = connectMockPage(pageUrl); assert.equal(await page.opened, true);
    await sleep(100);
    await t.test('health with page', async () => { const r = await req(port, { method: 'GET', path: '/health' }); assert.equal(r.json.page_connected, true); assert.ok(r.json.clients >= 1); });

    await t.test('HTTP initialize issues session; stdio initialize', async () => {
      const r = await req(port, { body: INIT(1) }); assert.equal(r.status, 200); assert.ok(r.sid); sid = r.sid;
      assert.equal(r.json.result.protocolVersion, '2025-06-18'); assert.equal(r.json.result.capabilities.tools.listChanged, true);
      const n = await req(port, { body: { jsonrpc: '2.0', method: 'notifications/initialized' }, sid }); assert.equal(n.status, 202);
      const s = await b.rpc(1, 'initialize', { protocolVersion: '2025-06-18' }); assert.equal(s.result.serverInfo.name, 'collabnet-mcp');
      b.send({ jsonrpc: '2.0', method: 'notifications/initialized' });
      const unk = await b.rpc('v', 'initialize', { protocolVersion: '1999-01-01' }); assert.equal(unk.result.protocolVersion, '2025-06-18');
      const p = await req(port, { body: { jsonrpc: '2.0', id: 'p', method: 'ping' }, sid }); assert.deepEqual(p.json.result, {});
    });
    await t.test('P3-5: page receives notifications/collabnet/clients with stdio + HTTP session count', async () => {
      const want = (await req(port, { method: 'GET', path: '/health' })).json.clients;
      assert.equal(want, 2, 'stdio + 1 HTTP session');
      let last = null; const t0 = Date.now();
      while (Date.now() - t0 < 4000) { const c = page.notes.filter((m) => m.method === 'notifications/collabnet/clients'); last = c.at(-1); if (last && last.params.count === want) break; await sleep(100); }
      assert.ok(last, 'clients notification received'); assert.equal(last.params.count, want);
      const s2 = await req(port, { body: INIT('c2') }); assert.ok(s2.sid);
      const t1 = Date.now(); while (Date.now() - t1 < 4000 && page.notes.filter((m) => m.method === 'notifications/collabnet/clients').at(-1).params.count !== want + 1) await sleep(100);
      assert.equal(page.notes.filter((m) => m.method === 'notifications/collabnet/clients').at(-1).params.count, want + 1, 'count updates when a session is added');
      const d = await req(port, { method: 'DELETE', sid: s2.sid }); assert.ok([200, 204].includes(d.status));
      const t2 = Date.now(); while (Date.now() - t2 < 4000 && page.notes.filter((m) => m.method === 'notifications/collabnet/clients').at(-1).params.count !== want) await sleep(100);
      assert.equal(page.notes.filter((m) => m.method === 'notifications/collabnet/clients').at(-1).params.count, want, 'count drops after DELETE');
    });
    await t.test('SSE GET stream opens', async () => { stream = sse(port, sid); await stream.ready; assert.equal(stream.status, 200); });

    await t.test('concurrent stdio + HTTP tools/list + tools/call: ids not crossed', async () => {
      const N = 8; const jobs = [];
      for (let i = 0; i < N; i++) {
        jobs.push(b.rpc(100 + i, 'tools/call', { name: 'collabnet_echo', arguments: { who: 'stdio', i } }).then((m) => ({ src: 'stdio', id: 100 + i, i, m })));
        jobs.push(req(port, { body: call(100 + i, 'collabnet_echo', { who: 'http', i }), sid }).then((r) => ({ src: 'http', id: 100 + i, i, m: r.json })));
      }
      jobs.push(b.rpc('tl', 'tools/list').then((m) => ({ src: 'stdio', list: m })));
      jobs.push(req(port, { body: { jsonrpc: '2.0', id: 'tl', method: 'tools/list' }, sid }).then((r) => ({ src: 'http', list: r.json })));
      const res = await Promise.all(jobs);
      for (const x of res) {
        if (x.list) { assert.equal(x.list.id, 'tl'); assert.deepEqual(x.list.result.tools.map((t) => t.name), ['collabnet_get_status', 'collabnet_echo']); continue; }
        assert.equal(x.m.id, x.id, 'client id restored');
        assert.deepEqual(x.m.result.structuredContent.echo, { who: x.src, i: x.i }, 'response routed to the right client');
        assert.match(x.m.result.structuredContent.page_saw_id, /^b\d+$/, 'page saw a rewritten id');
      }
      assert.equal(new Set(page.seen.map((m) => m.id)).size, page.seen.length, 'rewritten ids unique');
    });

    await t.test('toggle r_eval ON -> list_changed on stdio and SSE; tools/list includes it', async () => {
      const before = b.lines.length;
      page.setREval(true);
      await b.waitFor((m, i) => m.method === 'notifications/tools/list_changed' && b.lines.indexOf(m) >= before);
      await stream.waitFor((m) => m.method === 'notifications/tools/list_changed');
      const s = await b.rpc('tl2', 'tools/list'); assert.ok(s.result.tools.some((t) => t.name === 'collabnet_r_eval'));
      const h = await req(port, { body: { jsonrpc: '2.0', id: 'tl2', method: 'tools/list' }, sid }); assert.ok(h.json.result.tools.some((t) => t.name === 'collabnet_r_eval'));
    });
    await t.test('toggle r_eval OFF -> list_changed again; tool removed', async () => {
      const nS = b.lines.filter((m) => m.method === 'notifications/tools/list_changed').length, nH = stream.events.length;
      page.setREval(false);
      await b.waitFor(() => b.lines.filter((m) => m.method === 'notifications/tools/list_changed').length > nS);
      await stream.waitFor(() => stream.events.length > nH);
      const s = await b.rpc('tl3', 'tools/list'); assert.ok(!s.result.tools.some((t) => t.name === 'collabnet_r_eval'));
      const h = await req(port, { body: { jsonrpc: '2.0', id: 'tl3', method: 'tools/list' }, sid }); assert.equal(h.json.result.tools.length, 2);
    });
    await t.test('page timeout -> -32001', async () => {
      const m = await b.rpc('slow', 'tools/call', { name: 'collabnet_sleep', arguments: {} }); assert.equal(m.error.code, -32001);
    });

    await t.test('HTTP: bad token 401, missing token 401', async () => {
      assert.equal((await req(port, { body: INIT(1), token: 'wrong' })).status, 401);
      assert.equal((await req(port, { body: INIT(1), token: null })).status, 401);
    });
    await t.test('HTTP: non-local Origin 403; local Origin ok', async () => {
      assert.equal((await req(port, { body: INIT(1), origin: 'https://evil.example' })).status, 403);
      assert.equal((await req(port, { body: INIT(1), origin: 'https://vizkidd.github.io' })).status, 403);
      const ok = await req(port, { body: INIT(1), origin: 'http://localhost:3000' }); assert.equal(ok.status, 200);
      await req(port, { method: 'DELETE', sid: ok.sid });
    });
    await t.test('HTTP: bad Host 421 (also for WS upgrade)', async () => {
      assert.equal((await req(port, { body: INIT(1), host: 'evil.example' })).status, 421);
      assert.equal((await req(port, { body: INIT(1), host: `127.0.0.1:${port + 1}` })).status, 421);
      assert.equal((await req(port, { method: 'GET', path: '/health', host: 'attacker.test:' + port })).status, 421);
      assert.equal((await req(port, { body: INIT(1), host: `localhost:${port}` })).status, 200);
    });
    await t.test('HTTP: unknown session 404, missing session 400, oversized body 413', async () => {
      assert.equal((await req(port, { body: { jsonrpc: '2.0', id: 1, method: 'tools/list' }, sid: 'nope' })).status, 404);
      assert.equal((await req(port, { body: { jsonrpc: '2.0', id: 1, method: 'tools/list' } })).status, 400);
      const big = '{"jsonrpc":"2.0","id":1,"method":"ping","params":{"x":"' + 'a'.repeat(1.5 * 1024 * 1024) + '"}}';
      assert.equal((await req(port, { raw: big, sid })).status, 413);
    });
    await t.test('stdio: oversized line -> error, bridge keeps working', async () => {
      b.p.stdin.write('{"x":"' + 'a'.repeat(1.2 * 1024 * 1024) + '"}\n');
      await b.waitFor((m) => m.error && /too large/i.test(m.error.message));
      assert.ok((await b.rpc('afterbig', 'ping')).result);
    });

    await t.test('page disconnected: tools/call error has #mcp= link; tools/list cached with _meta', async () => {
      await page.close(); await sleep(150);
      const c = await b.rpc('dc', 'tools/call', { name: 'collabnet_echo', arguments: {} });
      assert.equal(c.error.code, -32000); assert.ok(c.error.message.includes(`#mcp=${encodeURIComponent('ws://127.0.0.1:' + port)}&token=${TOKEN}`));
      const l = await b.rpc('dl', 'tools/list'); assert.equal(l.result.tools.length, 2); assert.equal(l.result._meta.collabnet.page_connected, false);
      const h = await req(port, { body: call(9, 'collabnet_echo', {}), sid }); assert.equal(h.json.error.code, -32000); assert.ok(h.json.error.message.includes('#mcp='));
      const hl = await req(port, { body: { jsonrpc: '2.0', id: 10, method: 'tools/list' }, sid }); assert.equal(hl.json.result._meta.collabnet.page_connected, false);
    });
    await t.test('DELETE ends the session', async () => {
      assert.equal((await req(port, { method: 'DELETE', sid })).status, 204);
      assert.equal((await req(port, { body: { jsonrpc: '2.0', id: 1, method: 'ping' }, sid })).status, 404);
    });
    await t.test('max 8 HTTP sessions', async () => {
      const have = (await req(port, { method: 'GET', path: '/health' })).json.sessions;
      const codes = []; for (let i = have; i < 9; i++) codes.push((await req(port, { body: INIT(i) })).status);
      assert.deepEqual(codes, [...Array(8 - have).fill(200), 503]);
      assert.equal((await req(port, { method: 'GET', path: '/health' })).json.sessions, 8);
    });
  } finally { if (stream) stream.close(); await b.stop(); }
});

test('rate limiting (defaults 20 burst, 10/s)', async (t) => {
  const b = startBridge(['--port', '0', '--token', TOKEN, '--stdio']); await b.ready; const port = b.port;
  try {
    await t.test('stdio burst -> -32029', async () => {
      for (let i = 0; i < 40; i++) b.send({ jsonrpc: '2.0', id: 'r' + i, method: 'ping' });
      await b.waitFor((m) => m.id === 'r39');
      const limited = b.lines.filter((m) => m.error && m.error.code === -32029).length;
      assert.ok(limited >= 15, `limited=${limited}`);
    });
    await t.test('HTTP burst -> 429', async () => {
      const r = await req(port, { body: INIT(1) }); const sid = r.sid;
      const codes = await Promise.all(Array.from({ length: 40 }, (_, i) => req(port, { body: { jsonrpc: '2.0', id: i, method: 'ping' }, sid }).then((x) => x.status)));
      assert.ok(codes.filter((c) => c === 429).length >= 15, codes.join(','));
      await sleep(1100);
      assert.equal((await req(port, { body: { jsonrpc: '2.0', id: 'later', method: 'ping' }, sid })).status, 200, 'bucket refills');
    });
  } finally { await b.stop(); }
});

test('--print-config prints valid Claude Desktop JSON', () => {
  const out = execFileSync(process.execPath, [BRIDGE, '--print-config', '--token', TOKEN], { env: { ...process.env, HOME: tmpHome } }).toString();
  const cfg = JSON.parse(out);
  assert.equal(cfg.mcpServers.collabnet.command, 'node');
  assert.deepEqual(cfg.mcpServers.collabnet.args, [BRIDGE, '--stdio']);
  assert.equal(cfg.httpClientConfig.collabnet.headers.Authorization, 'Bearer ' + TOKEN);
});

test('--help exits 0', () => { assert.match(execFileSync(process.execPath, [BRIDGE, '--help']).toString(), /--print-config/); });

test('token persistence (temp HOME and --token-file), mode 0600, --new-token rotates, env override', async () => {
  const env = { ...process.env, HOME: tmpHome, COLLABNET_MCP_TOKEN: '' };
  const tok = (args, e = env) => JSON.parse(execFileSync(process.execPath, [BRIDGE, '--print-config', ...args], { env: e }).toString()).httpClientConfig.collabnet.headers.Authorization.slice(7);
  const a = tok([]), b2 = tok([]);
  assert.equal(a, b2); assert.ok(a.length >= 32);
  const f = path.join(tmpHome, '.collabnet-mcp', 'token');
  assert.equal(fs.readFileSync(f, 'utf8').trim(), a);
  if (process.platform !== 'win32') assert.equal(fs.statSync(f).mode & 0o777, 0o600);
  const tf = path.join(tmpHome, 'custom', 'tok');
  const c = tok(['--token-file', tf]), d = tok(['--token-file', tf]);
  assert.equal(c, d); assert.notEqual(c, a);
  const e = tok(['--token-file', tf, '--new-token']); assert.notEqual(e, c); assert.equal(tok(['--token-file', tf]), e);
  assert.equal(tok([], { ...env, COLLABNET_MCP_TOKEN: 'from-env-token-123' }), 'from-env-token-123');
  // a running bridge reports the persisted token
  const br = startBridge(['--port', '0']); await br.ready; await br.stop();
  assert.ok(br.stderr.includes(`PAIRING TOKEN: ${a}`));
});

test.after(() => { fs.rmSync(tmpHome, { recursive: true, force: true }); });
