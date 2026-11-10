// E3: Buy Me a Coffee (BMC) verified unlock — ONE module shared by the Cloudflare Worker and the Pages Function
// (tools/cf_deploy.mjs copies it next to worker.js / into functions/ and wrangler bundles the import).
//
// handle(request, env, opts) -> Response, or null when the path is not ours (caller serves static assets).
// Routes, relative to the base path (also accepted under "/collabnet/" so the BMC webhook URLs work on either base):
//   POST success | failure | monthlysuccess | monthlyfailure      BMC webhooks (GET -> small status page)
//   POST api/gate/code      -> {code}                 one-time donation code, 2 h
//   GET  api/gate/claim?code= -> {status[, token]}    polled by the app while the dialog is open
//   POST api/gate/email {email} -> {token} | {status} weaker fallback: unclaimed donation by this email < 60 min
//   POST api/gate/member {email} -> {active, ends, token?, source}
//   GET  api/gate/status?token= -> {valid, reason}     app re-validates before each export (caches <= 60 s)
//   GET  api/gate/stats      -> counters, no PII
// Storage (KV binding GATE_KV): NO names, notes or raw emails; emails only as SHA-256(lower(trim(email)) + EMAIL_PEPPER).
// EVERY key has a TTL. Secrets come from env (wrangler secret put ...); this file never logs them.

export const WEBHOOKS = {
  success: ["donation.created"],
  failure: ["donation.refunded"],
  monthlysuccess: ["membership.started", "membership.updated", "recurring_donation.started", "recurring_donation.updated"],
  monthlyfailure: ["membership.cancelled", "membership.paused", "recurring_donation.cancelled"],
};
export const SECRET_FOR = { success: "BMC_SECRET_SUCCESS", failure: "BMC_SECRET_FAILURE", monthlysuccess: "BMC_SECRET_MONTHLY_SUCCESS", monthlyfailure: "BMC_SECRET_MONTHLY_FAILURE" };
export const SECRET_NAMES = [...Object.values(SECRET_FOR), "EMAIL_PEPPER", "GATE_TOKEN_SECRET"];
export const REQUIRED_SECRETS = SECRET_NAMES;
export const OPTIONAL_SECRETS = ["BMC_API_TOKEN", "BMC_ACCEPT_TEST_EVENTS", "ALLOWED_ORIGINS"];
export const TTL = { event: 7 * 86400, code: 2 * 3600, token: 12 * 3600, emailWindow: 3600, apiCache: 300, stats: 370 * 86400, statusCache: 60 };
export const MAX_EVENT_AGE = 3 * 86400;          // reject `created` older than ~3 days (replays)
export const RATE_LIMIT = 60;                     // /api/gate/* requests per IP per minute
const CODE_ALPHABET = "23456789ABCDEFGHJKMNPQRSTUVWXYZ";   // no 0/O/1/I/L
const CODE_RE = /C\s*N\s*-?\s*((?:[2-9A-HJKMNP-Z]\s*){6})/i;
const enc = new TextEncoder();

// ---------- crypto helpers ----------
const hex = (buf) => [...new Uint8Array(buf)].map((b) => b.toString(16).padStart(2, "0")).join("");
export async function sha256Hex(s) { return hex(await crypto.subtle.digest("SHA-256", enc.encode(s))); }
export async function hmacHex(secret, data) {
  const k = await crypto.subtle.importKey("raw", enc.encode(secret), { name: "HMAC", hash: "SHA-256" }, false, ["sign"]);
  return hex(await crypto.subtle.sign("HMAC", k, typeof data === "string" ? enc.encode(data) : data));
}
export function timingSafeEqualStr(a, b) {
  a = String(a || ""); b = String(b || "");
  let d = a.length ^ b.length;
  for (let i = 0; i < Math.max(a.length, b.length); i++) d |= (a.charCodeAt(i) || 0) ^ (b.charCodeAt(i) || 0);
  return d === 0;
}
export async function emailHash(email, pepper) {
  const e = String(email || "").trim().toLowerCase();
  return e ? sha256Hex(e + (pepper || "")) : "";
}
const b64u = (s) => btoa(s).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
const unb64u = (s) => atob(s.replace(/-/g, "+").replace(/_/g, "/"));
export async function makeToken(env, claim, now) {
  const body = b64u(JSON.stringify({ ...claim, exp: Math.floor(now / 1000) + TTL.token }));
  return body + "." + (await hmacHex(env.GATE_TOKEN_SECRET, body));
}
export async function readToken(env, token, now) {
  const [body, sig] = String(token || "").split(".");
  if (!body || !sig || !env.GATE_TOKEN_SECRET) return null;
  if (!timingSafeEqualStr(sig, await hmacHex(env.GATE_TOKEN_SECRET, body))) return null;
  let c; try { c = JSON.parse(unb64u(body)); } catch { return null; }
  return c && c.exp * 1000 > now ? c : null;
}
export function newCode(rand = crypto.getRandomValues(new Uint8Array(6))) {
  return "CN-" + [...rand].map((b) => CODE_ALPHABET[b % CODE_ALPHABET.length]).join("");
}
export function findCode(text) {
  const m = CODE_RE.exec(String(text || ""));
  return m ? "CN-" + m[1].replace(/\s+/g, "").toUpperCase() : null;
}

// ---------- tolerant BMC payload parsing (field names are not fully documented; see BMC_WEBHOOK_NOTES.md) ----------
const pick = (o, keys) => { for (const k of keys) if (o && o[k] != null && o[k] !== "") return o[k]; return undefined; };
export function toEpochS(v) {
  if (v == null || v === "") return undefined;
  if (typeof v === "number") return v > 1e12 ? Math.floor(v / 1000) : v;
  if (/^\d+$/.test(String(v))) return toEpochS(Number(v));
  const t = Date.parse(String(v).replace(" ", "T")); return Number.isFinite(t) ? Math.floor(t / 1000) : undefined;
}
export function parseEvent(raw) {
  const ev = typeof raw === "string" ? JSON.parse(raw) : raw;
  const d = (ev && ev.data) || {};
  const dur = String(pick(d, ["membership_level_duration", "duration_type", "duration", "interval", "billing_cycle", "frequency"]) || "").toLowerCase();
  const bool = (v) => v === true || v === 1 || v === "1" || String(v).toLowerCase() === "true";
  return {
    id: String(pick(ev, ["event_id", "id"]) ?? ""), type: String(ev.type || ""), live: !(ev.live_mode === false || ev.live_mode === "false" || ev.live_mode === 0),
    created: toEpochS(ev.created),
    email: pick(d, ["supporter_email", "payer_email", "email", "supporter_email_address"]),
    note: String(pick(d, ["support_note", "supporter_message", "note", "message", "support_message"]) || ""),
    pspId: String(pick(d, ["id", "support_id", "transaction_id", "psp_id", "membership_id", "subscription_id"]) ?? ""),
    periodEnd: toEpochS(pick(d, ["current_period_end", "membership_period_end", "subscription_current_period_end", "period_end", "renews_at", "next_charge_date", "expires_at"])),
    cancelAtPeriodEnd: bool(pick(d, ["cancel_at_period_end", "subscription_cancel_at_period_end"])),
    paused: bool(pick(d, ["paused", "is_paused"])) || /pause/i.test(String(pick(d, ["status", "subscription_status"]) || "")),
    level: String(pick(d, ["membership_level_name", "level_name", "membership_level_id", "level"]) ?? "").slice(0, 60),
    yearly: /year|annual/.test(dur),
  };
}

// ---------- HTTP helpers ----------
const JSONH = { "content-type": "application/json; charset=utf-8", "cache-control": "no-store" };
function json(obj, status = 200, extra = {}) { return new Response(JSON.stringify(obj), { status, headers: { ...JSONH, ...extra } }); }
// E3c: CORS for the app-facing /api/gate/* endpoints ONLY (webhooks never get CORS headers). Same origin is always
// allowed; ALLOWED_ORIGINS is a comma list of exact origins, where a port of "*" matches any port, e.g.
// "https://vizkidd.github.io,http://localhost:*,http://127.0.0.1:*". No credentials (the token travels in the body).
// CORS is not the security boundary: the signed webhook + code match and the signed token are.
export function originAllowed(origin, allowedList) {
  for (const a of allowedList) {
    if (a === origin) return true;
    const m = /^(https?:\/\/[^/:]+):\*$/i.exec(a);
    if (m && new RegExp("^" + m[1].replace(/[.*+?^${}()|[\]\\]/g, "\\$&") + "(:\\d{1,5})?$", "i").test(origin)) return true;
  }
  return false;
}
export function corsHeaders(request, env) {
  const origin = request.headers.get("origin");
  if (!origin) return {};
  const self = new URL(request.url).origin;
  const allowed = String(env.ALLOWED_ORIGINS || "").split(/[\s,]+/).filter(Boolean);
  if (origin !== self && !originAllowed(origin, allowed)) return null;
  return { "access-control-allow-origin": origin, "vary": "Origin", "access-control-allow-methods": "GET, POST, OPTIONS", "access-control-allow-headers": "content-type", "access-control-max-age": "600" };
}
async function kvGet(env, k) { const v = await env.GATE_KV.get(k); if (v == null) return null; try { return JSON.parse(v); } catch { return null; } }
async function kvPut(env, k, v, ttl) { await env.GATE_KV.put(k, JSON.stringify(v), { expirationTtl: Math.max(60, Math.ceil(ttl)) }); }   // KV minimum TTL is 60 s
async function bump(env, name) { const c = (await kvGet(env, "stat:" + name)) || 0; await kvPut(env, "stat:" + name, c + 1, TTL.stats); }
async function readJsonBody(request) { try { return await request.json(); } catch { return {}; } }

// ---------- webhooks ----------
export async function webhook(kind, request, env, now) {
  const secret = env[SECRET_FOR[kind]];
  const raw = await request.text();
  const sig = request.headers.get("x-signature-sha256") || "";
  if (!secret) return json({ error: "not configured" }, 503);
  if (!timingSafeEqualStr(sig.trim().toLowerCase(), await hmacHex(secret, raw))) return json({ error: "bad signature" }, 401);
  let ev; try { ev = parseEvent(raw); } catch { return json({ error: "bad json" }, 400); }
  if (!ev.live && env.BMC_ACCEPT_TEST_EVENTS !== "1") return json({ ok: true, ignored: "test event" });
  if (!WEBHOOKS[kind].includes(ev.type)) return json({ ok: true, ignored: "event type not handled on this path" });
  if (ev.created && now / 1000 - ev.created > MAX_EVENT_AGE) return json({ ok: true, ignored: "too old" });
  const evKey = "evt:" + (await sha256Hex(ev.id || raw));
  if (await env.GATE_KV.get(evKey)) return json({ ok: true, duplicate: true });
  await kvPut(env, evKey, 1, TTL.event);
  const eh = await emailHash(ev.email, env.EMAIL_PEPPER);
  const pspHash = ev.pspId ? (await sha256Hex("psp:" + ev.pspId + (env.EMAIL_PEPPER || ""))).slice(0, 32) : "";
  const t = Math.floor(now / 1000);
  if (ev.type === "donation.created") {
    const code = findCode(ev.note);
    const claimId = pspHash || evKey.slice(4, 36);
    if (code) {
      const c = await kvGet(env, "code:" + code);
      if (c && c.status === "open") await kvPut(env, "code:" + code, { ...c, status: "paid", claimId }, TTL.code);
    }
    if (eh) await kvPut(env, "em:" + eh, { claimId, t, claimed: false }, TTL.emailWindow);
    if (pspHash) await kvPut(env, "don:" + pspHash, { claimId }, TTL.token + 3600);
    return json({ ok: true, linked: !!code });
  }
  if (ev.type === "donation.refunded") {
    const d = pspHash && (await kvGet(env, "don:" + pspHash));
    const claimId = (d && d.claimId) || pspHash;
    if (claimId) await kvPut(env, "rev:" + claimId, 1, TTL.token + 3600);
    if (eh) await env.GATE_KV.delete("em:" + eh);
    return json({ ok: true, revoked: !!claimId });
  }
  if (!eh) return json({ ok: true, ignored: "no email" });
  const mk = "mem:" + eh, rec = await kvGet(env, mk);
  if (/\.(started|updated)$/.test(ev.type)) {
    if (/\.started$/.test(ev.type) && rec && rec.status === "active" && (!rec.current_period_end || rec.current_period_end > t)) await bump(env, "possible_duplicate_membership");
    let ttl, assumed = false;
    if (ev.periodEnd && ev.periodEnd > t) ttl = ev.periodEnd - t + 3 * 86400;
    else { ttl = (ev.yearly ? 370 : 35) * 86400; assumed = true; }
    if (ev.paused) { await env.GATE_KV.delete(mk); return json({ ok: true, removed: "paused" }); }
    await kvPut(env, mk, { kind: ev.type.split(".")[0], status: "active", current_period_end: ev.periodEnd || t + ttl - 3 * 86400,
      cancel_at_period_end: ev.cancelAtPeriodEnd, paused: false, psp_id_hash: pspHash, level: ev.level, period_end_assumed: assumed }, ttl);
    if (assumed) console.log("gate: membership period end missing; assumed", ev.yearly ? "370" : "35", "days");   // no PII
    return json({ ok: true, active: true, period_end_assumed: assumed });
  }
  // cancelled / paused
  if (ev.type.endsWith(".cancelled") && rec && (ev.cancelAtPeriodEnd || rec.cancel_at_period_end)) {
    const end = ev.periodEnd || rec.current_period_end;
    if (end && end > t) { await kvPut(env, mk, { ...rec, cancel_at_period_end: true }, end - t); return json({ ok: true, active_until: end }); }
  }
  await env.GATE_KV.delete(mk);
  return json({ ok: true, removed: true });
}

// ---------- membership lookup (live BMC API if BMC_API_TOKEN, else KV) ----------
export async function memberLookup(env, email, eh, now, fetchFn = fetch) {
  const t = Math.floor(now / 1000);
  if (env.BMC_API_TOKEN) {
    const ck = "apic:" + eh, cached = await kvGet(env, ck);
    if (cached) return { ...cached, source: "bmc_api_cached" };
    try {
      let url = "https://developers.buymeacoffee.com/api/v1/subscriptions?status=active", found = null, pages = 0;
      const want = String(email).trim().toLowerCase();
      while (url && pages++ < 20 && !found) {
        const r = await fetchFn(url, { headers: { authorization: "Bearer " + env.BMC_API_TOKEN, accept: "application/json" } });
        if (!r.ok) throw new Error("bmc api " + r.status);
        const j = await r.json();
        found = (j.data || []).find((s) => String(s.payer_email || "").trim().toLowerCase() === want) || null;
        url = j.next_page_url || null;
      }
      const out = found ? { active: true, ends: toEpochS(found.subscription_current_period_end || found.current_period_end) || null,
        cancel_at_period_end: !!found.subscription_cancel_at_period_end } : { active: false, ends: null };
      await kvPut(env, ck, out, TTL.apiCache);
      return { ...out, source: "bmc_api" };
    } catch (e) { /* fall through to KV */ }
  }
  const rec = await kvGet(env, "mem:" + eh);
  const active = !!(rec && rec.status === "active" && (!rec.current_period_end || rec.current_period_end + 3 * 86400 > t));
  return { active, ends: active ? rec.current_period_end : null, cancel_at_period_end: !!(rec && rec.cancel_at_period_end), source: env.BMC_API_TOKEN ? "kv_fallback" : "kv" };
}

// ---------- app API ----------
// In-memory per-isolate counters (best effort, no KV writes: polling every 5 s would exhaust KV's free 1,000 writes/day).
const RL = new Map();
async function rateLimited(request, env, now) {
  const ip = request.headers.get("cf-connecting-ip") || request.headers.get("x-forwarded-for") || "local";
  const win = Math.floor(now / 60000), k = (await sha256Hex(ip)).slice(0, 24) + ":" + win;
  if (RL.size > 5000) for (const key of RL.keys()) if (!key.endsWith(":" + win)) RL.delete(key);
  const n = RL.get(k) || 0;
  if (n >= RATE_LIMIT) return true;
  RL.set(k, n + 1);
  return false;
}
export async function api(sub, request, env, now, fetchFn) {
  const cors = corsHeaders(request, env);
  if (cors === null) return json({ error: "origin not allowed" }, 403, { vary: "Origin" });   // no ACAO, no data
  const J = (o, s = 200) => json(o, s, cors);
  if (request.method === "OPTIONS") return new Response(null, { status: 204, headers: cors });
  if (await rateLimited(request, env, now)) return J({ error: "rate limited" }, 429);
  const u = new URL(request.url);
  if (sub === "code" && request.method === "POST") {
    const code = newCode(); await kvPut(env, "code:" + code, { status: "open", t: Math.floor(now / 1000) }, TTL.code);
    return J({ code, expires_in: TTL.code });
  }
  if (sub === "claim" && request.method === "GET") {
    const code = findCode(u.searchParams.get("code")); if (!code) return J({ status: "invalid" }, 400);
    const c = await kvGet(env, "code:" + code);
    if (!c) return J({ status: "expired" });
    if (c.status !== "paid") return J({ status: c.status });     // open | claimed
    if (await env.GATE_KV.get("rev:" + c.claimId)) return J({ status: "refunded" });
    await kvPut(env, "code:" + code, { ...c, status: "claimed" }, TTL.code);   // single use
    return J({ status: "paid", token: await makeToken(env, { cid: c.claimId, k: "d" }, now) });
  }
  if (sub === "email" && request.method === "POST") {
    const b = await readJsonBody(request), eh = await emailHash(b.email, env.EMAIL_PEPPER);
    if (!eh) return J({ status: "invalid" }, 400);
    const r = await kvGet(env, "em:" + eh);
    if (!r || r.claimed || now / 1000 - r.t > TTL.emailWindow || (await env.GATE_KV.get("rev:" + r.claimId))) return J({ status: "not_found" });
    await kvPut(env, "em:" + eh, { ...r, claimed: true }, TTL.emailWindow);
    return J({ status: "paid", token: await makeToken(env, { cid: r.claimId, k: "d" }, now) });
  }
  if (sub === "member" && request.method === "POST") {
    const b = await readJsonBody(request), eh = await emailHash(b.email, env.EMAIL_PEPPER);
    if (!eh) return J({ status: "invalid" }, 400);
    const m = await memberLookup(env, b.email, eh, now, fetchFn);
    const out = { active: m.active, ends: m.ends, cancel_at_period_end: m.cancel_at_period_end, source: m.source };
    if (m.active) out.token = await makeToken(env, { cid: "m:" + eh.slice(0, 32), k: "m", eh }, now);
    return J(out);
  }
  if (sub === "status" && (request.method === "GET" || request.method === "POST")) {   // E3c: POST {token} (not in URLs/logs)
    const tok = request.method === "POST" ? (await readJsonBody(request)).token : u.searchParams.get("token");
    const c = await readToken(env, tok, now);
    if (!c) return J({ valid: false, reason: "invalid_or_expired" });
    if (c.k === "d" && (await env.GATE_KV.get("rev:" + c.cid))) return J({ valid: false, reason: "refunded" });
    if (c.k === "m") { const rec = await kvGet(env, "mem:" + c.eh); if (!rec && !env.BMC_API_TOKEN) return J({ valid: false, reason: "membership_ended" }); }
    return J({ valid: true, exp: c.exp, cache_s: TTL.statusCache });
  }
  if (sub === "stats" && request.method === "GET") {
    return J({ possible_duplicate_membership: (await kvGet(env, "stat:possible_duplicate_membership")) || 0 });
  }
  return J({ error: "not found" }, 404);
}

function statusPage(kind) {
  return new Response(`<!doctype html><meta charset="utf-8"><title>CollabNET webhook</title><p>CollabNET Buy Me a Coffee webhook endpoint (<code>${kind}</code>). It accepts signed POSTs from Buy Me a Coffee only. If you donated, return to the CollabNET tab — it unlocks automatically.</p>`,
    { status: 200, headers: { "content-type": "text/html; charset=utf-8", "cache-control": "no-store" } });
}

export async function handle(request, env, { basePath = "/", now = Date.now(), fetchFn } = {}) {
  const p = new URL(request.url).pathname;
  let rel = null;
  for (const b of [basePath, "/collabnet/", "/"]) if (p.startsWith(b)) { rel = p.slice(b.length).replace(/\/$/, ""); if (WEBHOOKS[rel] || rel.startsWith("api/gate/")) break; rel = null; }
  if (rel == null) return null;
  if (!env.GATE_KV) return json({ error: "GATE_KV binding missing" }, 503);
  if (WEBHOOKS[rel]) {
    if (request.method === "POST") return webhook(rel, request, env, now);
    if (request.method === "GET" || request.method === "HEAD") return statusPage(rel);   // no CORS headers on webhooks
    return json({ error: "method not allowed" }, 405, { allow: "GET, POST" });
  }
  return api(rel.slice("api/gate/".length), request, env, now, fetchFn || fetch);
}
