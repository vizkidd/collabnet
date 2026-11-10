#!/usr/bin/env node
// E2: stage a built Shinylive export (wasm_build/) for Cloudflare Workers (static assets + a tiny Worker) or
// Cloudflare Pages (static assets + Pages Functions), run the size preflight, then deploy / dry-run / serve via a
// pinned wrangler. Called by buildWASM.R; usable on its own:
//   node tools/cf_deploy.mjs --target=workers|pages [--src=wasm_build] [--out=cf_build] [--base-path=/|/collabnet/]
//        [--project-name=collabnet] [--paid-plan] [--dry-run] [--serve] [--port=8787]
// Credentials: ONLY the env vars CLOUDFLARE_API_TOKEN and CLOUDFLARE_ACCOUNT_ID (read by wrangler itself).
// Nothing here writes them anywhere; a real deploy is refused when they are missing (--dry-run / --serve work without).
import { REQUIRED_SECRETS, OPTIONAL_SECRETS } from "./gate_core.mjs";
import { readdirSync, statSync, mkdirSync, rmSync, cpSync, writeFileSync, readFileSync, existsSync } from "node:fs";
import { join, dirname, resolve } from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
export const WRANGLER_VERSION = JSON.parse(readFileSync(join(HERE, "..", "package.json"), "utf8")).devDependencies.wrangler;
export const MIB = 1024 * 1024;
// Cloudflare static-asset limits (https://developers.cloudflare.com/workers/platform/limits/ , Pages limits page):
// 25 MiB per file; 20,000 files on the Free plan, 100,000 on Workers Paid.
export const LIMITS = { fileBytes: 25 * MIB, filesFree: 20000, filesPaid: 100000 };
export const COMPAT_DATE = "2025-10-01";

export function parseArgs(argv) {
  const a = { target: "workers", src: "wasm_build", out: "cf_build", basePath: "/", projectName: "collabnet",
              paidPlan: false, dryRun: false, serve: false, port: 8787 };
  for (const s of argv) {
    const [k, v] = s.replace(/^--/, "").split(/=(.*)/s);
    switch (k) {
      case "target": a.target = v; break;
      case "src": a.src = v; break;
      case "out": a.out = v; break;
      case "base-path": a.basePath = normBasePath(v); break;
      case "project-name": a.projectName = v; break;
      case "paid-plan": a.paidPlan = true; break;
      case "dry-run": a.dryRun = true; break;
      case "serve": a.serve = true; break;
      case "port": a.port = Number(v); break;
      default: throw new Error(`unknown option --${k}`);
    }
  }
  if (!["workers", "pages"].includes(a.target)) throw new Error(`--target must be workers or pages (got ${a.target})`);
  if (!/^[a-z0-9][a-z0-9-]{0,62}$/.test(a.projectName)) throw new Error(`--project-name must be lowercase letters, digits, dashes (got ${a.projectName})`);
  return a;
}

// "/", "collabnet", "/collabnet", "/collabnet/" -> "/" or "/collabnet/"
export function normBasePath(p) {
  const parts = String(p ?? "/").split("/").filter(Boolean);
  if (parts.some((s) => s === "." || s === ".." || !/^[A-Za-z0-9._-]+$/.test(s))) throw new Error(`bad --base-path ${p}`);
  return parts.length ? "/" + parts.join("/") + "/" : "/";
}

export function walk(dir) {
  const out = [];
  const rec = (d) => { for (const e of readdirSync(d, { withFileTypes: true })) { const f = join(d, e.name); if (e.isDirectory()) rec(f); else out.push(f); } };
  rec(dir); return out;
}

export function preflight(dir, { paidPlan = false } = {}) {
  const files = walk(dir), maxFiles = paidPlan ? LIMITS.filesPaid : LIMITS.filesFree, errors = [];
  const big = files.map((f) => [f, statSync(f).size]).filter(([, s]) => s > LIMITS.fileBytes);
  for (const [f, s] of big) errors.push(`file larger than 25 MiB: ${f.slice(dir.length + 1)} (${(s / MIB).toFixed(1)} MiB)`);
  if (files.length > maxFiles) errors.push(`too many files: ${files.length} > ${maxFiles} (${paidPlan ? "Workers Paid" : "Free plan; use --paid-plan if your account is on Workers Paid"})`);
  return { ok: errors.length === 0, errors, fileCount: files.length, maxFiles };
}

// _headers (same syntax for Workers static assets and Pages). No COOP/COEP: Shinylive's webR falls back to the
// PostMessage channel when the page is not crossOriginIsolated (shinylive src/hooks/useWebR.tsx), and its service
// worker only adds COI headers when ?coi=1 is requested; forcing COEP would also block the cross-origin Buy Me a Coffee
// button (script, fonts) and any non-CORP third-party resource. The CSP stays the <meta> injected by csp_inject.py.
export function headersFile(basePath = "/") {
  const b = normBasePath(basePath);
  return [
    `${b}*`, "  X-Content-Type-Options: nosniff", "  Referrer-Policy: strict-origin-when-cross-origin", "",
    // the shell, app bundle and service worker change every build: always revalidate
    `${b}`, "  Cache-Control: no-cache", "",
    `${b}index.html`, "  Cache-Control: no-cache", "",
    `${b}shinylive-sw.js`, "  Cache-Control: no-cache", "",
    `${b}app.json`, "  Cache-Control: no-cache", "",
    // Shinylive runtime + webR packages: versioned by the pinned shinylive assets, but paths are NOT content-hashed,
    // so cache for a day (not "immutable") and revalidate in the background
    `${b}shinylive/*`, "  Cache-Control: public, max-age=86400, stale-while-revalidate=604800", "",
    `${b}shinylive/webr/R.wasm`, "  Content-Type: application/wasm", "",
    `${b}success/*`, "  X-Robots-Tag: noindex", "",
    `${b}failure/*`, "  X-Robots-Tag: noindex", "",
  ].join("\n");
}

// E3: the BMC webhook + verified-unlock logic lives in tools/gate_core.mjs, shared by the Worker and the Pages
// Function (copied into the staging dir; wrangler bundles the import). Secrets/KV come from env, never from files here.
export const WEBHOOK_KINDS = ["success", "failure", "monthlysuccess", "monthlyfailure"];
export const KV_PLACEHOLDER = "REPLACE_WITH_GATE_KV_NAMESPACE_ID";
export function gateRoutes(basePath = "/") {
  const out = [];
  for (const b of [...new Set([normBasePath(basePath), "/collabnet/", "/"])]) {
    for (const k of WEBHOOK_KINDS) out.push(`${b}${k}`, `${b}${k}/`);
    out.push(`${b}api/gate/*`);
  }
  return out;
}

export function workerSource(basePath = "/") {
  const b = normBasePath(basePath);
  return `// Generated by tools/cf_deploy.mjs (E3). BMC webhooks (${WEBHOOK_KINDS.map((k) => b + k).join(", ")}) and the
// verified-unlock API (${b}api/gate/*) are handled by gate_core.mjs; everything else is a static asset.
import { handle } from "./gate_core.mjs";
const BASE = ${JSON.stringify(b)};
export default {
  async fetch(request, env) {
    return (await handle(request, env, { basePath: BASE })) || env.ASSETS.fetch(request);
  },
};
`;
}

export function functionSource(basePath = "/") {
  const b = normBasePath(basePath);
  return `// Generated by tools/cf_deploy.mjs (E3): Pages Function (catch-all, limited by _routes.json to the BMC webhooks and
// ${b}api/gate/*). Same logic as the Worker (gate_core.mjs); anything else falls through to the static assets.
import { handle } from "../lib/gate_core.mjs";
export async function onRequest(context) {
  return (await handle(context.request, context.env, { basePath: ${JSON.stringify(b)} })) || context.next();
}
`;
}

export function wranglerConfig({ projectName = "collabnet", basePath = "/", kvId = process.env.GATE_KV_ID || KV_PLACEHOLDER } = {}) {
  const b = normBasePath(basePath);
  return {
    $schema: "./node_modules/wrangler/config-schema.json",
    name: projectName,
    compatibility_date: COMPAT_DATE,
    main: "worker.js",
    assets: {
      directory: "./public",
      binding: "ASSETS",
      html_handling: "auto-trailing-slash",
      // Not "single-page-application": Shinylive routes app_* inside its service worker, not on the server, and an
      // index.html fallback would turn a missing webR package or asset into a confusing 200 HTML response.
      not_found_handling: "none",
      run_worker_first: gateRoutes(b),
    },
    // Create once: npx wrangler kv namespace create GATE_KV  -> put the id here via env GATE_KV_ID (not a secret).
    kv_namespaces: [{ binding: "GATE_KV", id: kvId }],
  };
}

// Copy src into <out>/<target>/public<basePath> and write the generated files. Returns the staging paths.
export function stage(a) {
  const root = resolve(a.out, a.target), pub = join(root, "public"), dest = join(pub, ...a.basePath.split("/").filter(Boolean));
  rmSync(root, { recursive: true, force: true });
  mkdirSync(dest, { recursive: true });
  cpSync(a.src, dest, { recursive: true });
  writeFileSync(join(pub, "_headers"), headersFile(a.basePath));
  // local dev only: a git-ignored .dev.vars (fake/dev secrets) is copied for wrangler dev; never for deploy
  const devVars = process.env.COLLABNET_DEV_VARS || resolve(HERE, "..", ".dev.vars");
  if (a.serve && existsSync(devVars)) cpSync(devVars, join(root, ".dev.vars"));
  if (a.target === "workers") {
    cpSync(join(HERE, "gate_core.mjs"), join(root, "gate_core.mjs"));
    writeFileSync(join(root, "worker.js"), workerSource(a.basePath));
    writeFileSync(join(root, "wrangler.jsonc"), "// Generated by tools/cf_deploy.mjs (E2). Do not put credentials here.\n" +
      JSON.stringify(wranglerConfig(a), null, 2).replace('"./node_modules/', `"${resolve(HERE, "..", "node_modules")}/`) + "\n");
  } else {
    mkdirSync(join(root, "functions"), { recursive: true }); mkdirSync(join(root, "lib"), { recursive: true });
    cpSync(join(HERE, "gate_core.mjs"), join(root, "lib", "gate_core.mjs"));
    writeFileSync(join(root, "functions", "[[path]].js"), functionSource(a.basePath));
    writeFileSync(join(pub, "_routes.json"), JSON.stringify({ version: 1, include: gateRoutes(a.basePath), exclude: [] }, null, 2) + "\n");
  }
  return { root, pub };
}

export function wranglerArgs(a) {
  const pin = `wrangler@${WRANGLER_VERSION}`;
  if (a.target === "workers") {
    if (a.serve) return ["--yes", pin, "dev", "--config", "wrangler.jsonc", "--ip", "127.0.0.1", "--port", String(a.port)];
    return ["--yes", pin, "deploy", "--config", "wrangler.jsonc", ...(a.dryRun ? ["--dry-run", "--outdir", "dist"] : [])];
  }
  if (a.serve) return ["--yes", pin, "pages", "dev", "public", "--kv", "GATE_KV", "--ip", "127.0.0.1", "--port", String(a.port)];
  return ["--yes", pin, "pages", "deploy", "public", `--project-name=${a.projectName}`];
}

// E3: secret NAMES only (values are never read or printed). Returns the names missing from `wrangler secret list`.
export function missingSecrets(listJson, names = REQUIRED_SECRETS) {
  let have = [];
  try { have = JSON.parse(listJson).map((s) => s.name); } catch { return null; }
  return names.filter((n) => !have.includes(n));
}
export function secretChecklist(a) {
  const pages = a.target === "pages" ? ` --project-name=${a.projectName}` : "";
  return [...REQUIRED_SECRETS.map((n) => `  npx wrangler${pages ? " pages" : ""} secret put ${n}${pages}`),
    ...OPTIONAL_SECRETS.map((n) => `  (optional) npx wrangler${pages ? " pages" : ""} secret put ${n}${pages}`)].join("\n");
}

export function credentialsMissing(env = process.env) {
  return ["CLOUDFLARE_API_TOKEN", "CLOUDFLARE_ACCOUNT_ID"].filter((k) => !env[k]);
}

export function main(argv) {
  let a;
  try { a = parseArgs(argv); } catch (e) { console.error("cf_deploy:", e.message); return 2; }
  if (Number(process.versions.node.split(".")[0]) < 18) { console.error(`cf_deploy: Node >= 18 required (found ${process.version})`); return 2; }
  if (!existsSync(a.src)) { console.error(`cf_deploy: build dir not found: ${a.src} (run the Shinylive export first)`); return 2; }
  const { root, pub } = stage(a);
  const pf = preflight(pub, { paidPlan: a.paidPlan });
  console.log(`cf_deploy: staged ${a.target} at ${root} (${pf.fileCount} files, limit ${pf.maxFiles}; base path ${a.basePath})`);
  if (!pf.ok) { console.error("cf_deploy: PREFLIGHT FAILED (Cloudflare static asset limits):\n  - " + pf.errors.join("\n  - ")); return 3; }
  const args = wranglerArgs(a);
  if (a.target === "workers" && !a.serve && (process.env.GATE_KV_ID || KV_PLACEHOLDER) === KV_PLACEHOLDER) {
    console.error("cf_deploy: GATE_KV namespace id not set. Create it once with:\n  npx wrangler kv namespace create GATE_KV\n" +
      "then re-run with GATE_KV_ID=<the id it prints> (the id is not a secret).");
    if (!a.dryRun) return 5;
  }
  if (a.target === "pages" && !a.serve) console.log("cf_deploy: Pages: bind KV namespace GATE_KV to the project (dashboard > Settings > Bindings) " +
    "after: npx wrangler kv namespace create GATE_KV");
  if (a.dryRun || a.serve) console.log("cf_deploy: secrets the gate Worker expects (names only; set each once):\n" + secretChecklist(a));
  if (a.dryRun && a.target === "pages") {   // wrangler pages deploy has no dry-run: show what would run
    console.log("cf_deploy: --dry-run: would run (in " + root + "): npx " + args.join(" ")); return 0;
  }
  if (!a.dryRun && !a.serve) {
    const miss = credentialsMissing();
    if (miss.length) { console.error(`cf_deploy: refusing to deploy, missing env ${miss.join(", ")} (use --dry-run or --serve without them)`); return 4; }
    const local0 = resolve(HERE, "..", "node_modules", ".bin", "wrangler");
    const listArgs = a.target === "pages" ? ["pages", "secret", "list", `--project-name=${a.projectName}`] : ["secret", "list", "--format", "json"];
    const l = spawnSync(existsSync(local0) ? local0 : "npx", existsSync(local0) ? listArgs : ["--yes", `wrangler@${WRANGLER_VERSION}`, ...listArgs],
      { cwd: root, encoding: "utf8", env: process.env });
    const missing = a.target === "workers" ? missingSecrets(l.stdout || "") : REQUIRED_SECRETS.filter((n) => !(l.stdout || "").includes(n));
    if (missing && missing.length) console.warn("cf_deploy: WARNING: secrets not set yet (verified unlock will answer 503/401 until they are):\n" +
      missing.map((n) => "  npx wrangler" + (a.target === "pages" ? " pages" : "") + " secret put " + n).join("\n"));
  }
  if (spawnSync("npx", ["--version"], { stdio: "ignore" }).status !== 0) { console.error("cf_deploy: npx not found; install Node.js >= 18 (includes npm/npx)"); return 2; }
  // prefer the pinned wrangler from package-lock (npm ci) to avoid a second download by npx
  const local = resolve(HERE, "..", "node_modules", ".bin", "wrangler");
  const [cmd, cargs] = existsSync(local) ? [local, args.slice(2)] : ["npx", args];
  console.log("cf_deploy: running (in " + root + "): " + cmd + " " + cargs.join(" "));
  const r = spawnSync(cmd, cargs, { cwd: root, stdio: "inherit", env: process.env });
  return r.status ?? 1;
}
if (process.argv[1] === fileURLToPath(import.meta.url)) process.exitCode = main(process.argv.slice(2));
