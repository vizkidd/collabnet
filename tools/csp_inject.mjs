#!/usr/bin/env node
// E2b: Node port of csp_inject.py (identical output; see that file's docstring for scope and honesty notes).
// Usage: node tools/csp_inject.mjs wasm_build/index.html [--extra-connect https://my.llm.host ...]
// Idempotent. Also reads COLLABNET_CSP_EXTRA_CONNECT (space-separated). Copies www/js/collabnet-session-guard.js
// next to index.html. Exit 1 if the file cannot be read or written.
import { createHash } from "node:crypto";
import { readFileSync, writeFileSync, existsSync, copyFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

export const API_HOSTS = [
  "https://api.elsevier.com", "https://api.crossref.org", "https://api.openalex.org",
  "https://api.semanticscholar.org", "https://api.opencitations.net", "https://pub.orcid.org",
  "https://doi.org", "https://www.ncbi.nlm.nih.gov",
  "https://repo.r-wasm.org",
  "https://api.openai.com",
  "https://api.anthropic.com", "https://generativelanguage.googleapis.com", "https://openrouter.ai",
];
// E3: Buy Me a Coffee button (gate dialog, rendered in a srcdoc iframe that inherits this policy) + its detect script.
export const BMC_SCRIPT = ["https://cdnjs.buymeacoffee.com", "'sha256-eMWsPfmv41Z85fYg+d/gPBrG/kS8UkAha6JImKwQrrA='"];
export const BMC_STYLE = ["https://fonts.googleapis.com"];
export const BMC_FONT = ["https://fonts.gstatic.com"];
export const LOCAL_BRIDGE = ["ws://127.0.0.1:*", "ws://localhost:*", "http://127.0.0.1:*", "http://localhost:*"];
export const MARK = 'data-collabnet-csp="1"';
export const GUARD_FILE = "collabnet-session-guard.js";
const GUARD_TAG = `<script src="${GUARD_FILE}" data-collabnet-guard="1"></script>`;
const ROOT = join(dirname(fileURLToPath(import.meta.url)), "..");

// Python str.strip() whitespace (ASCII + Unicode spaces); JS trim() is close enough for script bodies.
export function inlineScriptHashes(html) {
  const out = [];
  for (const m of html.matchAll(/<script(?![^>]*\bsrc=)[^>]*>([\s\S]*?)<\/script>/gi)) {
    if (m[1].trim()) out.push(`'sha256-${createHash("sha256").update(m[1], "utf8").digest("base64")}'`);
  }
  return out;
}

export function buildPolicy(html, extraConnect = []) {
  const script = ["'self'", "'wasm-unsafe-eval'", ...inlineScriptHashes(html), ...BMC_SCRIPT];
  const connect = ["'self'", "blob:", "data:", ...API_HOSTS, ...LOCAL_BRIDGE, ...extraConnect];
  return [
    "default-src 'self'",
    "script-src " + script.join(" "),
    "worker-src 'self' blob:",
    "child-src 'self' blob:",
    "frame-src 'self' blob:",
    "connect-src " + connect.join(" "),
    "img-src 'self' data: blob: https:",
    "style-src 'self' 'unsafe-inline' " + BMC_STYLE.join(" "),
    "font-src 'self' data: " + BMC_FONT.join(" "),
    "object-src 'none'",
    "base-uri 'self'",
    "form-action 'self'",
  ].join("; ");
}

export function inject(html, extraConnect = []) {
  html = html.replace(/\s*<meta[^>]*data-collabnet-csp="1"[^>]*>/g, "");
  html = html.replace(/\s*<script[^>]*data-collabnet-guard="1"[^>]*><\/script>/g, "");
  const meta = `<meta http-equiv="Content-Security-Policy" ${MARK} content="${buildPolicy(html, extraConnect)}">`;
  if (/<head[^>]*>/i.test(html)) return html.replace(/(<head[^>]*>)/i, (h) => h + "\n    " + meta + "\n    " + GUARD_TAG);
  return meta + "\n" + GUARD_TAG + "\n" + html;
}

export function main(argv, env = process.env) {
  if (!argv.length) { console.log("Usage: node tools/csp_inject.mjs wasm_build/index.html [--extra-connect URL ...]"); return 2; }
  const path = argv[0];
  const extra = argv.includes("--extra-connect") ? argv.slice(argv.indexOf("--extra-connect") + 1) : [];
  extra.push(...(env.COLLABNET_CSP_EXTRA_CONNECT || "").split(/\s+/).filter(Boolean));
  let html;
  try { html = readFileSync(path, "utf8").replace(/\r\n?/g, "\n"); }   // Python text-mode newline handling
  catch (e) { console.error("csp_inject:", e.message); return 1; }
  writeFileSync(path, inject(html, extra), "utf8");
  const src = join(ROOT, "www", "js", GUARD_FILE);
  if (existsSync(src)) copyFileSync(src, join(dirname(resolve(path)), GUARD_FILE));
  console.log("CSP injected into", path); return 0;
}
if (process.argv[1] === fileURLToPath(import.meta.url)) process.exitCode = main(process.argv.slice(2));
