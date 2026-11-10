#!/usr/bin/env node
// E3: fail if anything resembling a webhook signing secret (a run of >= 64 hex chars, e.g. BMC's 80-hex secrets) or a
// .dev.vars file is tracked/staged. Usage: node tools/secret_scan.mjs [--staged]   (exit 1 + file:line on a hit; the
// matched value itself is never printed). Install as a pre-commit hook: tools/pre-commit (see its header).
import { execFileSync } from "node:child_process";
import { readFileSync } from "node:fs";
export const HEX_RE = /(?<![0-9a-fA-F])[0-9a-fA-F]{64,}(?![0-9a-fA-F])/;
// Known non-secret long hex strings may be allowed per line with this marker (e.g. a published file checksum).
export const ALLOW = "secret-scan: allow";
export function scanText(text) {
  const hits = [];
  text.split("\n").forEach((l, i) => { if (HEX_RE.test(l) && !l.includes(ALLOW)) hits.push(i + 1); });
  return hits;
}
export function main(argv) {
  const staged = argv.includes("--staged");
  const files = execFileSync("git", staged ? ["diff", "--cached", "--name-only", "--diff-filter=ACM"] : ["ls-files"], { encoding: "utf8" }).split("\n").filter(Boolean);
  let bad = 0;
  for (const f of files) {
    if (/(^|\/)\.dev\.vars$/.test(f)) { console.error(`secret_scan: ${f}: .dev.vars must never be committed`); bad++; continue; }
    let t; try { t = staged ? execFileSync("git", ["show", ":" + f], { encoding: "utf8", maxBuffer: 64 << 20 }) : readFileSync(f, "utf8"); } catch { continue; }
    if (t.includes("\u0000")) continue;   // binary
    for (const ln of scanText(t)) { console.error(`secret_scan: ${f}:${ln}: looks like a secret (>= 64 hex chars); value not shown`); bad++; }
  }
  if (bad) console.error(`secret_scan: ${bad} problem(s). Secrets belong in Cloudflare (npx wrangler secret put ...) or a git-ignored .dev.vars.`);
  else console.log(`secret_scan: OK (${files.length} files)`);
  return bad ? 1 : 0;
}
if (process.argv[1] && import.meta.url.endsWith(process.argv[1].split("/").pop())) process.exitCode = main(process.argv.slice(2));
