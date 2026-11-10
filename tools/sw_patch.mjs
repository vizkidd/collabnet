#!/usr/bin/env node
// E2: Node port of sw_patch.py (identical output; see that file's docstring for the why).
// Usage: node tools/sw_patch.mjs wasm_build/shinylive-sw.js   (idempotent; exit 1 if the fetch handler is not found)
import { readFileSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

export const MARK = "/* collabnet-sw-patch D2-A1 */";
const OLD_GUARD = /\n?[ \t]*\/\* collabnet-sw-patch D1-7 \*\/[^\n]*/g;
const HANDLER = /addEventListener\(\s*["']fetch["']\s*,\s*(?:async\s*)?(?:function\s*\(\s*(\w+)\s*\)|\(?\s*(\w+)\s*\)?\s*=>)\s*\{/;

// Python's text-mode read turns \r\n and \r into \n; do the same so the output is byte-identical.
export const readText = (p) => readFileSync(p, "utf8").replace(/\r\n?/g, "\n");

export function patch(js) {
  if (js.includes(MARK)) return [js, true];
  js = js.replace(OLD_GUARD, "");
  const m = HANDLER.exec(js);
  if (!m) return [js, false];
  const ev = m[1] || m[2], end = m.index + m[0].length;
  return [js.slice(0, end) + `\n  ${MARK} if (${ev}.request.cache === "only-if-cached") return;` + js.slice(end), true];
}

export function main(argv) {
  if (!argv.length) { console.log("Usage: node tools/sw_patch.mjs wasm_build/shinylive-sw.js"); return 2; }
  const [out, ok] = patch(readText(argv[0]));
  if (!ok) { console.error("sw_patch: fetch handler not found in", argv[0], "- NOT patched"); return 1; }
  writeFileSync(argv[0], out, "utf8");
  console.log("sw_patch: patched", argv[0]); return 0;
}
if (process.argv[1] === fileURLToPath(import.meta.url)) process.exitCode = main(process.argv.slice(2));
