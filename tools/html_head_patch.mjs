#!/usr/bin/env node
// E2: Node port of html_head_patch.py (identical output; see that file's docstring for the why).
// Usage: node tools/html_head_patch.mjs wasm_build/index.html [--title CollabNET] [--icons-from www]
import { readFileSync, writeFileSync, existsSync, copyFileSync } from "node:fs";
import { dirname, resolve, join } from "node:path";
import { fileURLToPath } from "node:url";

export const MARK = 'data-collabnet-icon="1"';
const ICONS = [["favicon.ico", "favicon.ico"], ["favicon-32.png", "favicon-32.png"], ["img/apple-touch-icon.png", "apple-touch-icon.png"]];
const LINKS = `<link rel="icon" type="image/png" sizes="32x32" href="favicon-32.png" ${MARK}>\n` +
  `<link rel="icon" href="favicon.ico" sizes="any" ${MARK}>\n` +
  `<link rel="apple-touch-icon" href="apple-touch-icon.png" ${MARK}>\n`;
// Python html.escape(quote=True)
export const escapeHtml = (s) => s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;").replace(/'/g, "&#x27;");

export function patchHtml(text, title = "CollabNET") {
  if (!/<head[^>]*>/i.test(text)) throw new Error("no <head> in index.html");
  const t = `<title>${escapeHtml(title)}</title>`;
  if (/<title[^>]*>[\s\S]*?<\/title>/i.test(text)) text = text.replace(/<title[^>]*>[\s\S]*?<\/title>/i, () => t);
  else text = text.replace(/(<head[^>]*>)/i, (h) => h + "\n" + t);
  if (!text.includes(MARK)) text = text.replace(/(<\/head>)/i, (h) => LINKS + h);
  return text;
}

export function main(argv) {
  if (!argv.length) { console.log("Usage: node tools/html_head_patch.mjs wasm_build/index.html [--title T] [--icons-from www]"); return 2; }
  const path = argv[0];
  const opt = (k, d) => (argv.includes(k) ? argv[argv.indexOf(k) + 1] : d);
  const title = opt("--title", "CollabNET"), iconsFrom = opt("--icons-from", "www");
  const text = readFileSync(path, "utf8").replace(/\r\n?/g, "\n");   // = Python text-mode newline handling
  let out;
  try { out = patchHtml(text, title); } catch (e) { console.error(`html_head_patch: ${e.message} (${path})`); return 1; }
  writeFileSync(path, out, "utf8");
  const dest = dirname(resolve(path));
  for (const [src, name] of ICONS) {
    const s = join(iconsFrom, src);
    if (existsSync(s)) copyFileSync(s, join(dest, name)); else console.error(`html_head_patch: missing icon ${s}`);
  }
  console.log(`html_head_patch: title='${title}', favicon links present`); return 0;
}
if (process.argv[1] === fileURLToPath(import.meta.url)) process.exitCode = main(process.argv.slice(2));
