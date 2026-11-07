#!/usr/bin/env python3
"""D1-7: patch the exported Shinylive service worker (wasm_build/shinylive-sw.js).

Usage:  python3 sw_patch.py wasm_build/shinylive-sw.js      (idempotent; exit 1 if the fetch handler is not found)

Problem (shinylive assets 0.9.1, pinned via R shinylive 0.3.0; unchanged through assets v0.10.16): the fetch
handler re-creates app_* requests with `new Request(url, {cache: request.cache, ...})` and no `mode`, so the
new Request gets mode "cors". When Chrome issues a request with cache "only-if-cached" (DevTools does this
when it re-fetches a document to display its response), the constructor throws
  TypeError: Failed to construct 'Request': 'only-if-cached' can be set only with 'same-origin' mode
inside event.respondWith(), the promise rejects, and Chrome also logs
  The FetchEvent for ".../app_<id>/" resulted in a network error response: the promise was rejected.
Fix (D2-A1, replaces the D1-7 guard): return early -- without calling respondWith(), so the browser performs
its default handling -- for EVERY request whose cache mode is "only-if-cached". The D1-7 guard additionally required
mode !== "same-origin", but the Fetch spec only lets only-if-cached exist together with mode "same-origin" (the
Request constructor throws otherwise), so that guard never fired for the requests that actually reach the handler,
and the rebuilt `new Request(url.toString(), {cache: request.cache})` (string URL => mode "cors") still threw.
Why "return" and not respondWith(fetch(event.request)): not calling respondWith is the long-standing service-worker
idiom for this case; it cannot reject, and the page itself never issues only-if-cached requests (DevTools does), so
app traffic is unchanged. The anchor is the code pattern of the fetch listener, not a line number; an older D1-7 guard
is upgraded in place. If the pattern is not found, main() exits 1 and buildWASM.R stops the build.
"""
import re, sys

MARK = "/* collabnet-sw-patch D2-A1 */"
OLD_GUARD = re.compile(r"\n?[ \t]*/\* collabnet-sw-patch D1-7 \*/[^\n]*")
HANDLER = re.compile(r"""addEventListener\(\s*["']fetch["']\s*,\s*(?:async\s*)?(?:function\s*\(\s*(\w+)\s*\)|\(?\s*(\w+)\s*\)?\s*=>)\s*\{""")

def patch(js):
    if MARK in js: return js, True
    js = OLD_GUARD.sub("", js)   # upgrade a D1-7-patched file
    m = HANDLER.search(js)
    if not m: return js, False
    ev = m.group(1) or m.group(2)
    guard = '\n  %s if (%s.request.cache === "only-if-cached") return;' % (MARK, ev)
    return js[:m.end()] + guard + js[m.end():], True

def main(argv):
    if not argv: print(__doc__); return 2
    with open(argv[0], encoding="utf-8") as f: js = f.read()
    out, ok = patch(js)
    if not ok:
        print("sw_patch: fetch handler not found in", argv[0], "- NOT patched", file=sys.stderr); return 1
    with open(argv[0], "w", encoding="utf-8") as f: f.write(out)
    print("sw_patch: patched", argv[0]); return 0

if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
