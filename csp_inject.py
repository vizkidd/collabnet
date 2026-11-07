#!/usr/bin/env python3
"""P3-6: inject a Content-Security-Policy <meta> into the exported Shinylive shell (wasm_build/index.html).

Usage:  python3 csp_inject.py wasm_build/index.html [--extra-connect https://my.llm.host ...]
Idempotent: an existing CollabNET CSP meta is replaced. Inline <script> blocks in the shell are allowed by
sha256 hash (computed here, so the policy follows Shinylive template changes) instead of 'unsafe-inline'.

Scope / honesty notes (UNTESTED IN A BROWSER):
- The meta applies to the outer shell document only. The Shiny UI itself is rendered by Shinylive inside an
  iframe served by its service worker; that document (with CollabNET's inline scripts from ui.R) is NOT
  covered by this meta. Workers created from blob: URLs inherit this policy.
- connect-src lists the APIs CollabNET's R/JS code actually calls (grep of *.R / www/js), the webR package
  repository, and the local MCP bridge (ws://127.0.0.1:* / ws://localhost:*). The in-page AI chat lets the
  user configure any OpenAI-compatible endpoint; only https://api.openai.com is allowed by default. Add other
  providers with --extra-connect (or COLLABNET_CSP_EXTRA_CONNECT, space-separated).
"""
import base64, hashlib, os, re, shutil, sys

API_HOSTS = [
    "https://api.elsevier.com", "https://api.crossref.org", "https://api.openalex.org",
    "https://api.semanticscholar.org", "https://api.opencitations.net", "https://pub.orcid.org",
    "https://doi.org", "https://www.ncbi.nlm.nih.gov",          # bibliographic APIs used by R/JS code
    "https://repo.r-wasm.org",                                   # webR package repository (fallback)
    "https://api.openai.com",                                    # default endpoint of the in-page AI chat
    "https://api.anthropic.com", "https://generativelanguage.googleapis.com", "https://openrouter.ai",  # D2-B3 providers
]
LOCAL_BRIDGE = ["ws://127.0.0.1:*", "ws://localhost:*", "http://127.0.0.1:*", "http://localhost:*"]
MARK = 'data-collabnet-csp="1"'
# D1-1: external (CSP 'self') script that shows "The R session has ended. Reload to restart." when webR exits
# (unhandledrejection with an ExitStatus reason). Copied next to index.html from www/js by main().
GUARD_FILE = "collabnet-session-guard.js"
GUARD_TAG = '<script src="%s" data-collabnet-guard="1"></script>' % GUARD_FILE

def inline_script_hashes(html):
    out = []
    for m in re.finditer(r"<script(?![^>]*\bsrc=)[^>]*>(.*?)</script>", html, flags=re.S | re.I):
        body = m.group(1)
        if body.strip():
            out.append("'sha256-%s'" % base64.b64encode(hashlib.sha256(body.encode("utf-8")).digest()).decode())
    return out

def build_policy(html, extra_connect=()):
    script = ["'self'", "'wasm-unsafe-eval'"] + inline_script_hashes(html)
    connect = ["'self'", "blob:", "data:"] + API_HOSTS + LOCAL_BRIDGE + list(extra_connect)
    return "; ".join([
        "default-src 'self'",
        "script-src " + " ".join(script),
        "worker-src 'self' blob:",
        "child-src 'self' blob:",
        "frame-src 'self' blob:",
        "connect-src " + " ".join(connect),
        "img-src 'self' data: blob: https:",
        "style-src 'self' 'unsafe-inline'",
        "font-src 'self' data:",
        "object-src 'none'",
        "base-uri 'self'",
        "form-action 'self'",
    ])

def inject(html, extra_connect=()):
    html = re.sub(r'\s*<meta[^>]*%s[^>]*>' % re.escape(MARK), "", html)
    html = re.sub(r'\s*<script[^>]*data-collabnet-guard="1"[^>]*></script>', "", html)
    meta = '<meta http-equiv="Content-Security-Policy" %s content="%s">' % (MARK, build_policy(html, extra_connect))
    if re.search(r"<head[^>]*>", html, flags=re.I):
        return re.sub(r"(<head[^>]*>)", lambda m: m.group(1) + "\n    " + meta + "\n    " + GUARD_TAG,
                      html, count=1, flags=re.I)
    return meta + "\n" + GUARD_TAG + "\n" + html

def main(argv):
    if not argv: print(__doc__); return 2
    path, extra = argv[0], []
    if "--extra-connect" in argv: extra = argv[argv.index("--extra-connect") + 1:]
    extra += os.environ.get("COLLABNET_CSP_EXTRA_CONNECT", "").split()
    with open(path, encoding="utf-8") as f: html = f.read()
    with open(path, "w", encoding="utf-8") as f: f.write(inject(html, extra))
    src = os.path.join(os.path.dirname(os.path.abspath(__file__)), "www", "js", GUARD_FILE)
    if os.path.exists(src): shutil.copyfile(src, os.path.join(os.path.dirname(os.path.abspath(path)), GUARD_FILE))
    print("CSP injected into", path); return 0

if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
