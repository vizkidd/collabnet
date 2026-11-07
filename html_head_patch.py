#!/usr/bin/env python3
"""D3-6/7: set the browser-tab title and favicon of the exported Shinylive shell (wasm_build/index.html).

Why: under Shinylive the app runs inside an iframe; the browser tab shows the OUTER index.html, whose <title> comes from
shinylive's export template ({{#title}} is optional, so it is often missing or a default) and which has no favicon.
fluidPage(title = "CollabNET") only titles the inner iframe document.

Usage: python3 html_head_patch.py wasm_build/index.html [--title CollabNET] [--icons-from www]
- replaces any existing <title>...</title> (or inserts one) with the given title (HTML-escaped);
- inserts <link rel="icon"> / apple-touch-icon tags once (marked data-collabnet-icon="1"), idempotent;
- copies favicon.ico, favicon-32.png and img/apple-touch-icon.png next to index.html (from --icons-from).
Exit 1 if index.html has no <head>.
"""
import html, os, re, shutil, sys

MARK = 'data-collabnet-icon="1"'
ICONS = [("favicon.ico", "favicon.ico"), ("favicon-32.png", "favicon-32.png"), ("img/apple-touch-icon.png", "apple-touch-icon.png")]
LINKS = ('<link rel="icon" type="image/png" sizes="32x32" href="favicon-32.png" %s>\n'
         '<link rel="icon" href="favicon.ico" sizes="any" %s>\n'
         '<link rel="apple-touch-icon" href="apple-touch-icon.png" %s>\n') % (MARK, MARK, MARK)


def patch_html(text, title="CollabNET"):
    if not re.search(r"<head[^>]*>", text, re.I):
        raise ValueError("no <head> in index.html")
    t = "<title>%s</title>" % html.escape(title)
    if re.search(r"<title[^>]*>.*?</title>", text, re.I | re.S):
        text = re.sub(r"<title[^>]*>.*?</title>", t, text, count=1, flags=re.I | re.S)
    else:
        text = re.sub(r"(<head[^>]*>)", r"\1\n" + t, text, count=1, flags=re.I)
    if MARK not in text:
        text = re.sub(r"(</head>)", LINKS + r"\1", text, count=1, flags=re.I)
    return text


def main(argv):
    if not argv:
        print(__doc__); return 2
    path = argv[0]; title = "CollabNET"; icons_from = "www"
    if "--title" in argv: title = argv[argv.index("--title") + 1]
    if "--icons-from" in argv: icons_from = argv[argv.index("--icons-from") + 1]
    with open(path, encoding="utf-8") as f:
        text = f.read()
    try:
        new = patch_html(text, title)
    except ValueError as e:
        print("html_head_patch: %s (%s)" % (e, path), file=sys.stderr); return 1
    with open(path, "w", encoding="utf-8") as f:
        f.write(new)
    dest = os.path.dirname(os.path.abspath(path))
    for src, name in ICONS:
        s = os.path.join(icons_from, src)
        if os.path.exists(s): shutil.copyfile(s, os.path.join(dest, name))
        else: print("html_head_patch: missing icon %s" % s, file=sys.stderr)
    print("html_head_patch: title=%r, favicon links %s" % (title, "present"))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
