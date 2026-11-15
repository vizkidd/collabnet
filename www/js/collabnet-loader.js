// P10-B: CollabNET loading screen for the Cloudflare editions (free + full; demo keeps Shinylive's default loader).
// Shinylive 0.9.1 has no loader hook (its spinner is an internal React component) and webR downloads R and the
// packages inside a Web Worker, which a page-level fetch wrapper cannot see. So the stages below are driven only by
// signals the page CAN observe honestly; byte counts are the build-time sizes (data-* on this script tag), not live.
//   1 Downloading R      -- from page start; "loaded from cache" if the Shinylive cache already holds R.bin.wasm
//   2 Starting R         -- the Shinylive app frame exists (webR is booting in its worker)
//   3 Installing packages-- the frame's document is loading the Shiny app (package installs happen here)
//   4 Starting CollabNET -- Shiny reports connected (shiny:connected / Shiny.shinyapp.isConnected()); then fade out
// A stage that does not advance for 60 s offers a reload. CSP-safe: external file, classes only, no inline code.
(function () {
  "use strict";
  var me = document.currentScript, d = (me && me.dataset) || {};
  var MB = function (b) { return Math.round(Number(b || 0) / 1048576); };
  var t0 = Date.now(), stage = 0, stageAt = Date.now(), done = false, el = {}, cached = false;
  var STAGES = ["Downloading R", "Starting R", "Installing packages", "Starting CollabNET"];
  function txt(tag, cls, s) { var e = document.createElement(tag); if (cls) e.className = cls; if (s) e.textContent = s; return e; }
  function build() {
    var box = txt("div", "cn-ld"); box.id = "cn-loader"; box.setAttribute("role", "status"); box.setAttribute("aria-live", "polite");
    var card = txt("div", "cn-ld-card"); box.appendChild(card);
    card.appendChild(txt("h1", "cn-ld-title", "Starting CollabNET"));
    var ol = txt("ol", "cn-ld-stages"); el.items = STAGES.map(function (s) { var li = txt("li", "", s); ol.appendChild(li); return li; });
    card.appendChild(ol);
    var bar = txt("div", "cn-ld-bar"); el.fill = txt("div", "cn-ld-fill"); bar.appendChild(el.fill); card.appendChild(bar);
    el.detail = txt("p", "cn-ld-detail"); card.appendChild(el.detail);
    el.time = txt("p", "cn-ld-time"); card.appendChild(el.time);
    el.stall = txt("p", "cn-ld-stall"); el.stall.hidden = true;
    el.stall.appendChild(document.createTextNode("This is taking longer than usual. "));
    var rb = txt("button", "cn-ld-reload", "Reload"); rb.type = "button"; rb.addEventListener("click", function () { location.reload(); });
    el.stall.appendChild(rb); card.appendChild(el.stall);
    var about = txt("div", "cn-ld-about");
    about.appendChild(txt("h2", "", "While you wait"));
    about.appendChild(txt("p", "", "CollabNET builds co-authorship networks from ORCID, Scopus, Semantic Scholar, OpenAlex and Crossref records, entirely in your browser: your data goes from your browser to the providers and nowhere else."));
    about.appendChild(txt("p", "", "How to read the network: each node is an author, each edge a shared publication (weighted by 1/(n-1) for papers with n authors). Larger nodes are more central; colours mark communities; ringed nodes are articulation points whose removal splits the network."));
    card.appendChild(about);
    document.body.appendChild(box); el.box = box;
    render();
  }
  function render() {
    if (!el.box) return;
    el.items.forEach(function (li, i) { li.className = i < stage ? "done" : i === stage ? "active" : ""; if (i === stage) li.setAttribute("aria-current", "step"); else li.removeAttribute("aria-current"); });
    el.fill.style.width = Math.round(((stage + 0.5) / STAGES.length) * 100) + "%";
    var tot = MB(Number(d.runtimeBytes || 0) + Number(d.packagesBytes || 0) + Number(d.appBytes || 0));
    el.detail.textContent = stage === 0
      ? (cached ? "Loaded from your browser cache." : "First visit: up to " + tot + " MB (R runtime " + MB(d.runtimeBytes) + " MB, " + (d.packages || "?") + " R packages " + MB(d.packagesBytes) + " MB, app " + MB(d.appBytes) + " MB). Later visits use the cache.")
      : stage === 1 ? "R (webR) is starting in your browser."
      : stage === 2 ? "Loading the R packages the app needs (" + (d.packages || "?") + " available in this build)."
      : "Starting the CollabNET app.";
  }
  function go(n) { if (n > stage && !done) { stage = n; stageAt = Date.now(); el.stall && (el.stall.hidden = true); render(); } }
  function finish() { if (done) return; done = true; stage = STAGES.length; render();
    if (el.box) { el.box.classList.add("cn-ld-out"); setTimeout(function () { el.box && el.box.remove(); }, 400); } }
  function frameDoc() { var f = document.querySelector("#root iframe"); try { return f && f.contentWindow && f.contentDocument ? f : null; } catch (e) { return null; } }
  function tick() {
    if (done) return;
    var s = Math.round((Date.now() - t0) / 1000);
    if (el.time) el.time.textContent = "Elapsed: " + s + " s";
    var f = frameDoc();
    if (f) { go(1); var w = f.contentWindow;
      if (w.document && w.document.readyState !== "uninitialized" && w.document.body) go(2);
      if (w.Shiny && w.Shiny.shinyapp) { go(3); if (typeof w.Shiny.shinyapp.isConnected === "function" && w.Shiny.shinyapp.isConnected()) setTimeout(finish, 300); }
      if (w.jQuery && !f.__cnHooked) { f.__cnHooked = true; w.jQuery(w.document).on("shiny:connected", function () { go(3); setTimeout(finish, 300); }); }
    }
    if (el.stall && Date.now() - stageAt > 60000) el.stall.hidden = false;
    setTimeout(tick, 250);
  }
  function checkCache() {
    if (!("caches" in window)) return;
    var url = new URL("shinylive/webr/R.bin.wasm", location.href).href;
    caches.match(url).then(function (r) { if (r) { cached = true; render(); } }).catch(function () {});
  }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", function () { build(); checkCache(); tick(); });
  else { build(); checkCache(); tick(); }
  setTimeout(function () { if (!done && el.stall) el.stall.hidden = false; }, 180000);
})();
