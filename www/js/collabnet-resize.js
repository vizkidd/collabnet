/*
 * collabnet-resize.js -- D2-B1: make visNetwork / plotly / other htmlwidgets re-fit when their container changes size
 * (tab shown, collapse toggled, modal shown, window or orientation change, sidebar stacking).
 * Idea from bslib's ShinyResizeObserver (bslib srcts/src/components/_shinyResizeObserver.ts, MIT): observe widget
 * containers and dispatch a single window 'resize' (which htmlwidgets/plotly/visNetwork already listen to); our own
 * implementation for Bootstrap 3, debounced and CSP-safe (external file, no eval).
 */
(function (root) {
  "use strict";
  var DEBOUNCE_MS = 120;

  // Pure: debounce that coalesces bursts into one call (unit-tested).
  function debounce(fn, ms, timers) {
    var t = null, st = (timers && timers.setTimeout) || setTimeout, ct = (timers && timers.clearTimeout) || clearTimeout;
    return function () { if (t) ct(t); t = st(function () { t = null; fn(); }, ms); };
  }

  function attach(win) {
    var doc = win.document;
    var fire = debounce(function () {
      try { win.dispatchEvent(new win.Event("resize")); } catch (e) { /* very old browsers */ }
    }, DEBOUNCE_MS);
    var lastW = new WeakMap();
    var ro = win.ResizeObserver ? new win.ResizeObserver(function (entries) {
      var changed = false;
      for (var i = 0; i < entries.length; i++) {
        var w = Math.round(entries[i].contentRect.width);
        if (lastW.get(entries[i].target) !== w) { lastW.set(entries[i].target, w); changed = true; }
      }
      if (changed) fire();
    }) : null;
    var observed = new WeakSet();
    function scan() {
      if (!ro) return;
      var els = doc.querySelectorAll(".html-widget, .shiny-plot-output");
      for (var i = 0; i < els.length; i++) if (!observed.has(els[i])) { observed.add(els[i]); ro.observe(els[i]); }
    }
    scan();
    if (win.MutationObserver) new win.MutationObserver(debounce(scan, 250)).observe(doc.body, { childList: true, subtree: true });
    win.addEventListener("orientationchange", fire);
    var $ = win.jQuery;
    if ($) $(doc).on("shown.bs.tab shown.bs.collapse hidden.bs.collapse shown.bs.modal", fire);
    return { fire: fire, scan: scan };
  }

  var api = { debounce: debounce, attach: attach, DEBOUNCE_MS: DEBOUNCE_MS };
  if (typeof module !== "undefined" && module.exports) module.exports = api;
  else {
    var go = function () { root.CollabNETResize = attach(root); };
    if (root.document.readyState === "loading") root.document.addEventListener("DOMContentLoaded", go); else go();
  }
})(typeof window !== "undefined" ? window : this);
