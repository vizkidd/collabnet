/*
 * collabnet-layout.js -- D3-4: the Execution Log (#log_wrapper) must never cover content.
 * Below 992px collabnet-d3.css puts it in normal flow. At >= 992px it stays docked (position: fixed at the bottom of
 * the sidebar column, synced by ui.R); here we reserve exactly its height as padding at the end of the sidebar, so
 * the last sidebar card can always be scrolled clear of it (the old fixed 150px was smaller than a log with output).
 */
(function (root) {
  "use strict";

  // Pure (unit-tested): padding to reserve for a docked log of the given height.
  function reservedPadding(position, logHeight) {
    return position === "fixed" ? Math.ceil(Math.max(0, logHeight)) + 24 : null;
  }

  function attach(win) {
    var doc = win.document;
    var log = doc.getElementById("log_wrapper"), side = doc.querySelector(".left-sidebar-col");
    if (!log || !side) return null;
    var apply = function () {
      var pad = reservedPadding(win.getComputedStyle(log).position, log.getBoundingClientRect().height);
      side.style.paddingBottom = pad === null ? "" : pad + "px";
    };
    if (win.ResizeObserver) new win.ResizeObserver(apply).observe(log);
    win.addEventListener("resize", apply);
    apply();
    return { apply: apply };
  }

  var api = { reservedPadding: reservedPadding, attach: attach };
  if (typeof module !== "undefined" && module.exports) module.exports = api;
  else {
    var go = function () { root.CollabNETLayout = attach(root); };
    if (root.document.readyState === "loading") root.document.addEventListener("DOMContentLoaded", go); else go();
  }
})(typeof window !== "undefined" ? window : this);
