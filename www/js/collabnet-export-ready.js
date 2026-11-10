/* E3e: empty-export pre-check. R sends "cn_export_ready" {<download id>: bool, message}; a click on an export whose
 * data is empty is stopped here (window capture phase = before the E1 gate's document listener and before the browser
 * follows the download link) and a message is shown instead, so no gate dialog opens and no file (such as an HTML
 * error page saved as download_btn.html) is downloaded. Also a DataTables backstop: Ajax errors are logged, not
 * alerted. */
(function () {
  "use strict";
  var state = {};
  var MSG = "No data to export — load or fetch data first, or loosen the current filters/lookup.";
  function ids() { return Object.keys(state).filter(function (k) { return k !== "message"; }); }
  function decorate() {
    ids().forEach(function (id) {
      var el = document.getElementById(id); if (!el) return;
      var empty = state[id] === false;
      el.classList.toggle("cn-export-empty", empty);
      if (empty && !el.hasAttribute("data-cn-title")) { el.setAttribute("data-cn-title", el.getAttribute("title") || ""); el.setAttribute("title", state.message || MSG); }
      if (!empty && el.hasAttribute("data-cn-title")) { el.setAttribute("title", el.getAttribute("data-cn-title")); el.removeAttribute("data-cn-title"); }
      if (empty) el.setAttribute("aria-disabled", "true"); else el.removeAttribute("aria-disabled");
    });
  }
  function notify(text) {
    if (window.Shiny && Shiny.notifications && Shiny.notifications.show) Shiny.notifications.show({ html: text, type: "warning", duration: 6000, id: "cn_export_empty" });
    else { var d = document.getElementById("cn-export-empty-msg"); if (!d) { d = document.createElement("div"); d.id = "cn-export-empty-msg"; d.setAttribute("role", "alert"); document.body.appendChild(d); } d.textContent = text; }
  }
  function onClick(e) {
    var t = e.target; if (!t || !t.closest) return;
    var el = null;
    ids().some(function (id) { var x = t.closest("#" + id); if (x) { el = x; return true; } return false; });
    if (!el || state[el.id] !== false) return;
    e.preventDefault(); e.stopImmediatePropagation(); e.stopPropagation();
    notify(state.message || MSG);
  }
  window.addEventListener("click", onClick, true);
  function bind() {
    Shiny.addCustomMessageHandler("cn_export_ready", function (m) { state = m || {}; decorate(); });
    if (window.jQuery) jQuery(document).on("shiny:value shown.bs.modal", function () { setTimeout(function () { decorate(); dtBackstop(); }, 0); });
  }
  if (window.Shiny && Shiny.addCustomMessageHandler) bind();
  else document.addEventListener("shiny:connected", function () { if (window.Shiny) bind(); }, { once: true });
  // DT loads DataTables lazily with the first table, so (re)apply after every Shiny output value.
  function dtBackstop() { if (window.jQuery && jQuery.fn && jQuery.fn.dataTable && jQuery.fn.dataTable.ext) jQuery.fn.dataTable.ext.errMode = function (s, tn, msg) { if (window.console) console.warn("DataTables:", msg); }; }
  dtBackstop();
  window.CollabnetExportReady = { _state: function () { return state; }, _onClick: onClick, message: MSG };
})();
