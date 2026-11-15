/*
 * collabnet-widget-guard.js -- E3f: make plotly widgets safe in the Shinylive/webR build.
 * Owner's browser log (phase9-e3e, ORCID fetch + plots enabled):
 *   1. "Plotly is not defined" at plotly.js renderValue: htmlwidgets' shinyBinding.renderValue calls
 *      Shiny.renderDependencies(data.deps) and then bindingDef.renderValue straight away; when the plotly.js library
 *      (the plotly.js library) is injected as an async <script> (Shinylive's Shiny), Plotly does not exist yet and that plot never initialises.
 *   2. "This element is not a Plotly plot" (animate) / "reading 'length'" (coerceTraceIndices): the global
 *      "plotly-calls" handler (registered by the FIRST plot that rendered) runs plotlyProxyInvoke() calls on plots
 *      that were never initialised.
 *   3. "reading '_guiEditing'" at plotly.js resize -> Plotly.relayout on such a plot (window resize, e.g. from
 *      collabnet-resize.js).
 * Fix: wait for Plotly before rendering; queue proxy calls until the target plot exists; skip resize on a
 * non-plot. Pure helpers are exported for Node tests (tests/plotly_guard.test.mjs).
 */
(function (root) {
  "use strict";
  var WAIT_MS = 50, WAIT_MAX = 30000, CALL_MS = 150, CALL_MAX = 60000;

  // A div that Plotly has drawn (has the internals the proxy methods and relayout need).
  function isPlot(gd) { return !!(gd && gd._fullLayout && Array.isArray(gd.data)); }
  function plotlyReady(win) { return !!(win.Plotly && typeof win.Plotly.newPlot === "function"); }

  // Wrap a registered htmlwidgets shinyBinding for plotly (idempotent).
  function guardBinding(b, win) {
    if (!b || b.__cnGuard) return b;
    var render = b.renderValue, resize = b.resize;
    b.renderValue = function (el, data) {
      var self = this, args = arguments;
      if (plotlyReady(win)) return render.apply(self, args);
      try { if (win.Shiny && win.Shiny.renderDependencies && data && data.deps) win.Shiny.renderDependencies(data.deps); } catch (e) { /* retried by render */ }
      return new Promise(function (resolve) {
        var waited = 0;
        (function poll() {
          if (plotlyReady(win)) { try { resolve(render.apply(self, args)); } catch (e) { win.console && win.console.warn("CollabNET plotly render:", e); resolve(); } return; }
          if ((waited += WAIT_MS) > WAIT_MAX) { win.console && win.console.warn("CollabNET: plotly.js did not load; plot " + (el && el.id) + " not drawn"); resolve(); return; }
          win.setTimeout(poll, WAIT_MS);
        })();
      });
    };
    if (resize) b.resize = function (el) {
      if (!plotlyReady(win) || !isPlot(el)) return;   // never relayout a div Plotly has not drawn
      try { return resize.apply(this, arguments); } catch (e) { win.console && win.console.warn("CollabNET plotly resize:", e); }
    };
    b.__cnGuard = true;
    return b;
  }

  // E3i: "plotly-calls" = the plotlyProxyInvoke() handler. The plotly binding (R plotly 4.10.x-4.12.1, inst/htmlwidgets/
  // plotly.js) registers it inside plot.then() of every plot it draws and runs Plotly[msg.method].apply(null, [gd, ...args])
  // with no checks, so a restyle with trace indices on a div that is not drawn yet (proxy sent in the same flush as the
  // render, plot in a hidden tab, plot being re-rendered) dies in plotly's coerceTraceIndices ("reading 'length'" of
  // gd.data) and surfaces as "[shiny] Error on client". We register ONE persistent guarded handler and ignore the
  // binding's later re-registrations; it reproduces the binding's semantics (incl. "reconfig").
  function log(win, msg, e) { var c = win.console; if (c) (c.debug || c.warn).call(c, "CollabNET plotly: " + msg, e || ""); }
  // Same as the binding's handler body, minus its throws.
  function invokeCall(msg, win, doc) {
    var gd = doc.getElementById(msg.id), P = win.Plotly;
    if (msg.method == "reconfig") return P.react(gd, gd.data, gd.layout, msg.args);
    if (!P[msg.method]) { log(win, "unknown method " + msg.method); return; }
    return P[msg.method].apply(null, [gd].concat(msg.args));
  }
  // Which msg.args position holds trace indices, per Plotly method (animate: frame object {data, traces}).
  var TRACE_ARG = { restyle: 1, extendTraces: 1, prependTraces: 1, deleteTraces: 0, moveTraces: 0 };
  function validIdx(i, n) { return typeof i === "number" && i % 1 === 0 && i >= -n && i < n; }
  // Keep only trace indices that exist in gd.data (and the per-trace update arrays aligned with them);
  // null = nothing valid left (drop the call).
  function fixTraces(msg, n) {
    var m = msg.method;
    if (!(m in TRACE_ARG) && m !== "animate") return msg;   // reconfig, relayout, ...: untouched
    var a = Array.isArray(msg.args) ? msg.args.slice() : (msg.args == null ? [] : [msg.args]);
    function filt(idx, upd) {
      var arr = Array.isArray(idx) ? idx : [idx], keep = [];
      for (var i = 0; i < arr.length; i++) if (validIdx(arr[i], n)) keep.push(i);
      if (!keep.length) return null;
      if (keep.length === arr.length) return { idx: idx, upd: upd };
      var nu = upd;
      if (upd && typeof upd === "object" && !Array.isArray(upd)) {
        nu = {};
        for (var k in upd) nu[k] = Array.isArray(upd[k]) && upd[k].length === arr.length ? keep.map(function (j) { return upd[k][j]; }) : upd[k];
      } else if (Array.isArray(upd) && upd.length === arr.length) nu = keep.map(function (j) { return upd[j]; });
      return { idx: keep.map(function (j) { return arr[j]; }), upd: nu };
    }
    if (m in TRACE_ARG) {
      var p = TRACE_ARG[m], idx = a[p];
      if (idx == null) return msg;   // no indices = all traces (plotly maps gd.data itself)
      if (m === "moveTraces") { var all = Array.isArray(idx) ? idx : [idx]; for (var i = 0; i < all.length; i++) if (!validIdx(all[i], n)) return null; return msg; }
      var r = filt(idx, p === 1 ? a[0] : null); if (!r) return null;
      a[p] = r.idx; if (p === 1) a[0] = r.upd;
    } else if (m === "animate" && a[0] && typeof a[0] === "object" && !Array.isArray(a[0]) && a[0].traces != null) {
      var f = {}; for (var k in a[0]) f[k] = a[0][k];
      var r2 = filt(f.traces, f.data); if (!r2) return null;
      f.traces = r2.idx; f.data = r2.upd; a[0] = f;
    }
    var out = {}; for (var key in msg) out[key] = msg[key]; out.args = a; return out;
  }
  // A plot that can take proxy calls now: drawn, and not being re-rendered by Shiny.
  function callable(gd, win) {
    return plotlyReady(win) && isPlot(gd) && !(gd.classList && gd.classList.contains("recalculating"));
  }
  // Per-plot FIFO queue: run calls once the plot is drawn; drop (one console.debug) after 60 s; never throw.
  function guardCalls(handler, win, doc) {
    var queues = {};
    function run(msg) {
      var gd = doc.getElementById(msg.id), m = fixTraces(msg, gd.data.length);
      if (!m) { log(win, "dropped " + msg.method + " on " + msg.id + " (no valid trace index for " + gd.data.length + " traces)"); return; }
      try { var r = handler(m); if (r && typeof r.then === "function") r.then(null, function (e) { log(win, msg.method + " on " + msg.id + " failed", e); }); }
      catch (e) { log(win, msg.method + " on " + msg.id + " failed", e); }
    }
    function drain(id) {
      var q = queues[id]; if (!q) return;
      while (q.items.length && callable(doc.getElementById(id), win)) run(q.items.shift());
      if (!q.items.length) { delete queues[id]; return; }
      if ((q.waited += CALL_MS) > CALL_MAX) { delete queues[id]; log(win, "dropped " + q.items.length + " call(s) for " + id + " (plot never drawn)"); return; }
      win.setTimeout(function () { drain(id); }, CALL_MS);
    }
    var guarded = function (msg) {
      var id = msg && msg.id; if (!id) return;
      if (queues[id]) { queues[id].items.push(msg); return; }
      if (callable(doc.getElementById(id), win)) { run(msg); return; }
      queues[id] = { items: [msg], waited: 0 };
      win.setTimeout(function () { drain(id); }, CALL_MS);
    };
    guarded._queues = queues;
    return guarded;
  }

  function attach(win) {
    var doc = win.document, S = win.Shiny;
    if (!S || win.__cnPlotlyGuard) return false;
    win.__cnPlotlyGuard = true;
    var add = S.addCustomMessageHandler;
    if (typeof add === "function") {
      // One persistent guarded "plotly-calls" handler, registered now (before any plot draws, so early calls are queued
      // instead of being dropped as "no handler") and kept: the binding's own re-registrations are ignored.
      var calls = guardCalls(function (msg) { return invokeCall(msg, win, doc); }, win, doc);
      add.call(S, "plotly-calls", calls);
      S.addCustomMessageHandler = function (type, handler) {
        if (type === "plotly-calls") return;
        return add.call(this, type, handler);
      };
      S.__cnPlotlyCalls = calls;
      doc.addEventListener("shiny:connected", function () { add.call(S, "plotly-calls", calls); });   // reconnects
    }
    function wrapBindings() {
      var reg = S.outputBindings && S.outputBindings.getBindings ? S.outputBindings.getBindings() : [];
      for (var i = 0; i < reg.length; i++) if (reg[i] && reg[i].name === "plotly") guardBinding(reg[i].binding, win);
      var byName = S.outputBindings && S.outputBindings.bindingNames;   // Shiny keeps {binding, priority} by name here
      if (byName && byName.plotly) guardBinding(byName.plotly.binding, win);
    }
    wrapBindings();
    var reg = S.outputBindings && S.outputBindings.register;
    if (typeof reg === "function") S.outputBindings.register = function (binding, name) {
      if (name === "plotly") guardBinding(binding, win);
      return reg.apply(this, arguments);
    };
    doc.addEventListener("shiny:connected", wrapBindings);
    return true;
  }

  var api = { isPlot: isPlot, guardBinding: guardBinding, guardCalls: guardCalls, fixTraces: fixTraces, invokeCall: invokeCall, attach: attach };
  if (typeof module !== "undefined" && module.exports) module.exports = api;
  else {
    root.CollabNETPlotlyGuard = api;
    if (!attach(root)) root.document.addEventListener("DOMContentLoaded", function () { attach(root); });
  }
})(typeof window !== "undefined" ? window : this);
