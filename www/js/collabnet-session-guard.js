/*
 * collabnet-session-guard.js -- D1-1: "R session ended" overlay + "Restart R session" helper.
 *
 * Loaded twice: (1) inside the app iframe (ui.R) where it listens for shiny:disconnected and wires the
 * console "Restart R session" button; (2) in the outer Shinylive shell (wasm_build/index.html, added by
 * csp_inject.py) where it listens for unhandledrejection whose reason is webR's ExitStatus
 * ("Program terminated with exit(0)"). External file on purpose: the shell CSP has no 'unsafe-inline'.
 * All text is set with textContent; no inline handlers.
 *
 * D2-A3 heartbeat (app frame only): the D1 outer-shell ExitStatus listener is unlikely to fire, because webR runs R in
 * a Web Worker and neither webR nor Shinylive forwards the exit to the page (reference_findings.md A1). So the app also
 * pings the server every ~10 s while the tab is visible (Shiny input "cn_hb_ping" -> custom message "cn_hb_pong",
 * register_session_heartbeat() in GScholarLENS-Console.R). Missed replies are judged against Shiny's busy state:
 *   R idle + 3 missed replies (~30 s)            -> "R session has stopped responding" overlay (Reload / Save first)
 *   R busy (shiny-busy) + >= 2 missed replies     -> non-blocking "R is busy..." banner, no overlay
 *   R busy for longer than busyDeadMs (3 min)     -> overlay with an extra "Keep waiting" button
 * A reply clears the banner and the counters. shiny:disconnected still shows the overlay at once.
 */
(function (root) {
  "use strict";

  var OVERLAY_ID = "collabnet-session-ended";
  var MSG = "The R session has ended. Reload to restart.";

  // True when an unhandledrejection reason / error looks like an R (Emscripten) exit.
  function isExitReason(reason) {
    if (reason == null) return false;
    var name = "", text = "";
    try {
      name = String((reason.constructor && reason.constructor.name) || reason.name || "");
      text = String(reason.message || reason.status !== undefined && ("ExitStatus " + reason.status) || reason);
    } catch (e) { return false; }
    return /ExitStatus/.test(name) || /ExitStatus|Program terminated with exit\(/.test(text);
  }

  function reloadTop(win) {
    win = win || root;
    try { if (win.top && win.top.location) { win.top.location.reload(); return "top"; } } catch (e) { /* cross-origin */ }
    win.location.reload();
    return "frame";
  }

  function showOverlay(doc, opts) {
    opts = opts || {};
    doc = doc || root.document;
    if (!doc || !doc.body) return null;
    var existing = doc.getElementById(OVERLAY_ID);
    if (existing) return existing;
    var ov = doc.createElement("div");
    ov.id = OVERLAY_ID;
    ov.setAttribute("role", "alertdialog");
    ov.setAttribute("aria-live", "assertive");
    ov.setAttribute("aria-label", MSG);
    ov.style.cssText = "position:fixed;inset:0;z-index:2147483647;background:rgba(20,20,20,.82);color:#fff;" +
      "display:flex;flex-direction:column;align-items:center;justify-content:center;font:16px sans-serif;";
    var p = doc.createElement("p");
    p.textContent = opts.message || MSG;
    var b = doc.createElement("button");
    b.type = "button";
    b.className = "btn btn-primary";
    b.textContent = "Reload";
    b.addEventListener("click", function () { (opts.reload || reloadTop)(); });
    ov.appendChild(p);
    ov.appendChild(b);
    doc.body.appendChild(ov);
    try { b.focus(); } catch (e) { /* ignore */ }
    return ov;
  }

  // Restart button: warn, optionally save a .cnet first (clicks the existing export button), then reload.
  function restartSession(opts) {
    opts = opts || {};
    var win = opts.window || root;
    var ask = opts.confirm || function (m) { return win.confirm(m); };
    if (!ask("Restart the R session? The page reloads and unsaved analysis state is lost.")) return "cancelled";
    var doc = opts.document || win.document;
    if (ask("Save the current session as a .cnet file first? (OK = save, Cancel = restart without saving)")) {
      var btn = doc && doc.getElementById("predownload_btn");
      if (btn) { if (win.jQuery) win.jQuery(btn).trigger("click"); else btn.click(); }
      return "saving";  // the user restarts again after the download finishes
    }
    (opts.reload || function () { return reloadTop(win); })();
    return "reloaded";
  }

  // ---- D2-A3 heartbeat: pure state machine (unit-tested; no timers or DOM inside) ----
  var HB_DEFAULTS = { intervalMs: 10000, deadMisses: 3, busyBannerMisses: 2, busyDeadMs: 180000 };
  function createHeartbeat(cfg) {
    var c = {}, k;
    for (k in HB_DEFAULTS) c[k] = (cfg && typeof cfg[k] === "number") ? cfg[k] : HB_DEFAULTS[k];
    var st = { seq: 0, outstanding: null, missed: 0, busySince: null, status: "ok", dismissedUntilPong: false };
    // Called every intervalMs. env = {visible, connected, busy}. Returns {send: seq|null, status}.
    function tick(now, env) {
      env = env || {};
      if (env.busy) { if (st.busySince === null) st.busySince = now; } else st.busySince = null;
      if (!env.visible || !env.connected) return { send: null, status: st.status };   // paused: hidden tab / not yet connected
      if (st.outstanding !== null) st.missed += 1;
      st.seq += 1; st.outstanding = st.seq;
      var status = "ok";
      if (env.busy) {
        if (now - st.busySince >= c.busyDeadMs) status = "dead-busy";
        else if (st.missed >= c.busyBannerMisses) status = "busy";
      } else if (st.missed >= c.deadMisses) status = "dead";
      else if (st.missed >= 1) status = "slow";
      if (st.dismissedUntilPong && status === "dead-busy") status = "busy";
      st.status = status;
      return { send: st.seq, status: status };
    }
    function pong(seq) {
      if (typeof seq !== "number" || !isFinite(seq) || seq < 1 || seq > st.seq) return st.status;   // ignore malformed / future replies
      st.outstanding = null; st.missed = 0; st.dismissedUntilPong = false; st.status = "ok";
      return st.status;
    }
    function keepWaiting() { st.dismissedUntilPong = true; st.status = "busy"; }
    return { tick: tick, pong: pong, keepWaiting: keepWaiting, state: function () { return st; }, config: c };
  }

  var BANNER_ID = "collabnet-r-busy";
  function setBusyBanner(doc, on, text) {
    var el = doc.getElementById(BANNER_ID);
    if (!on) { if (el && el.parentNode) el.parentNode.removeChild(el); return null; }
    if (!el) {
      el = doc.createElement("div");
      el.id = BANNER_ID;
      el.className = "collabnet-r-busy";
      el.setAttribute("role", "status");
      el.setAttribute("aria-live", "polite");
      doc.body.appendChild(el);
    }
    el.textContent = text;
    return el;
  }

  function saveSessionFirst(win, doc) {
    var btn = doc && doc.getElementById("predownload_btn");
    if (!btn) return false;
    if (win.jQuery) win.jQuery(btn).trigger("click"); else btn.click();
    return true;
  }

  function showDeadOverlay(win, doc, hb, busy) {
    var secs = Math.round((hb.config.deadMisses * hb.config.intervalMs) / 1000);
    var msg = busy
      ? "R has been busy for more than " + Math.round(hb.config.busyDeadMs / 60000) + " minutes without answering. It may still be computing, or the session may have ended."
      : "The R session has stopped responding (no reply for about " + secs + " s). It has probably ended.";
    var ov = showOverlay(doc, { message: msg });
    if (!ov || ov.__cnHb) return ov;
    ov.__cnHb = true;
    var save = doc.createElement("button");
    save.type = "button"; save.className = "btn btn-default";
    save.textContent = "Save session (.cnet) first";
    save.title = "Tries to download the session; works only if R still responds.";
    save.addEventListener("click", function () { if (!saveSessionFirst(win, doc)) save.textContent = "Save not available"; });
    ov.appendChild(save);
    if (busy) {
      var wait = doc.createElement("button");
      wait.type = "button"; wait.className = "btn btn-default";
      wait.textContent = "Keep waiting";
      wait.addEventListener("click", function () { hb.keepWaiting(); if (ov.parentNode) ov.parentNode.removeChild(ov); });
      ov.appendChild(wait);
    }
    return ov;
  }

  // Wire the heartbeat to Shiny in the app frame. Returns the heartbeat (for tests / debugging) or null.
  function startHeartbeat(win, cfg) {
    var doc = win.document, $ = win.jQuery;
    if (!$ || !doc) return null;
    var hb = createHeartbeat(cfg || win.CollabNETHeartbeatConfig);
    var connected = false;
    $(doc).on("shiny:connected", function () { connected = true; });
    $(doc).on("shiny:disconnected", function () { connected = false; });
    function isConnected() {
      if (connected) return true;
      try { return !!(win.Shiny && win.Shiny.shinyapp && win.Shiny.shinyapp.isConnected()); } catch (e) { return false; }
    }
    function busyNow() { return !!(doc.documentElement && /(^|\s)shiny-busy(\s|$)/.test(doc.documentElement.className)); }
    function registerPong() {
      if (win.Shiny && win.Shiny.addCustomMessageHandler && !win.__cnHbHandler) {
        win.__cnHbHandler = true;
        win.Shiny.addCustomMessageHandler("cn_hb_pong", function (seq) {
          hb.pong(Number(seq));
          setBusyBanner(doc, false);
          var ov = doc.getElementById(OVERLAY_ID);
          if (ov && ov.__cnHb && connected) ov.parentNode.removeChild(ov);   // R answered after all
        });
      }
    }
    registerPong();
    $(doc).on("shiny:connected", registerPong);
    win.setInterval(function () {
      var r = hb.tick(Date.now(), { visible: doc.visibilityState !== "hidden", connected: isConnected(), busy: busyNow() });
      if (r.send !== null && win.Shiny && win.Shiny.setInputValue) win.Shiny.setInputValue("cn_hb_ping", r.send, { priority: "event" });
      if (r.status === "busy") setBusyBanner(doc, true, "R is busy\u2026 (still computing; the app will update when it finishes)");
      else if (r.status === "ok" || r.status === "slow") setBusyBanner(doc, false);
      if (r.status === "dead") { setBusyBanner(doc, false); showDeadOverlay(win, doc, hb, false); }
      if (r.status === "dead-busy") { setBusyBanner(doc, false); showDeadOverlay(win, doc, hb, true); }
    }, hb.config.intervalMs);
    return hb;
  }

  function attach(win) {
    win = win || root;
    var doc = win.document;
    if (!doc || !win.addEventListener) return;
    win.addEventListener("unhandledrejection", function (ev) {
      if (isExitReason(ev && ev.reason)) showOverlay(doc);
    });
    win.addEventListener("error", function (ev) {
      if (isExitReason(ev && (ev.error || ev.message))) showOverlay(doc);
    });
    // inside the app frame: Shiny socket closed (jQuery event)
    if (win.jQuery) win.jQuery(doc).on("shiny:disconnected", function () { showOverlay(doc); });
    // D2-A3: heartbeat only where Shiny runs (the app frame), not in the outer Shinylive shell
    if (win.jQuery && win.Shiny) win.__cnHeartbeat = startHeartbeat(win);
    var wire = function () {
      var rb = doc.getElementById("collabnet-console-restart");
      if (rb && !rb.__cnWired) { rb.__cnWired = true; rb.addEventListener("click", function () { restartSession({ window: win }); }); }
    };
    if (doc.readyState === "loading") doc.addEventListener("DOMContentLoaded", wire); else wire();
  }

  var api = { isExitReason: isExitReason, showOverlay: showOverlay, restartSession: restartSession,
              reloadTop: reloadTop, attach: attach, OVERLAY_ID: OVERLAY_ID, MESSAGE: MSG,
              createHeartbeat: createHeartbeat, setBusyBanner: setBusyBanner, showDeadOverlay: showDeadOverlay,
              startHeartbeat: startHeartbeat, HB_DEFAULTS: HB_DEFAULTS, BANNER_ID: BANNER_ID };
  if (typeof module !== "undefined" && module.exports) module.exports = api;
  else { root.CollabNETSessionGuard = api; attach(root); }
})(typeof window !== "undefined" ? window : this);
