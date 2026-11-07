/*
 * collabnet-console.js -- CollabNET debug mode floating R console (PROTOCOL.md section 5).
 *
 * States: "hidden" (debug off; also flagged minimized), "minimized" (header only),
 *         "normal" (~32vh), "maximized" (~85vh). Debug ON -> "normal".
 * Inputs sent:   Shiny.setInputValue("console_cmd", {id, code}, {priority: "event"})
 * Messages recv: "collabnet_console": {id, origin, code, entries, truncated, elapsed_ms}
 *                                     or {type: "state", debug, ai_access}
 * All data is rendered with textContent only (never innerHTML).
 */
(function (root) {
  "use strict";

  var STATES = ["hidden", "minimized", "normal", "maximized"];
  var ENTRY_TYPES = { output: 1, message: 1, warning: 1, error: 1, value: 1 };
  var MAX_NODES = 2000;
  var MAX_HISTORY = 200;

  function createConsole(opts) {
    opts = opts || {};
    var doc = opts.document || root.document;
    var getShiny = opts.getShiny || function () { return root.Shiny; };

    var el = {
      root: doc.getElementById("collabnet-console"),
      status: doc.getElementById("collabnet-console-status"),
      output: doc.getElementById("collabnet-console-output"),
      input: doc.querySelector('[data-cn="input"]'),
      run: doc.getElementById("collabnet-console-run"),
      clear: doc.getElementById("collabnet-console-clear"),
      min: doc.getElementById("collabnet-console-min"),
      restore: doc.getElementById("collabnet-console-restore"),
      max: doc.getElementById("collabnet-console-max"),
      header: doc.getElementById("collabnet-console-header"),
      debug: doc.getElementById("debug_mode"),
      ai: doc.getElementById("console_ai_access")
    };
    if (!el.root) return null;

    var st = {
      state: "hidden",
      debug: false,
      aiAccess: false,
      history: [],
      histIdx: -1,     // -1 = editing a fresh line
      draft: "",
      pending: {},
      seq: 0,
      busy: 0
    };

    function setClass(node, cls, on) {
      if (!node || !node.classList) return;
      if (on) node.classList.add(cls); else node.classList.remove(cls);
    }

    function setStatus(text) { if (el.status) el.status.textContent = text; }

    function refreshStatus() {
      if (!st.debug) return setStatus("debug off");
      var s = st.busy > 0 ? "running…" : "ready";
      if (st.aiAccess) s += " · AI access ON";
      setStatus(s);
    }

    function setState(s) {
      if (STATES.indexOf(s) < 0) return st.state;
      if (s !== "hidden" && !st.debug) s = "hidden";
      st.state = s;
      for (var i = 0; i < STATES.length; i++) setClass(el.root, "is-" + STATES[i], STATES[i] === s);
      if (s === "hidden") setClass(el.root, "is-minimized", true); // hidden + minimized
      el.root.setAttribute("data-state", s);
      el.root.setAttribute("aria-hidden", s === "hidden" ? "true" : "false");
      var body = doc.body;
      setClass(body, "cn-console-open", s === "normal" || s === "maximized");
      setClass(body, "cn-console-min", s === "minimized");
      if (s === "normal" || s === "maximized") scrollToEnd();
      return s;
    }

    function setDebug(on) {
      on = !!on;
      var changed = on !== st.debug;
      st.debug = on;
      if (el.debug && el.debug.checked !== on) el.debug.checked = on;
      if (!on) {
        setAiAccess(false);
        setState("hidden");
      } else if (changed || st.state === "hidden") {
        setState("normal");
      }
      if (el.ai) el.ai.disabled = !on;
      refreshStatus();
    }

    function setAiAccess(on) {
      on = !!on && st.debug;
      st.aiAccess = on;
      if (el.ai && el.ai.checked !== on) el.ai.checked = on;
      setClass(el.root, "ai-on", on);
      refreshStatus();
    }

    function scrollToEnd() {
      if (el.output) el.output.scrollTop = el.output.scrollHeight || 0;
    }

    function appendNode(node) {
      el.output.appendChild(node);
      while (el.output.childNodes && el.output.childNodes.length > MAX_NODES && el.output.firstChild) {
        el.output.removeChild(el.output.firstChild);
      }
      scrollToEnd();
    }

    function line(cls, text) {
      var d = doc.createElement("div");
      d.className = "cn-entry " + cls;
      d.textContent = text == null ? "" : String(text);
      return d;
    }

    function renderEcho(code, origin) {
      var d = doc.createElement("div");
      d.className = "cn-entry cn-echo" + (origin === "ai" ? " cn-origin-ai" : "");
      var p = doc.createElement("span");
      p.className = "cn-echo-prompt";
      p.textContent = origin === "ai" ? "[AI] > " : "> ";
      var c = doc.createElement("span");
      c.className = "cn-echo-code";
      c.textContent = code == null ? "" : String(code);
      d.appendChild(p);
      d.appendChild(c);
      appendNode(d);
    }

    function renderEntries(entries) {
      if (!entries) return;
      if (!Array.isArray(entries)) entries = [entries];
      for (var i = 0; i < entries.length; i++) {
        var e = entries[i] || {};
        var t = ENTRY_TYPES[e.type] ? e.type : "output";
        appendNode(line("cn-" + t, e.text));
      }
    }

    function clear() {
      // no innerHTML: remove children one by one
      while (el.output.firstChild) el.output.removeChild(el.output.firstChild);
    }

    function handleResult(msg) {
      if (!msg) return;
      if (msg.type === "state") {
        setDebug(!!msg.debug);
        setAiAccess(!!msg.ai_access);
        return;
      }
      var id = msg.id == null ? "" : String(msg.id);
      var wasPending = Object.prototype.hasOwnProperty.call(st.pending, id);
      if (wasPending) {
        delete st.pending[id];
        st.busy = Math.max(0, st.busy - 1);
      } else {
        renderEcho(msg.code, msg.origin);
      }
      renderEntries(msg.entries);
      if (msg.truncated) appendNode(line("cn-note", "[output truncated]"));
      refreshStatus();
    }

    function submit(code) {
      if (code == null) code = el.input ? el.input.value : "";
      code = String(code);
      if (!code.replace(/\s+/g, "")) return null;
      if (st.history[st.history.length - 1] !== code) st.history.push(code);
      if (st.history.length > MAX_HISTORY) st.history.shift();
      st.histIdx = -1;
      st.draft = "";
      if (el.input) el.input.value = "";
      var id = "u" + (++st.seq) + "-" + Date.now();
      renderEcho(code, "user");
      var S = getShiny();
      if (!S || typeof S.setInputValue !== "function") {
        appendNode(line("cn-error", "Error: Shiny is not connected."));
        return null;
      }
      st.pending[id] = true;
      st.busy++;
      refreshStatus();
      S.setInputValue("console_cmd", { id: id, code: code }, { priority: "event" });
      return id;
    }

    function historyPrev() {
      if (!st.history.length) return false;
      if (st.histIdx === -1) { st.draft = el.input.value; st.histIdx = st.history.length - 1; }
      else if (st.histIdx > 0) st.histIdx--;
      el.input.value = st.history[st.histIdx];
      return true;
    }

    function historyNext() {
      if (st.histIdx === -1) return false;
      if (st.histIdx < st.history.length - 1) { st.histIdx++; el.input.value = st.history[st.histIdx]; }
      else { st.histIdx = -1; el.input.value = st.draft; }
      return true;
    }

    function onKeyDown(ev) {
      if (ev.isComposing) return;
      var v = el.input.value || "";
      var pos = typeof el.input.selectionStart === "number" ? el.input.selectionStart : v.length;
      if (ev.key === "Enter") {
        if (ev.shiftKey) return; // default: newline
        ev.preventDefault();
        submit();
      } else if (ev.key === "ArrowUp") {
        if (v.slice(0, pos).indexOf("\n") >= 0) return; // not on first line
        if (historyPrev()) ev.preventDefault();
      } else if (ev.key === "ArrowDown") {
        if (v.slice(pos).indexOf("\n") >= 0) return; // not on last line
        if (historyNext()) ev.preventDefault();
      }
    }

    // ---- wiring ----
    function on(node, evt, fn) { if (node && node.addEventListener) node.addEventListener(evt, fn); }
    on(el.input, "keydown", onKeyDown);
    on(el.run, "click", function () { submit(); });
    on(el.clear, "click", clear);
    on(el.min, "click", function () { setState("minimized"); });
    on(el.restore, "click", function () { setState("normal"); });
    on(el.max, "click", function () { setState("maximized"); });
    on(el.header, "dblclick", function (ev) {
      if (ev && ev.target && ev.target.closest && ev.target.closest("button,label,input")) return;
      setState(st.state === "minimized" ? "normal" : "minimized");
    });
    // Immediate local feedback; the server's state message stays authoritative.
    on(el.debug, "change", function () { setDebug(!!el.debug.checked); });
    on(el.ai, "change", function () { if (!st.debug && el.ai.checked) el.ai.checked = false; });

    setDebug(el.debug ? !!el.debug.checked : false);

    return {
      state: function () { return st.state; },
      setState: setState,
      setDebug: setDebug,
      setAiAccess: setAiAccess,
      submit: submit,
      handleMessage: handleResult,
      clear: clear,
      history: function () { return st.history.slice(); },
      _st: st,
      _el: el
    };
  }

  var api = { createConsole: createConsole, instance: null };
  var queued = [];

  function registerHandler() {
    var S = root.Shiny;
    if (!S || typeof S.addCustomMessageHandler !== "function" || api._registered) return !!api._registered;
    try {
      S.addCustomMessageHandler("collabnet_console", function (msg) {
        if (api.instance) api.instance.handleMessage(msg); else queued.push(msg);
      });
      api._registered = true;
    } catch (e) { if (root.console) root.console.error("collabnet-console: handler registration failed", e); }
    return !!api._registered;
  }

  // C1-1: never throw out of a shiny:connected / DOMContentLoaded listener (a throw in shiny:connected aborts
  // Shiny's socket.onopen before the init message is sent, which kills the whole app).
  function init() {
    try {
      if (!api.instance) api.instance = createConsole();
      registerHandler();
      if (api.instance) while (queued.length) api.instance.handleMessage(queued.shift());
    } catch (e) { if (root.console) root.console.error("collabnet-console: init failed", e); }
    return api.instance;
  }
  api.init = init;

  root.CollabnetConsole = api;
  registerHandler(); // as early as possible so no message is lost
  var d = root.document;
  if (root.jQuery && d) root.jQuery(d).on("shiny:connected", init); // fallback if Shiny loaded late
  if (d) {
    if (d.readyState === "loading" && d.addEventListener) d.addEventListener("DOMContentLoaded", init);
    else init();
  }
  if (typeof module !== "undefined" && module.exports) module.exports = api;
})(typeof window !== "undefined" ? window : this);
