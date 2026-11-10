/* E1/E3: export gate. One shared gate: CollabnetGate.request(kind, proceedFn). While publication data is loaded, every
 * export opens a dialog with a countdown (default 5 min) that ends in a free export. E3: optional support via Buy Me a
 * Coffee (BMC, any amount). The owner's BMC button script uses document.writeln, so it is rendered inside a small
 * sandboxed srcdoc iframe (its own document; link opens a new tab); if it has not rendered after ~3 s a plain yellow
 * link is shown instead. Verified unlock (Cloudflare Worker, tools/gate_core.mjs): the dialog fetches a one-time code
 * (e.g. CN-7KQ4X2) to put in the BMC message, polls <api>claim every ~5 s while open (backing off; stops on close)
 * and gets a signed token when the donation webhook arrives. Fallbacks: "Check by email" (weaker) and "Already a
 * monthly supporter? Check". The token is re-validated (<api>status, cached <= 60 s) before exports; a refund re-enables
 * the countdown. Downloads are never blocked: network errors fall back to the countdown.
 * Paid state/token are JS variables only (mirrored to the R session); never storage, cookies or .cnet.
 * The "I've donated" honour button appears ONLY when no verify endpoint is reachable (honour_fallback auto) or "on".
 * E3c: ONE Worker serves every copy (Cloudflare, GitHub Pages, local). Its origin ("gate API") is resolved as:
 * window.COLLABNET_GATE_API / CollabnetGate.setGateApi() -> config.gate_api from R (option collabnet.gate_api, env
 * COLLABNET_GATE_API, baked gate_config.json; GScholarLENS-Gate.R) -> legacy verify_url -> same origin. A placeholder
 * or "" means "no Worker" (honour fallback). Network error, timeout (5 s), CORS refusal or 5xx = unreachable;
 * a Worker answering "not paid yet" is NOT.
 * Wrapped exports: Shiny download links, DataTables Copy/PDF/Print buttons and the plotly PNG button, all via ONE
 * capture-phase click listener; agent/MCP exports are blocked in R and open this dialog. */
(function (root) {
  "use strict";
  var config = {
    enabled: true, countdown_seconds: 300,
    bmc_url: "https://buymeacoffee.com/vishveshkac",
    bmc_membership_url: "https://buymeacoffee.com/vishveshkac/membership",
    policy_url: "https://vizkidd.github.io/collabnet/donation-policy/",
    verify_url: "auto", gate_api: "same-origin", honour_fallback: "auto",
    exempt_sources: [], gate_cnet: true
  };
  // The owner's BMC button tag, verbatim (rendered only inside the srcdoc iframe).
  var BMC_TAG = '<script type="text/javascript" src="https://cdnjs.buymeacoffee.com/1.0.0/button.prod.min.js" data-name="bmc-button" data-slug="vishveshkac" data-color="#FFDD00" data-emoji="📖"  data-font="Lato" data-text="Support Me" data-outline-color="#000000" data-font-color="#000000" data-coffee-color="#ffffff" ></' + 'script>';
  // Reports to the parent whether the widget rendered. Its sha256 is allowed in tools/csp_inject.* (BMC_DETECT_HASH).
  var BMC_DETECT = '<script>parent.postMessage({cnBmc:!!document.querySelector(".bmc-btn")},"*")</' + 'script>';
  var BMC_SRCDOC = '<!doctype html><html><head><meta charset="utf-8"><base target="_blank"><style>html,body{margin:0;padding:2px;background:transparent;overflow:hidden}</style></head><body>' + BMC_TAG + BMC_DETECT + '</body></html>';
  var LIVE_EVERY = 15, BMC_TIMEOUT_MS = 3000, POLL_MS = 5000, STATUS_CACHE_MS = 60000, API_TIMEOUT_MS = 5000;
  var runtimeGateApi = null;   // explicit runtime setting (highest priority)
  var state = { paid: false, sources: [], gated: false, token: null, how: null, checkedAt: 0, verify: null };
  var queue = [];          // pending {kind, proceed}; the head owns the dialog and its countdown
  var timer = null, remaining = 0, lastLive = 0, dlg = null, lastFocus = null;

  function fmt(sec) { sec = Math.max(0, Math.ceil(sec)); var m = Math.floor(sec / 60), s = sec % 60; return (m < 10 ? "0" : "") + m + ":" + (s < 10 ? "0" : "") + s; }
  function applies(kind) {
    if (!config.enabled || !state.gated) return false;
    if (kind === "cnet" && !config.gate_cnet) return false;
    if (state.paid) return needsCheck();     // verified token older than 60 s: re-validate before this export
    return true;
  }
  function needsCheck() { return !!state.token && Date.now() - state.checkedAt > STATUS_CACHE_MS; }
  function sendInput(name, value) {
    try { if (root.Shiny && root.Shiny.setInputValue) root.Shiny.setInputValue(name, value, { priority: "event" }); } catch (e) {}
  }

  // ---- countdown-completed grant: R records it, acks, then we run the export (download handlers re-check) ----
  var acks = {}, nonceSeq = 0;
  function grantThen(kind, fn) {
    if (!(root.Shiny && root.Shiny.setInputValue)) { fn(); return; }
    var nonce = "g" + (++nonceSeq) + "_" + Date.now(), done = false;
    function go() { if (!done) { done = true; delete acks[nonce]; fn(); } }
    acks[nonce] = go;
    sendInput("cn_gate_grant", { kind: kind, nonce: nonce });
    setTimeout(go, 2000);     // no ack (e.g. disconnected): proceed anyway; R refuses if it never saw the grant
  }
  function onGranted(msg) { var f = msg && acks[msg.nonce]; if (f) f(); }

  // ---- dialog ----
  function el(tag, attrs, text) {
    var e = document.createElement(tag);
    for (var k in attrs) if (Object.prototype.hasOwnProperty.call(attrs, k)) e.setAttribute(k, attrs[k]);
    if (text != null) e.textContent = text;
    return e;
  }
  function link(href, cls, text) { return el("a", { href: href, target: "_blank", rel: "noopener noreferrer", "class": cls }, text); }
  function buildDialog() {
    var wrap = el("div", { id: "cn-gate", "class": "cn-gate-backdrop", hidden: "" });
    var box = el("div", { "class": "cn-gate-dialog", role: "dialog", "aria-modal": "true", "aria-labelledby": "cn-gate-title", "aria-describedby": "cn-gate-desc" });
    box.appendChild(el("h2", { id: "cn-gate-title" }, "Your export will start shortly"));
    box.appendChild(el("p", { id: "cn-gate-desc" }, ""));
    box.appendChild(el("div", { "class": "cn-gate-clock", "aria-hidden": "true" }, "05:00"));
    box.appendChild(el("div", { "class": "sr-only cn-gate-live", "aria-live": "polite", "aria-atomic": "true" }, ""));
    box.appendChild(el("p", { "class": "cn-gate-queue", hidden: "" }, ""));
    box.appendChild(el("p", { "class": "cn-gate-pay-text" }, ""));
    // BMC button: srcdoc iframe (the script uses document.writeln); fallback link if not rendered in ~3 s.
    var bmc = el("div", { "class": "cn-gate-bmc" });
    bmc.appendChild(el("iframe", { "class": "cn-gate-bmc-frame", title: "Buy Me a Coffee: Support Me", width: "230", height: "64",
      sandbox: "allow-scripts allow-popups allow-popups-to-escape-sandbox", referrerpolicy: "no-referrer", hidden: "" }));
    var fb = link(config.bmc_url, "cn-gate-bmc-fallback", "📖 Support Me on Buy Me a Coffee"); fb.hidden = true;
    bmc.appendChild(fb);
    box.appendChild(bmc);
    var code = el("div", { "class": "cn-gate-code", hidden: "" });
    code.appendChild(el("span", {}, "Put this code in your Buy Me a Coffee message: "));
    code.appendChild(el("strong", { "class": "cn-gate-code-value" }, ""));
    code.appendChild(el("span", { "class": "cn-gate-code-hint" }, " This tab unlocks automatically when the donation arrives (keep it open)."));
    box.appendChild(code);
    var more = el("details", { "class": "cn-gate-more", hidden: "" });
    more.appendChild(el("summary", {}, "Forgot the code, or already a monthly supporter?"));
    var form = el("div", { "class": "cn-gate-email-row" });
    form.appendChild(el("input", { type: "email", "class": "form-control cn-gate-email", placeholder: "Email used on Buy Me a Coffee", "aria-label": "Email used on Buy Me a Coffee", autocomplete: "email" }));
    form.appendChild(el("button", { type: "button", "class": "btn btn-default cn-gate-check-email" }, "Check by email"));
    form.appendChild(el("button", { type: "button", "class": "btn btn-default cn-gate-check-member" }, "Already a monthly supporter? Check"));
    more.appendChild(form);
    more.appendChild(el("p", { "class": "cn-gate-small" }, "Check by email is weaker than the code: it matches an unclaimed donation from that email in the last 60 minutes. " +
      "The email is sent once to the CollabNET Worker, which compares a salted hash; nothing is remembered in this browser."));
    box.appendChild(more);
    var monthly = el("p", { "class": "cn-gate-monthly" });
    monthly.appendChild(link(config.bmc_membership_url, "cn-gate-monthly-link", "Support monthly"));
    monthly.appendChild(el("span", { "class": "cn-gate-small cn-gate-dup-note" }, " — if you might already support monthly, use “Check” above first so you don't subscribe twice."));
    box.appendChild(monthly);
    var nr = el("p", { "class": "cn-gate-norefund" });
    nr.appendChild(el("strong", {}, "Voluntary gift, any amount. "));
    nr.appendChild(document.createTextNode("A personal gift to an individual developer via Buy Me a Coffee (their terms and fees apply). It is not a purchase " +
      "of data, a service or a licence; no 80G or other tax receipt is issued; non-refundable except under Buy Me a Coffee's refund policy. " +
      "Exports stay free after the 5-minute wait. See the "));
    nr.appendChild(link(config.policy_url, "cn-gate-policy", "donation policy"));
    nr.appendChild(document.createTextNode("."));
    box.appendChild(nr);
    box.appendChild(el("p", { "class": "cn-gate-small cn-gate-mode", hidden: "" }, ""));
    box.appendChild(el("p", { "class": "cn-gate-status", role: "status", hidden: "" }, ""));
    var row = el("div", { "class": "cn-gate-actions" });
    row.appendChild(el("button", { type: "button", "class": "btn btn-primary cn-gate-paid", hidden: "" }, "I've donated"));
    row.appendChild(el("button", { type: "button", "class": "btn btn-success cn-gate-continue", hidden: "" }, "Continue download"));
    row.appendChild(el("button", { type: "button", "class": "btn btn-default cn-gate-cancel" }, "Cancel"));
    box.appendChild(row);
    wrap.appendChild(box);
    document.body.appendChild(wrap);
    row.querySelector(".cn-gate-paid").addEventListener("click", function () { markPaid("honour"); });
    row.querySelector(".cn-gate-cancel").addEventListener("click", cancel);
    row.querySelector(".cn-gate-continue").addEventListener("click", function () { var h = queue[0]; if (h) runHead(true); });
    form.querySelector(".cn-gate-check-email").addEventListener("click", function () { checkEmail("email"); });
    form.querySelector(".cn-gate-check-member").addEventListener("click", function () { checkEmail("member"); });
    wrap.addEventListener("keydown", function (e) {
      if (e.key === "Escape") { e.preventDefault(); cancel(); }
      if (e.key === "Tab") {   // keep focus in the dialog
        var f = box.querySelectorAll("a[href]:not([hidden]), button:not([hidden]), iframe:not([hidden]), input, summary"); if (!f.length) return;
        var first = f[0], last = f[f.length - 1];
        if (e.shiftKey && document.activeElement === first) { e.preventDefault(); last.focus(); }
        else if (!e.shiftKey && document.activeElement === last) { e.preventDefault(); first.focus(); }
      }
    });
    return wrap;
  }

  // ---- BMC widget ----
  var bmcTimer = null, bmcState = "idle";   // idle | loading | rendered | fallback
  function onBmcMessage(ev) {
    var f = q(".cn-gate-bmc-frame");
    if (!f || ev.source !== f.contentWindow || !ev.data || typeof ev.data !== "object" || !("cnBmc" in ev.data)) return;
    if (ev.data.cnBmc) { bmcState = "rendered"; clearTimeout(bmcTimer); f.hidden = false; f.style.position = f.style.opacity = f.style.pointerEvents = ""; q(".cn-gate-bmc-fallback").hidden = true; }
    else showBmcFallback();
  }
  function showBmcFallback() {
    clearTimeout(bmcTimer); if (bmcState === "rendered") return;
    bmcState = "fallback"; q(".cn-gate-bmc-fallback").hidden = false;
    var f = q(".cn-gate-bmc-frame"); f.style.position = "absolute"; f.style.opacity = "0"; f.style.pointerEvents = "none";   // keep loading (late render)
  }
  function loadBmc() {
    if (bmcState !== "idle") return;     // load once per page; a late render still replaces the fallback
    bmcState = "loading";
    var f = q(".cn-gate-bmc-frame"); f.hidden = false; f.srcdoc = BMC_SRCDOC;
    bmcTimer = setTimeout(showBmcFallback, BMC_TIMEOUT_MS);
  }

  // ---- verified unlock (Worker API) ----
  // "" / placeholder -> null (no Worker); "same-origin" -> this page's origin; else the Worker origin (+ optional prefix).
  function gateOrigin(v) {
    v = v == null ? "" : String(v).trim();
    if (!v || /YOUR-WORKER|REPLACE|example\.invalid/i.test(v) || v === "none") return null;
    if (v === "same-origin" || v === "auto") { try { return root.location.origin; } catch (e) { return null; } }
    return /^https?:\/\/[^/\s]+/i.test(v) ? v.replace(/\/+$/, "") : null;
  }
  function apiBase() {
    var rt = runtimeGateApi != null ? runtimeGateApi : (root.COLLABNET_GATE_API != null ? root.COLLABNET_GATE_API : null);
    if (rt != null) { var o = gateOrigin(rt); return o ? o + "/api/gate/" : null; }
    if (config.verify_url && config.verify_url !== "auto") return String(config.verify_url).replace(/\/?$/, "/");   // legacy E3 option
    var g = gateOrigin(config.gate_api == null ? "same-origin" : config.gate_api);
    return g ? g + "/api/gate/" : null;
  }
  function apiFetch(path, opts) {
    var base = apiBase(); if (!base || !root.fetch) return Promise.reject(new Error("no api"));
    opts = opts || {};
    var ac = typeof AbortController !== "undefined" ? new AbortController() : null, t = null;
    if (ac) { opts.signal = ac.signal; t = setTimeout(function () { ac.abort(); }, API_TIMEOUT_MS); }
    opts.credentials = "omit";   // no cookies: the token lives in page memory and is sent in the body
    return root.fetch(base + path, opts).then(function (r) {
      clearTimeout(t);
      if (!r.ok && r.status !== 400) throw new Error("http " + r.status);
      return r.json();
    }, function (e) { clearTimeout(t); throw e; });
  }
  function postJson(path, obj) { return apiFetch(path, { method: "POST", headers: { "content-type": "application/json" }, body: JSON.stringify(obj) }); }
  function showMode(ok) {
    var m = q(".cn-gate-mode"); if (!m) return;
    m.hidden = false;
    m.textContent = ok ? "Donations are verified via Cloudflare (CollabNET's donation checker)."
      : "Couldn't reach the donation checker, so the “I've donated” button (honour system) is shown instead.";
  }
  function unreachable() { state.verify = false; showHonour(config.honour_fallback !== "off"); showMode(false); }
  function probeVerify() {   // -> Promise<boolean>; cached for the page session
    if (config.honour_fallback === "on") { state.verify = false; return Promise.resolve(false); }
    if (state.verify !== null) return Promise.resolve(state.verify);
    return apiFetch("stats").then(function (j) { state.verify = !!j && typeof j === "object" && "possible_duplicate_membership" in j; return state.verify; },
      function () { state.verify = false; return false; });
  }
  function showHonour(on) { var b = q(".cn-gate-paid"); if (b) b.hidden = !on; }
  var poll = { timer: null, code: null, delay: POLL_MS, started: 0 };
  function stopPolling() { clearTimeout(poll.timer); poll.timer = null; }
  function schedulePoll() {
    stopPolling();
    var age = Date.now() - poll.started;   // back off: 5 s, then 10 s after 2 min, 20 s after 10 min
    poll.delay = age > 600000 ? 4 * POLL_MS : age > 120000 ? 2 * POLL_MS : POLL_MS;
    poll.timer = setTimeout(pollOnce, poll.delay);
  }
  function pollOnce() {
    if (!poll.code || !dlg || dlg.hidden) { stopPolling(); return; }
    apiFetch("claim?code=" + encodeURIComponent(poll.code)).then(function (j) {
      if (j && j.status === "paid" && j.token) { unlockWithToken(j.token, "bmc-code"); return; }
      if (j && j.status === "refunded") status("That donation was refunded — the countdown continues.");
      if (j && j.status === "expired") { poll.code = null; return startCode(); }
      if (dlg && !dlg.hidden) schedulePoll();
    }, function () { if (dlg && !dlg.hidden) { unreachable(); schedulePoll(); } });
  }
  function startCode() {
    if (poll.code) { q(".cn-gate-code").hidden = false; poll.started = Date.now(); schedulePoll(); return Promise.resolve(); }
    return apiFetch("code", { method: "POST" }).then(function (j) {
      if (!j || !j.code || !dlg || dlg.hidden) return;
      poll.code = j.code; poll.started = Date.now();
      q(".cn-gate-code-value").textContent = j.code; q(".cn-gate-code").hidden = false; q(".cn-gate-more").hidden = false;
      schedulePoll();
    }, unreachable);
  }
  function status(text) { var s = q(".cn-gate-status"); if (s) { s.hidden = !text; s.textContent = text || ""; } }
  function fmtDate(sec) { try { return new Date(sec * 1000).toISOString().slice(0, 10); } catch (e) { return ""; } }
  function checkEmail(which) {
    var inp = q(".cn-gate-email"), email = inp && inp.value.trim();
    if (!email || email.indexOf("@") < 1) { status("Enter the email you used on Buy Me a Coffee."); return; }
    status("Checking…");
    postJson(which, { email: email }).then(function (j) {
      if (inp) inp.value = "";   // not kept
      if (which === "member") {
        if (j && j.active) {
          var ends = j.ends ? fmtDate(j.ends) : "";
          q(".cn-gate-monthly-link").hidden = true;
          q(".cn-gate-dup-note").textContent = "You already have an active monthly support" + (ends ? " (" + (j.cancel_at_period_end ? "ends " : "renews ") + ends + ")" : "") + ".";
          if (j.token) unlockWithToken(j.token, "bmc-member");
        } else status("No active monthly support found for that email.");
      } else if (j && j.status === "paid" && j.token) unlockWithToken(j.token, "bmc-email");
      else status("No unclaimed donation from that email in the last 60 minutes.");
    }, function () { status("Could not reach the verification service — the countdown continues."); });
  }
  function unlockWithToken(token, how) { state.token = token; state.checkedAt = Date.now(); markPaid(how); }
  function revoke(reason) {
    state.paid = false; state.token = null; state.how = null;
    sendInput("cn_gate_paid", { paid: false, how: reason, t: Date.now() });
  }
  // Re-validate a token before an export; network errors keep the unlock (never block a download).
  function revalidate() {
    return postJson("status", { token: state.token }).then(function (j) {
      if (j && j.valid === false) { revoke(j.reason || "invalid"); return false; }
      state.checkedAt = Date.now(); return true;
    }, function () { state.checkedAt = Date.now(); return true; });
  }

  function q(sel) { return dlg && dlg.querySelector(sel); }
  function render() {
    if (!dlg) return;
    q(".cn-gate-clock").textContent = fmt(remaining);
    var n = queue.length - 1, qp = q(".cn-gate-queue");
    qp.hidden = n <= 0; qp.textContent = n > 0 ? n + " more export" + (n > 1 ? "s" : "") + " queued; each gets its own countdown." : "";
  }
  function live(force) {
    var now = Date.now();
    if (!force && now - lastLive < LIVE_EVERY * 1000) return;
    lastLive = now;
    q(".cn-gate-live").textContent = "Export starts in " + fmt(remaining).replace(":", " minutes ") + " seconds.";
  }
  function openFor(head) {
    if (!dlg) dlg = buildDialog();
    var mins = Math.round(config.countdown_seconds / 60);
    q("#cn-gate-desc").textContent = "Data is loaded, so this export (" + head.kind + ") starts automatically when the countdown ends.";
    q(".cn-gate-pay-text").textContent = "Optionally support CollabNET with any amount on Buy Me a Coffee to skip the wait for the rest of this session (otherwise the wait is " +
      (mins >= 1 ? mins + " min" : config.countdown_seconds + " s") + " per export and the export is free).";
    q(".cn-gate-monthly-link").setAttribute("href", config.bmc_membership_url);
    q(".cn-gate-bmc-fallback").setAttribute("href", config.bmc_url);
    q(".cn-gate-continue").hidden = true;
    showHonour(config.honour_fallback === "on" || (config.honour_fallback === "auto" && state.verify === false));
    remaining = config.countdown_seconds; lastLive = 0;
    if (!dlg.hidden) { render(); live(true); } else {
      lastFocus = document.activeElement;
      dlg.hidden = false; render(); live(true);
      var b = q(".cn-gate-cancel"); if (b) b.focus();
      loadBmc();
      probeVerify().then(function (ok) {
        if (!dlg || dlg.hidden) return;
        showHonour(config.honour_fallback === "on" || (config.honour_fallback === "auto" && !ok));
        showMode(ok);
        if (ok) startCode();
      });
    }
    clearInterval(timer);
    var end = Date.now() + remaining * 1000;
    timer = setInterval(function () {
      remaining = (end - Date.now()) / 1000;
      if (remaining <= 0) { remaining = 0; clearInterval(timer); timer = null; render(); runHead(false); return; }
      render(); live(false);
    }, 250);
  }
  function close() {
    clearInterval(timer); timer = null; stopPolling();
    if (dlg) { dlg.hidden = true; var s = q(".cn-gate-status"); s.hidden = true; s.textContent = ""; }
    if (lastFocus && lastFocus.focus) try { lastFocus.focus(); } catch (e) {}
  }
  // Countdown ended (or "Continue download" clicked): grant + run the head export. Programmatic downloads can be
  // blocked (no user gesture after 5 minutes, e.g. Safari / multiple-download prompts), so the dialog stays open
  // with an enabled "Continue download" button for this export instead of closing straight away.
  function runHead(fromButton) {
    var h = queue[0]; if (!h) { close(); return; }
    var proceed = h.proceed;
    grantThen(h.kind, function () { try { proceed(); } catch (e) {} });
    if (fromButton || h.ranOnce) { queue.shift(); next(); return; }
    h.ranOnce = true;
    q(".cn-gate-clock").textContent = "00:00";
    q("#cn-gate-desc").textContent = "Your export (" + h.kind + ") has started. If no download appeared, use “Continue download”.";
    q(".cn-gate-live").textContent = "Countdown finished. Export started.";
    q(".cn-gate-continue").hidden = false;
    showHonour(false);
    var c = q(".cn-gate-continue"); if (c) c.focus();
    setTimeout(function () { if (queue[0] === h) { queue.shift(); next(); } }, 15000);
  }
  function next() { if (queue.length) openFor(queue[0]); else close(); }
  function cancel() { queue = []; close(); }

  function request(kind, proceed) {
    if (state.paid && applies(kind)) {   // verified unlock older than 60 s: re-check (refund -> countdown)
      revalidate().then(function (ok) { if (ok) grantThen(kind, proceed); else request(kind, proceed); });
      return "revalidating";
    }
    if (!applies(kind)) { proceed(); return "direct"; }
    // A second request while a countdown runs reuses the open dialog: it is queued and gets its own fresh countdown
    // after the current one (no stacked dialogs, no shared/shortened wait).
    for (var i = 0; i < queue.length; i++) if (queue[i].kind === kind && !queue[i].ranOnce) return "duplicate";
    queue.push({ kind: kind, proceed: proceed });
    if (queue.length === 1) openFor(queue[0]); else render();
    return "queued";
  }

  function markPaid(how) {
    state.paid = true; state.how = how;   // in memory only
    sendInput("cn_gate_paid", { paid: true, how: how, token: state.token, t: Date.now() });   // R re-checks the token with the Worker
    poll.code = null;
    var pending = queue.filter(function (h) { return !h.ranOnce; }); queue = []; close();
    // via the grant/ack round trip so R has processed cn_gate_paid before the download request arrives
    pending.forEach(function (h, i) { setTimeout(function () { grantThen(h.kind, h.proceed); }, i * 400); });
  }

  // ---- export wrapping: one capture-phase listener ----
  var BYPASS = "cnGateBypass";
  function kindOf(t) {
    var a = t.closest && t.closest("a.shiny-download-link");
    if (a) return { el: a, kind: a.id === "download_btn" ? "cnet" : (a.id || "download") };
    var b = t.closest && t.closest(".dt-button, .dt-buttons > *");
    if (b) { var m = (b.className.match(/buttons-(copy|pdf|print|csv|excel)/) || [])[1]; return { el: b, kind: "table_" + (m || "button") }; }
    var p = t.closest && t.closest(".modebar-btn");
    if (p) { var dt = (p.getAttribute("data-title") || "") + " " + (p.getAttribute("data-val") || ""); if (/png|download|image/i.test(dt)) return { el: p, kind: "plot_png" }; }
    return null;
  }
  function onClick(e) {
    var hit = kindOf(e.target); if (!hit) return;
    if (hit.el.dataset[BYPASS] === "1") { delete hit.el.dataset[BYPASS]; return; }
    if (!applies(hit.kind)) return;
    e.preventDefault(); e.stopImmediatePropagation();
    request(hit.kind, function () { hit.el.dataset[BYPASS] = "1"; hit.el.click(); });
  }

  function attach(win) {
    var doc = win.document;
    doc.addEventListener("click", onClick, true);
    win.addEventListener("message", onBmcMessage);
    var S = win.Shiny;
    function bind() {
      win.Shiny.addCustomMessageHandler("cn_gate_config", function (c) { for (var k in c) if (Object.prototype.hasOwnProperty.call(config, k)) config[k] = c[k]; if (!Array.isArray(config.exempt_sources)) config.exempt_sources = config.exempt_sources ? [config.exempt_sources] : []; });
      win.Shiny.addCustomMessageHandler("cn_gate_state", function (s) { state.sources = s.sources || []; state.gated = !!s.gated; });
      win.Shiny.addCustomMessageHandler("cn_gate_granted", onGranted);
      win.Shiny.addCustomMessageHandler("cn_gate_open", function (m) { request((m && m.kind) || "agent_export", function () {}); });
    }
    if (S && S.addCustomMessageHandler) bind();
    else doc.addEventListener("shiny:connected", function () { if (win.Shiny && win.Shiny.addCustomMessageHandler) bind(); }, { once: true });
  }

  var api = {
    request: request, markPaid: markPaid, cancel: cancel, attach: attach, config: config, fmt: fmt, applies: applies,
    isPaid: function () { return state.paid; }, _state: state, _queue: function () { return queue; },
    _poll: poll, _bmcState: function () { return bmcState; }, _revalidate: revalidate, _probeVerify: probeVerify,
    _apiBase: apiBase, setGateApi: function (v) { runtimeGateApi = v; state.verify = null; },
    BMC_TAG: BMC_TAG, BMC_DETECT: BMC_DETECT, BMC_SRCDOC: BMC_SRCDOC, LIVE_EVERY: LIVE_EVERY, POLL_MS: POLL_MS
  };
  if (typeof module !== "undefined" && module.exports) module.exports = api;
  root.CollabnetGate = api;
  if (typeof window !== "undefined" && window.document && !(typeof module !== "undefined" && module.exports)) {
    if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", function () { attach(window); });
    else attach(window);
  }
})(typeof window !== "undefined" ? window : globalThis);
