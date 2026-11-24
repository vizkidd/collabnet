// P10-C6: pure logic of the loading screen (no DOM), shared by every edition's loader (module "loader") and unit-tested
// in Node (tests/loader_p10c6.test.mjs): the "taking longer than usual" rule and the "While you wait" tip rotation.
// UMD: module.exports in Node, window.CollabnetLoaderCore in the browser.
(function (root, factory) {
  if (typeof module === "object" && module.exports) module.exports = factory();
  else root.CollabnetLoaderCore = factory();
})(typeof self !== "undefined" ? self : this, function () {
  "use strict";
  // All slow-hint constants live here. Progress = an observable stage change of the loader (download -> R start ->
  // packages -> app). Shinylive 0.9.1 exposes no byte counter to the page, so no byte-level progress is used.
  var SLOW = {
    stall_s: 100,     // earliest hint: at 100 s (C25_r10, was 80 s), if no progress was seen in the last stall_window_s
    window_s: 20,     // "stalled" = no progress for this long
    max_s: 120        // otherwise (still progressing, just slow) the hint appears at 120 s
  };
  function isSlow(nowMs, startMs, lastProgressMs, cfg) {
    var c = cfg || SLOW, el = (nowMs - startMs) / 1000;
    if (el >= c.max_s) return true;
    if (el < c.stall_s) return false;
    var since = (nowMs - (lastProgressMs == null ? startMs : lastProgressMs)) / 1000;
    return since >= c.window_s;
  }

  // Tip rotation: random start, auto-advance to a random DIFFERENT tip every minMs..maxMs (random interval), prev/next
  // (sequential, wraps), manual navigation pauses auto-advance for manualPauseMs, hover/focus pause until released.
  // Timers and randomness are injected so tests can drive them with fake clocks.
  var TIPS = { minMs: 10000, maxMs: 15000, manualPauseMs: 30000 };
  function createRotator(o) {
    var n = o.n, rand = o.rand || Math.random, setT = o.setTimeout || setTimeout, clrT = o.clearTimeout || clearTimeout;
    var cfg = { minMs: o.minMs || TIPS.minMs, maxMs: o.maxMs || TIPS.maxMs, manualPauseMs: o.manualPauseMs || TIPS.manualPauseMs };
    var onChange = o.onChange || function () {};
    var idx = n > 0 ? Math.min(n - 1, Math.floor(rand() * n)) : -1, timer = null, holds = {}, manualUntil = 0, stopped = false;
    function now() { return o.now ? o.now() : Date.now(); }
    function interval() { return cfg.minMs + Math.floor(rand() * (cfg.maxMs - cfg.minMs + 1)); }
    function randomOther() { if (n < 2) return idx; var j = Math.floor(rand() * (n - 1)); return j >= idx ? j + 1 : j; }
    function held() { for (var k in holds) if (holds[k]) return true; return false; }
    function set(i, how) { idx = ((i % n) + n) % n; onChange(idx, how); }
    function schedule() {
      clrT(timer); timer = null;
      if (stopped || n < 2 || held()) return;
      var wait = Math.max(interval(), manualUntil - now());
      timer = setT(function () {
        timer = null;
        if (now() < manualUntil) { schedule(); return; }
        set(randomOther(), "auto"); schedule();
      }, wait);
    }
    function manual(d) { if (n < 1) return; manualUntil = now() + cfg.manualPauseMs; set(idx + d, "manual"); schedule(); }
    return {
      index: function () { return idx; }, count: function () { return n; },
      start: function () { if (n > 0) onChange(idx, "start"); schedule(); },
      next: function () { manual(1); }, prev: function () { manual(-1); },
      hold: function (why, on) { holds[why] = !!on; schedule(); },   // hover / focus pause
      stop: function () { stopped = true; clrT(timer); timer = null; },
      pending: function () { return timer !== null; },
      cfg: cfg
    };
  }
  function position(i, n) { return n > 0 ? (i + 1) + " / " + n : ""; }
  return { SLOW: SLOW, TIPS: TIPS, isSlow: isSlow, createRotator: createRotator, position: position };
});
