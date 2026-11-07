/**
 * CollabNET worker pool (C2-1).
 *
 * Small, lazily created pool of Web Workers used to compute network layouts off
 * the page main thread. In Shinylive, R/igraph already runs inside the webR
 * worker; what blocks the page is vis-network work on large graphs. Layout
 * coordinates computed here are handed to vis-network with physics disabled,
 * so the main thread only renders.
 *
 *  - size = clamp(hardwareConcurrency - 1, 1, 4); workers are created on first use
 *  - inputs/outputs are transferable typed arrays (Int32Array edges, Float32Array coords)
 *  - a newer request with the same key cancels the stale one (worker is terminated)
 *  - per-task timeout; on any Worker/Blob failure the kernel runs on the main thread
 *  - every error is caught and reported; nothing here may break the app
 */
(function (root) {
  'use strict';

  // ---- layout kernel: plain JS, no dependencies; also runs inside the worker ----
  // Fruchterman-Reingold with grid-binned repulsion (O(n) per iteration on
  // average) and a deterministic seeded PRNG (mulberry32).
  function layoutKernel(n, edges, opts) {
    opts = opts || {};
    var iters = Math.max(1, opts.iterations | 0 || 300);
    var seed = (opts.seed | 0) || 42;
    var shouldStop = opts.shouldStop || function () { return false; };
    var pos = new Float32Array(2 * n);
    if (n === 0) return pos;
    var s = seed >>> 0;
    function rnd() { s = (s + 0x6D2B79F5) >>> 0; var t = s; t = Math.imul(t ^ (t >>> 15), t | 1); t ^= t + Math.imul(t ^ (t >>> 7), t | 61); return ((t ^ (t >>> 14)) >>> 0) / 4294967296; }
    var side = Math.sqrt(n) * 100, k = side / Math.sqrt(n), k2 = k * k;
    for (var i = 0; i < 2 * n; i++) pos[i] = (rnd() - 0.5) * side;
    var disp = new Float32Array(2 * n), m = edges.length >> 1;
    var cell = 2 * k, head, next = new Int32Array(n);
    var temp = side / 10, cool = temp / (iters + 1);
    for (var it = 0; it < iters; it++) {
      if ((it & 15) === 0 && shouldStop()) return null;
      disp.fill(0);
      // bin nodes into a grid; repulsion only from the 3x3 neighbourhood
      var minx = Infinity, miny = Infinity, maxx = -Infinity, maxy = -Infinity;
      for (i = 0; i < n; i++) { var x = pos[2 * i], y = pos[2 * i + 1]; if (x < minx) minx = x; if (x > maxx) maxx = x; if (y < miny) miny = y; if (y > maxy) maxy = y; }
      var gw = Math.max(1, Math.ceil((maxx - minx) / cell) + 1), gh = Math.max(1, Math.ceil((maxy - miny) / cell) + 1);
      if (gw * gh > 4 * n + 16) { var f = Math.sqrt(gw * gh / (4 * n + 16)); cell *= f; gw = Math.ceil((maxx - minx) / cell) + 1; gh = Math.ceil((maxy - miny) / cell) + 1; }
      head = new Int32Array(gw * gh).fill(-1);
      for (i = 0; i < n; i++) { var c = ((pos[2 * i] - minx) / cell | 0) + gw * ((pos[2 * i + 1] - miny) / cell | 0); next[i] = head[c]; head[c] = i; }
      for (i = 0; i < n; i++) {
        var cx = (pos[2 * i] - minx) / cell | 0, cy = (pos[2 * i + 1] - miny) / cell | 0;
        for (var dy = -1; dy <= 1; dy++) for (var dx = -1; dx <= 1; dx++) {
          var gx = cx + dx, gy = cy + dy; if (gx < 0 || gy < 0 || gx >= gw || gy >= gh) continue;
          for (var j = head[gx + gw * gy]; j !== -1; j = next[j]) {
            if (j === i) continue;
            var ddx = pos[2 * i] - pos[2 * j], ddy = pos[2 * i + 1] - pos[2 * j + 1], d2 = ddx * ddx + ddy * ddy;
            if (d2 < 1e-4) { ddx = (rnd() - 0.5); ddy = (rnd() - 0.5); d2 = ddx * ddx + ddy * ddy; }
            var fr = k2 / d2; disp[2 * i] += ddx * fr; disp[2 * i + 1] += ddy * fr;
          }
        }
      }
      for (var e = 0; e < m; e++) {
        var a = edges[2 * e], b = edges[2 * e + 1]; if (a === b) continue;
        var ex = pos[2 * a] - pos[2 * b], ey = pos[2 * a + 1] - pos[2 * b + 1], d = Math.sqrt(ex * ex + ey * ey) + 1e-6, fa = d / k;
        disp[2 * a] -= ex * fa; disp[2 * a + 1] -= ey * fa; disp[2 * b] += ex * fa; disp[2 * b + 1] += ey * fa;
      }
      for (i = 0; i < n; i++) {
        var vx = disp[2 * i], vy = disp[2 * i + 1], l = Math.sqrt(vx * vx + vy * vy) + 1e-9, st = Math.min(l, temp) / l;
        pos[2 * i] += vx * st; pos[2 * i + 1] += vy * st;
      }
      temp = Math.max(temp - cool, k * 0.01);
    }
    return pos;
  }

  var WORKER_SRC = 'var layoutKernel = ' + layoutKernel.toString() + ';\n' +
    'self.onmessage = function (e) { var d = e.data; try {' +
    ' var pos = layoutKernel(d.n, d.edges, d.opts);' +
    ' self.postMessage({ id: d.id, pos: pos }, [pos.buffer]);' +
    ' } catch (err) { self.postMessage({ id: d.id, error: String(err && err.message || err) }); } };';

  function makePool(env) {
    env = env || {};
    var hc = env.hardwareConcurrency != null ? env.hardwareConcurrency : (root.navigator && root.navigator.hardwareConcurrency) || 2;
    var size = Math.max(1, Math.min((hc | 0) - 1, 4));
    var createWorker = env.createWorker || defaultCreateWorker;
    var timeoutMs = env.timeoutMs || 30000;
    var workers = [], idle = [], queue = [], byKey = {}, seq = 0, fallback = false;
    var stats = { created: 0, fallbackRuns: 0, cancelled: 0, timeouts: 0, errors: 0 };

    function defaultCreateWorker() {
      var url = URL.createObjectURL(new Blob([WORKER_SRC], { type: 'text/javascript' }));
      var w = new Worker(url); URL.revokeObjectURL(url); return w;
    }
    function spawn() {
      try { var w = createWorker(); stats.created++; workers.push(w); return w; }
      catch (e) { fallback = true; return null; }   // CSP worker-src / no Worker: main thread
    }
    function runMain(task) {
      stats.fallbackRuns++;
      try { task.resolve(layoutKernel(task.n, task.edges, task.opts)); } catch (e) { task.reject(e); }
    }
    function pump() {
      while (queue.length) {
        var w = idle.pop() || (workers.length < size && !fallback ? spawn() : null);
        if (!w) { if (fallback) { runMain(queue.shift()); continue; } return; }
        dispatch(w, queue.shift());
      }
    }
    function finish(w, task, recycle) {
      clearTimeout(task.timer);
      if (byKey[task.key] === task) delete byKey[task.key];
      w.onmessage = w.onerror = null;
      if (recycle) idle.push(w); else { try { w.terminate(); } catch (e) {} workers.splice(workers.indexOf(w), 1); }
      pump();
    }
    function dispatch(w, task) {
      task.worker = w;
      w.onmessage = function (ev) {
        var d = ev.data; if (!d || d.id !== task.id) return;
        finish(w, task, true);
        if (d.error) { stats.errors++; task.reject(new Error(d.error)); } else task.resolve(d.pos);
      };
      w.onerror = function (ev) { stats.errors++; finish(w, task, false); task.reject(new Error((ev && ev.message) || 'worker error')); };
      task.timer = setTimeout(function () { stats.timeouts++; finish(w, task, false); task.reject(new Error('layout timeout')); }, timeoutMs);
      var edges = task.edges.slice();   // keep caller's copy; transfer ours
      try { w.postMessage({ id: task.id, n: task.n, edges: edges, opts: { iterations: task.opts.iterations, seed: task.opts.seed } }, [edges.buffer]); }
      catch (e) { finish(w, task, false); fallback = true; runMain(task); }
    }
    function cancel(task) {
      stats.cancelled++;
      var qi = queue.indexOf(task); if (qi >= 0) queue.splice(qi, 1);
      if (task.worker) finish(task.worker, task, false);   // terminate stale computation
      task.reject(Object.assign(new Error('cancelled'), { cancelled: true }));
    }
    function layout(n, edges, opts) {
      opts = opts || {};
      var key = opts.key || 'default';
      if (byKey[key]) cancel(byKey[key]);
      var task = { id: ++seq, key: key, n: n, edges: edges instanceof Int32Array ? edges : Int32Array.from(edges || []), opts: opts };
      var p = new Promise(function (res, rej) { task.resolve = res; task.reject = rej; });
      byKey[key] = task; queue.push(task); pump();
      return p;
    }
    return { layout: layout, size: size, stats: stats,
             workerCount: function () { return workers.length; },
             usingFallback: function () { return fallback; },
             destroy: function () { workers.forEach(function (w) { try { w.terminate(); } catch (e) {} }); workers = []; idle = []; } };
  }

  var api = { layoutKernel: layoutKernel, makePool: makePool, WORKER_SRC: WORKER_SRC };

  // ---- Shiny glue: server sends {key, ids, from, to, seed, iterations, el}; we set positions ----
  if (root.document) {
    var pool = null;
    function status(el, text) {
      var s = root.document.getElementById('cn_layout_status'); if (s) s.textContent = text || '';
    }
    function apply(msg) {
      try {
        pool = pool || makePool();
        var ids = msg.ids || [], idx = {}; ids.forEach(function (id, i) { idx[id] = i; });
        var from = msg.from || [], to = msg.to || [], e = new Int32Array(2 * from.length), m = 0;
        for (var i = 0; i < from.length; i++) { var a = idx[from[i]], b = idx[to[i]]; if (a != null && b != null) { e[2 * m] = a; e[2 * m + 1] = b; m++; } }
        status(msg.el, 'laying out… (' + (pool.usingFallback() ? 'main thread' : 'worker') + ')');
        var t0 = root.performance ? root.performance.now() : 0;
        pool.layout(ids.length, e.subarray(0, 2 * m), { key: msg.el || 'net', seed: msg.seed, iterations: msg.iterations }).then(function (pos) {
          var w = root.HTMLWidgets && root.HTMLWidgets.find('#' + (msg.el || 'network_filtered'));
          var net = w && w.network;
          if (net) {
            var upd = new Array(ids.length);
            for (var j = 0; j < ids.length; j++) upd[j] = { id: ids[j], x: pos[2 * j], y: pos[2 * j + 1] };
            net.setOptions({ physics: { enabled: false } });
            net.body.data.nodes.update(upd);
            net.fit({ animation: false });
          }
          if (root.Shiny && root.Shiny.setInputValue) {
            var xs = new Array(ids.length), ys = new Array(ids.length);
            for (var q = 0; q < ids.length; q++) { xs[q] = pos[2 * q]; ys[q] = pos[2 * q + 1]; }
            root.Shiny.setInputValue((msg.el || 'network_filtered').replace(/_filtered$/, '') + '_worker_positions', { ids: ids, x: xs, y: ys });
          }
          root.__cnLastLayout = { n: ids.length, ms: (root.performance ? root.performance.now() : 0) - t0, worker: !pool.usingFallback() };
          status(msg.el, '');
        }, function (err) {
          if (!(err && err.cancelled)) { console.warn('[CollabNET] worker layout failed; keeping server layout:', err && err.message); }
          status(msg.el, '');
        });
      } catch (err) { console.warn('[CollabNET] worker layout error:', err && err.message); status(msg.el, ''); }
    }
    function onLayoutMsg(msg) { apply(msg || {}); }
    api.apply = apply;
    var registered = false;
    function register() {
      if (registered || !root.Shiny || !root.Shiny.addCustomMessageHandler) return;
      try { root.Shiny.addCustomMessageHandler('collabnet_worker_layout', onLayoutMsg); registered = true; }
      catch (e) { console.error('[CollabNET] worker layout handler registration failed:', e); }
    }
    register();
    if (root.jQuery) root.jQuery(root.document).on('shiny:connected', function () { try { register(); } catch (e) {} });
  }

  root.CollabNETWorkerPool = api;
  if (typeof module !== 'undefined' && module.exports) module.exports = api;
})(typeof window !== 'undefined' ? window : globalThis);
