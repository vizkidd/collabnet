/*
 * collabnet-reset.js  (C25_r11)
 *
 * The "Close / reset" button in the top bar opens a small dialog with three actions:
 *   Soft reset  -- clear the loaded dataset and everything derived from it, in place (no reload): R resets its
 *                  reactive data state (input cn_reset -> server.R cn_reset_data()), the page clears the selection,
 *                  and other scripts listen for the "cn:reset" DOM event (e.g. the Full plot pane). Options stay.
 *   Hard reset  -- clear the app's own stored preferences, then reload: every Shiny input is back at its default and
 *                  the R session (webR) starts empty.
 *   Reload page -- location.reload(), nothing removed.
 * "Also remove stored keys" (default off) adds the stored API keys (soft + hard) and tells R to forget the keys it
 * holds in memory (soft).
 *
 * Never touched: cookies (sign-in session, Full pass/entitlement, checkout), the service worker and its caches
 * (Cache Storage / webR packages), the site theme (collabnet.theme belongs to the site pages, not the app).
 *
 * The pure part (plan / applyPlan) runs in Node for tests (tests/reset_c25r11.test.mjs).
 */
(function (root) {
  'use strict';

  // Everything the app keeps in browser storage (see deploy_static/privacy/index.html, "Stored in your browser").
  var KEY_ITEMS = {
    local: ['collabnet_keystore_v2', 'scopus_key', 'openalex_key', 'mail_key', 'wos_key', 'semantic_key',
      'crossref_key', 'opencites_key'],
    session: ['collabnet_mcp_ws_url']   // Full only: the local MCP bridge address (no token is ever stored)
  };
  // App preferences persisted in the browser. Today the app persists none (all options are Shiny inputs, reset by
  // the reload); the list exists so a future persisted option is cleared by Hard reset by adding its name here.
  var PREF_ITEMS = { local: [], session: [] };
  // Items that must survive every reset, even if a future list above names them by mistake.
  var NEVER = ['collabnet.theme'];

  function plan(mode, removeKeys) {
    if (mode !== 'soft' && mode !== 'hard' && mode !== 'reload') throw new Error('unknown reset mode ' + mode);
    var local = [], session = [];
    if (mode === 'hard') { local = local.concat(PREF_ITEMS.local); session = session.concat(PREF_ITEMS.session); }
    if (removeKeys && mode !== 'reload') { local = local.concat(KEY_ITEMS.local); session = session.concat(KEY_ITEMS.session); }
    var keep = function (k) { return NEVER.indexOf(k) < 0; };
    return {
      mode: mode,
      removeLocal: local.filter(keep),
      removeSession: session.filter(keep),
      serverReset: mode === 'soft',                 // in place, through Shiny
      forgetServerKeys: mode === 'soft' && !!removeKeys,
      reload: mode !== 'soft'
    };
  }
  function applyPlan(p, storages) {
    var removed = [];
    [['local', p.removeLocal], ['session', p.removeSession]].forEach(function (pair) {
      var s = storages && storages[pair[0]];
      if (!s) return;
      pair[1].forEach(function (k) {
        try { if (s.getItem(k) !== null) { s.removeItem(k); removed.push(pair[0] + ':' + k); } } catch (e) { /* storage blocked */ }
      });
    });
    return removed;
  }
  var api = { KEY_ITEMS: KEY_ITEMS, PREF_ITEMS: PREF_ITEMS, NEVER: NEVER, plan: plan, applyPlan: applyPlan };
  if (typeof module === 'object' && module.exports) module.exports = api;
  root.CollabnetReset = api;

  // ---------------------------------------------------------------- browser UI
  var doc = root.document;
  if (!doc || !doc.addEventListener) return;
  var dlg = null, opener = null;

  function storages() {
    var o = {};
    try { o.local = root.localStorage; } catch (e) { o.local = null; }
    try { o.session = root.sessionStorage; } catch (e) { o.session = null; }
    return o;
  }
  function el(tag, attrs, text) {
    var e = doc.createElement(tag);
    Object.keys(attrs || {}).forEach(function (k) { e.setAttribute(k, attrs[k]); });
    if (text) e.textContent = text;
    return e;
  }
  function toast(text) {
    var t = doc.getElementById('cn-reset-toast');
    if (!t) { t = el('div', { id: 'cn-reset-toast', role: 'status', 'aria-live': 'polite' }); doc.body.appendChild(t); }
    t.textContent = text; t.classList.add('is-on');
    clearTimeout(toast.timer); toast.timer = setTimeout(function () { t.classList.remove('is-on'); }, 4000);
  }
  function build() {
    var back = el('div', { id: 'cn-reset-backdrop', class: 'cn-reset-backdrop', hidden: '' });
    var box = el('div', { id: 'cn-reset-dialog', class: 'cn-reset-dialog', role: 'dialog', 'aria-modal': 'true',
      'aria-labelledby': 'cn-reset-title', 'aria-describedby': 'cn-reset-desc' });
    box.appendChild(el('h2', { id: 'cn-reset-title' }, 'Close / reset'));
    box.appendChild(el('p', { id: 'cn-reset-desc' }, 'Choose what to clear. Your sign-in and any Full pass are never affected.'));
    var rows = [
      ['soft', 'btn btn-primary', 'Soft reset', 'Clear the loaded data and all results (network, metrics, tables, charts, plots, selection); keep your options.'],
      ['hard', 'btn btn-danger', 'Hard reset', 'Clear the data and restore every option to its default, then reload the app.'],
      ['reload', 'btn btn-default', 'Reload page', 'Reload the page; nothing is removed.']
    ];
    rows.forEach(function (r) {
      var row = el('div', { class: 'cn-reset-row' });
      var b = el('button', { type: 'button', class: r[1] + ' cn-reset-action', 'data-mode': r[0], id: 'cn-reset-' + r[0],
        'aria-describedby': 'cn-reset-' + r[0] + '-desc' }, r[2]);
      row.appendChild(b); row.appendChild(el('span', { id: 'cn-reset-' + r[0] + '-desc', class: 'cn-reset-explain' }, r[3]));
      box.appendChild(row);
    });
    var lab = el('label', { class: 'cn-reset-keys' });
    var chk = el('input', { type: 'checkbox', id: 'cn-reset-keys' });
    lab.appendChild(chk); lab.appendChild(doc.createTextNode(' Also remove stored keys (API keys saved on this device'
      + (root.CollabnetMCP ? ', the AI bridge address' : '') + '); applies to Soft and Hard reset'));
    box.appendChild(lab);
    var cancel = el('button', { type: 'button', class: 'btn btn-default cn-reset-cancel', id: 'cn-reset-cancel' }, 'Cancel');
    var foot = el('div', { class: 'cn-reset-foot' }); foot.appendChild(cancel); box.appendChild(foot);
    back.appendChild(box); doc.body.appendChild(back);
    back.addEventListener('mousedown', function (ev) { if (ev.target === back) close(); });
    cancel.addEventListener('click', close);
    box.addEventListener('click', function (ev) {
      var b = ev.target.closest && ev.target.closest('.cn-reset-action');
      if (b) run(b.getAttribute('data-mode'), chk.checked);
    });
    box.addEventListener('keydown', function (ev) {
      if (ev.key === 'Escape') { ev.preventDefault(); close(); return; }
      if (ev.key !== 'Tab') return;                 // focus trap: cycle inside the dialog
      var f = Array.prototype.filter.call(box.querySelectorAll('button, input'), function (x) { return !x.disabled; });
      var first = f[0], last = f[f.length - 1];
      if (ev.shiftKey && doc.activeElement === first) { ev.preventDefault(); last.focus(); }
      else if (!ev.shiftKey && doc.activeElement === last) { ev.preventDefault(); first.focus(); }
    });
    return back;
  }
  function open(ev) {
    opener = (ev && ev.currentTarget) || doc.getElementById('cn_reset_btn');
    dlg = dlg || build();
    dlg.hidden = false; doc.body.classList.add('cn-reset-open');
    var k = doc.getElementById('cn-reset-keys'); if (k) k.checked = false;   // default OFF every time
    doc.getElementById('cn-reset-soft').focus();
  }
  function close() {
    if (!dlg || dlg.hidden) return;
    dlg.hidden = true; doc.body.classList.remove('cn-reset-open');
    if (opener && opener.focus) opener.focus();
  }
  function run(mode, removeKeys) {
    var p = plan(mode, removeKeys);
    var removed = applyPlan(p, storages());
    if (p.reload) {
      try { root.sessionStorage.setItem('cn_reset_notice', mode === 'hard' ? 'Hard reset done: options restored to defaults' + (removeKeys ? ', stored keys removed.' : '.') : ''); } catch (e) { /* ignore */ }
      close(); root.location.reload(); return;
    }
    var S = root.Shiny;
    if (S && S.setInputValue) S.setInputValue('cn_reset', { mode: 'soft', keys: !!removeKeys, n: Date.now() }, { priority: 'event' });
    var t = doc.getElementById('author_list_text');
    if (t) { t.value = ''; if (S && S.setInputValue) S.setInputValue('author_list', '', { priority: 'event' }); }
    try { doc.dispatchEvent(new root.CustomEvent('cn:reset', { detail: { mode: 'soft', keys: !!removeKeys, removed: removed } })); } catch (e) { /* old browser */ }
    if (root.CollabnetSpotlight && root.CollabnetSpotlight.reset) root.CollabnetSpotlight.reset();
    close();
    toast('Soft reset done: data cleared, options kept' + (removeKeys ? ', stored keys removed.' : '.'));
  }
  function init() {
    var b = doc.getElementById('cn_reset_btn');
    if (b && !b.__cnReset) { b.__cnReset = true; b.addEventListener('click', open); }
    try {
      var n = root.sessionStorage.getItem('cn_reset_notice');
      if (n !== null) { root.sessionStorage.removeItem('cn_reset_notice'); if (n) toast(n); }
    } catch (e) { /* ignore */ }
  }
  api.open = open; api.close = close; api.run = run;
  if (doc.readyState === 'loading') doc.addEventListener('DOMContentLoaded', init); else init();
})(typeof window !== 'undefined' ? window : globalThis);
