/*
 * collabnet-keystore.js  (P0-8b)
 *
 * API keys are kept IN MEMORY ONLY by default (they live in the R session of
 * the in-browser webR process and are gone on reload). Nothing is written to
 * localStorage unless the user ticks "Remember on this device" AND supplies a
 * passphrase. In that case the key bundle is encrypted with WebCrypto
 * AES-GCM-256 using a key derived by PBKDF2-SHA-256 (250 000 iterations,
 * random 16-byte salt, random 12-byte IV) and stored as
 *   localStorage["collabnet_keystore_v2"] = {"v":2,"salt":b64,"iv":b64,"ct":b64}
 *
 * Legacy versions stored keys in PLAINTEXT under these localStorage names:
 *   scopus_key, openalex_key, mail_key, wos_key, semantic_key, crossref_key,
 *   opencites_key
 * They are deleted on load and the user is notified.
 *
 * Works in browsers and in Node >= 20 (globalThis.crypto.subtle) for tests.
 */
(function (root) {
  'use strict';

  var STORE_NAME = 'collabnet_keystore_v2';
  var PBKDF2_ITERATIONS = 250000;
  var LEGACY_PLAINTEXT_KEYS = ['scopus_key', 'openalex_key', 'mail_key', 'wos_key',
    'semantic_key', 'crossref_key', 'opencites_key'];

  function subtle() {
    var c = (root && root.crypto) || globalThis.crypto;
    if (!c || !c.subtle) throw new Error('WebCrypto not available');
    return c;
  }
  function b64(bytes) {
    var s = '';
    var a = new Uint8Array(bytes);
    for (var i = 0; i < a.length; i++) s += String.fromCharCode(a[i]);
    return btoa(s);
  }
  function unb64(str) {
    var s = atob(str);
    var a = new Uint8Array(s.length);
    for (var i = 0; i < s.length; i++) a[i] = s.charCodeAt(i);
    return a;
  }

  async function deriveKey(passphrase, salt, iterations) {
    var c = subtle();
    var base = await c.subtle.importKey('raw', new TextEncoder().encode(passphrase), 'PBKDF2', false, ['deriveKey']);
    return c.subtle.deriveKey({ name: 'PBKDF2', hash: 'SHA-256', salt: salt, iterations: iterations || PBKDF2_ITERATIONS },
      base, { name: 'AES-GCM', length: 256 }, false, ['encrypt', 'decrypt']);
  }

  /** Encrypt an object of keys -> JSON string {v,salt,iv,ct}. */
  async function encryptKeys(keys, passphrase) {
    if (!passphrase || passphrase.length < 8) throw new Error('Passphrase must be at least 8 characters');
    var c = subtle();
    var salt = c.getRandomValues(new Uint8Array(16));
    var iv = c.getRandomValues(new Uint8Array(12));
    var key = await deriveKey(passphrase, salt);
    var pt = new TextEncoder().encode(JSON.stringify(keys || {}));
    var ct = await c.subtle.encrypt({ name: 'AES-GCM', iv: iv }, key, pt);
    return JSON.stringify({ v: 2, kdf: 'PBKDF2-SHA256', iter: PBKDF2_ITERATIONS, salt: b64(salt), iv: b64(iv), ct: b64(ct) });
  }

  /** Decrypt JSON string -> keys object. Throws on wrong passphrase / tampering. */
  async function decryptKeys(blob, passphrase) {
    var o = typeof blob === 'string' ? JSON.parse(blob) : blob;
    if (!o || o.v !== 2 || !o.salt || !o.iv || !o.ct) throw new Error('Unsupported keystore format');
    var key = await deriveKey(passphrase, unb64(o.salt), o.iter || PBKDF2_ITERATIONS);
    var pt;
    try {
      pt = await subtle().subtle.decrypt({ name: 'AES-GCM', iv: unb64(o.iv) }, key, unb64(o.ct));
    } catch (e) {
      throw new Error('Wrong passphrase or corrupted keystore');
    }
    return JSON.parse(new TextDecoder().decode(pt));
  }

  /** Remove legacy plaintext keys. Returns the list of removed names. */
  function cleanupLegacy(storage) {
    var removed = [];
    if (!storage) return removed;
    LEGACY_PLAINTEXT_KEYS.forEach(function (k) {
      var v = storage.getItem(k);
      if (v !== null && v !== undefined) { storage.removeItem(k); removed.push(k); }
    });
    return removed;
  }

  async function saveEncrypted(storage, keys, passphrase) {
    storage.setItem(STORE_NAME, await encryptKeys(keys, passphrase));
  }
  async function loadEncrypted(storage, passphrase) {
    var blob = storage.getItem(STORE_NAME);
    if (!blob) return null;
    return decryptKeys(blob, passphrase);
  }
  function hasStore(storage) { return !!(storage && storage.getItem(STORE_NAME)); }
  function forget(storage) { if (storage) storage.removeItem(STORE_NAME); }

  var api = {
    STORE_NAME: STORE_NAME, PBKDF2_ITERATIONS: PBKDF2_ITERATIONS, LEGACY_PLAINTEXT_KEYS: LEGACY_PLAINTEXT_KEYS,
    encryptKeys: encryptKeys, decryptKeys: decryptKeys, cleanupLegacy: cleanupLegacy,
    saveEncrypted: saveEncrypted, loadEncrypted: loadEncrypted, hasStore: hasStore, forget: forget
  };
  root.CollabnetKeystore = api;
  if (typeof module !== 'undefined' && module.exports) module.exports = api;

  // ------------------------------------------------------------------------
  // Browser / Shiny wiring
  // ------------------------------------------------------------------------
  if (typeof document === 'undefined' || typeof window === 'undefined') return;
  var ls = null;
  try { ls = window.localStorage; } catch (e) { ls = null; }
  var sessionKeys = {};   // in-memory copy, only used to re-encrypt the bundle

  function notify(text, type) {
    if (window.Shiny && Shiny.notifications && Shiny.notifications.show) {
      Shiny.notifications.show({ html: $('<span>').text(text).html(), type: type || 'message', duration: 10000 });
    } else if (window.console) { console.log(text); }
  }

  // C25_r12: styles for the unlock bar (kept in this file so every edition gets them without a new stylesheet)
  function injectUnlockStyle() {
    if (document.getElementById('cn-keystore-unlock-style')) return;
    var st = document.createElement('style'); st.id = 'cn-keystore-unlock-style';
    st.textContent =
      '#cn-keystore-unlock{background:#fff;color:#1f2937;border:1px solid #ccc;border-radius:6px;padding:4px 8px;font-size:13px;' +
        'display:flex;flex-wrap:wrap;align-items:center;gap:6px;box-sizing:border-box;max-width:100%;min-width:0}' +
      '#cn-keystore-unlock label{margin:0;font-weight:normal;flex:1 1 12em;min-width:0;white-space:normal}' +
      '#cn-keystore-unlock input{width:140px;max-width:100%;flex:0 1 140px;min-width:80px;color:#1f2937}' +
      '#cn-keystore-unlock .btn{min-height:30px}' +
      '#cn-keystore-unlock.cn-ks-inbar{position:static;flex:0 1 auto;order:99}' +
      '#cn-keystore-unlock.cn-ks-fixed{position:fixed;top:8px;right:8px;z-index:10050;box-shadow:0 2px 8px rgba(0,0,0,.2)}' +
      '@media (max-width:767.98px){#cn-keystore-unlock.cn-ks-inbar{flex:1 1 100%}}' +
      'body.dark-mode #cn-keystore-unlock{background:#1e293b;color:#f1f5f9;border-color:#475569}' +
      'body.dark-mode #cn-keystore-unlock .cn-ks-dismiss{color:#f1f5f9}';
    (document.head || document.documentElement).appendChild(st);
  }

  function showUnlockBar() {
    if (document.getElementById('cn-keystore-unlock')) return;
    var bar = document.createElement('div');
    bar.id = 'cn-keystore-unlock';
    bar.setAttribute('role', 'group'); bar.setAttribute('aria-label', 'Unlock saved API keys');
    // C25_r12: the bar is the LAST item of the top bar's button row (.header-buttons, a wrapping flex row), not a
    // position:fixed layer over it, so it keeps its place at the top-right but can never cover the Close / reset
    // button or any other top-bar control: the row makes room, and at narrow widths it wraps. Fallback (no top bar,
    // e.g. a test page): the old fixed corner.
    injectUnlockStyle();
    var row = document.querySelector('.header-container .header-buttons');
    bar.className = row ? 'cn-ks-inbar' : 'cn-ks-fixed';
    var label = document.createElement('label');
    label.textContent = 'Encrypted API keys saved on this device. Passphrase:';
    var pass = document.createElement('input'); pass.type = 'password'; pass.id = 'cn-keystore-pass-unlock'; pass.autocomplete = 'current-password';
    label.htmlFor = pass.id;
    var unlock = document.createElement('button'); unlock.type = 'button'; unlock.textContent = 'Unlock'; unlock.className = 'btn btn-xs btn-success';
    var forgetBtn = document.createElement('button'); forgetBtn.type = 'button'; forgetBtn.textContent = 'Forget'; forgetBtn.className = 'btn btn-xs btn-default';
    var close = document.createElement('button'); close.type = 'button'; close.textContent = '×'; close.className = 'btn btn-xs btn-link cn-ks-dismiss';
    close.setAttribute('aria-label', 'Hide the unlock bar'); close.title = 'Hide the unlock bar';
    bar.appendChild(label); bar.appendChild(pass); bar.appendChild(unlock); bar.appendChild(forgetBtn); bar.appendChild(close);
    if (row) row.appendChild(bar); else document.body.appendChild(bar);
    pass.addEventListener('keydown', function (e) { if (e.key === 'Enter') { e.preventDefault(); unlock.click(); } });
    unlock.addEventListener('click', async function () {
      try {
        var keys = await loadEncrypted(ls, pass.value);
        sessionKeys = keys || {};
        Shiny.setInputValue('browser_stored_keys', sessionKeys, { priority: 'event' });
        bar.remove();
        notify('API keys unlocked for this session.', 'message');
      } catch (e) { notify(e.message, 'error'); }
    });
    forgetBtn.addEventListener('click', function () { forget(ls); bar.remove(); notify('Saved encrypted keys removed from this device.', 'warning'); });
    close.addEventListener('click', function () { bar.remove(); });
  }

  // R -> JS handlers. C1-1: Shiny.addCustomMessageHandler throws unless handler.length === 1, and a throw inside a
  // 'shiny:connected' listener aborts Shiny's socket.onopen BEFORE it sends the "init" message, i.e. the whole app
  // (every input, the R console, MCP) silently stops working. Every handler therefore declares exactly one
  // parameter, registration happens once (guard flag; shiny:connected fires again on reconnect), and all setup
  // is wrapped in try/catch so a failure here can never break other scripts.
  function onSaveKey(msg) {
    if (!msg || !msg.platform) return;
    if (msg.key) sessionKeys[msg.platform] = msg.key; else delete sessionKeys[msg.platform];
    var remember = document.getElementById('cn_remember_keys');
    var passEl = document.querySelector('.cn-keystore-pass');
    if (!remember || !remember.checked) { return; }   // default: in-memory only
    saveEncrypted(ls, sessionKeys, passEl ? passEl.value : '').then(function () {
      notify('Key encrypted (AES-GCM, passphrase-derived) and saved on this device.', 'message');
    }, function (e) { notify('Key NOT saved on this device: ' + e.message, 'error'); });
  }
  function onForgetKeys(msg) { forget(ls); sessionKeys = {}; }   // eslint-disable-line no-unused-vars
  var handlersRegistered = false;
  function registerHandlers() {
    if (handlersRegistered) return true;
    var S = root.Shiny;
    if (!S || typeof S.addCustomMessageHandler !== 'function') return false;
    try {
      S.addCustomMessageHandler('save_key_to_browser', onSaveKey);
      S.addCustomMessageHandler('forget_browser_keys', onForgetKeys);
      handlersRegistered = true;
    } catch (e) { if (root.console) root.console.error('collabnet-keystore: handler registration failed', e); }
    return handlersRegistered;
  }
  var connectedOnce = false;
  function onConnected() {
    try {
      registerHandlers();
      if (connectedOnce) return;          // reconnect: legacy cleanup / unlock bar only once
      connectedOnce = true;
      var removed = cleanupLegacy(ls);
      if (removed.length) {
        notify('Security notice: an older CollabNET version stored API keys UNENCRYPTED in this browser (' +
          removed.join(', ') + '). They have been deleted. Please re-enter your keys.', 'warning');
      }
      if (hasStore(ls)) showUnlockBar();
    } catch (e) { if (root.console) root.console.error('collabnet-keystore: shiny:connected setup failed', e); }
  }
  api.showUnlockBar = function () { if (hasStore(ls)) showUnlockBar(); };   // C25_r12: tests (no reload needed)
  registerHandlers();   // as early as possible (Shiny's API exists before the socket opens)
  if (typeof $ === 'function') $(document).on('shiny:connected', onConnected);
})(typeof window !== 'undefined' ? window : globalThis);
