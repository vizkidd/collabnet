/* collabnet-mcp-ui.js: "Connect AI" panel for collabnet-webmcp.js.
 * textContent only; never innerHTML with data. Exposes window.CollabnetMCPUI.open()/close()/toggle(). */
(function (root) {
  'use strict';
  if (typeof document === 'undefined' || root.CollabnetMCPUI) return;
  var HTTP_URL = 'http://127.0.0.1:8765/mcp';
  var CONFIGS = [
    ['Claude Desktop / Cursor (npx)', JSON.stringify({ mcpServers: { collabnet: { command: 'npx', args: ['-y', 'collabnet-mcp', '--stdio'] } } }, null, 2)],
    ['Local checkout (node)', JSON.stringify({ mcpServers: { collabnet: { command: 'node', args: ['mcp/collabnet-mcp.mjs', '--stdio'] } } }, null, 2)],
    ['VS Code (.vscode/mcp.json)', JSON.stringify({ servers: { collabnet: { type: 'stdio', command: 'npx', args: ['-y', 'collabnet-mcp', '--stdio'] } } }, null, 2)],
    ['HTTP (Streamable HTTP)', JSON.stringify({ mcpServers: { collabnet: { type: 'http', url: HTTP_URL, headers: { Authorization: 'Bearer <TOKEN printed by the bridge>' } } } }, null, 2)]
  ];
  var BRIDGE_LABEL = { connected: 'Bridge connected', connecting: 'Connecting to bridge…', reconnecting: 'Bridge reconnecting', disconnected: 'Bridge not connected', rejected: 'Bridge rejected pairing', error: 'Bridge error' };

  function el(tag, attrs, text) {
    var e = document.createElement(tag);
    Object.keys(attrs || {}).forEach(function (k) { if (k === 'class') e.className = attrs[k]; else e.setAttribute(k, attrs[k]); });
    if (text != null) e.textContent = String(text);
    return e;
  }
  function add(parent) { for (var i = 1; i < arguments.length; i++) if (arguments[i]) parent.appendChild(arguments[i]); return parent; }
  function hhmm(t) { var d = t ? new Date(t) : new Date(); return d.toTimeString().slice(0, 8); }

  // Accepts a one-click link (...#mcp=<ws>&token=<t>), "ws://... token", or "ws://...?token=t".
  function parsePairing(text) {
    var s = String(text || '').trim(), C = root.CollabnetMCP;
    if (!s) return null;
    var i = s.indexOf('#');
    if (i >= 0 && /mcp=/.test(s.slice(i))) { var f = C.parseFragment(s.slice(i)); return f && { url: f.url, token: f.token }; }
    var parts = s.split(/\s+/), url = parts[0], token = parts[1] || '';
    try { var u = new URL(url); if (!token && u.searchParams.get('token')) { token = u.searchParams.get('token'); u.search = ''; url = u.toString().replace(/\/page\/?$/, ''); } } catch (e) { return null; }
    return { url: url, token: token };
  }

  function build(core) {
    var fab = el('button', { type: 'button', id: 'collabnet-mcp-fab', class: 'cmcp-fab', 'aria-haspopup': 'dialog', 'aria-controls': 'collabnet-mcp-dock' }, 'Connect AI');
    var dot = el('span', { class: 'cmcp-dot', 'aria-hidden': 'true' }); fab.prepend(dot);
    var panel = el('section', { id: 'collabnet-mcp-dock', class: 'cmcp-panel', role: 'dialog', 'aria-label': 'Connect AI to CollabNET', hidden: '' });
    var head = add(el('div', { class: 'cmcp-head' }), el('strong', null, 'Connect AI'));
    var closeBtn = add(head, el('button', { type: 'button', class: 'cmcp-x', 'aria-label': 'Close' }, '×')).lastChild;
    panel.appendChild(head);

    // status
    var st = { access: el('span'), bridge: el('span'), clients: el('span'), webmcp: el('span'), policy: el('span'), hint: el('span', { class: 'cmcp-hint' }) };
    var stList = el('ul', { class: 'cmcp-status', 'aria-live': 'polite' });
    Object.keys(st).forEach(function (k) { add(stList, add(el('li'), st[k])); });
    var accessRow = el('label', { class: 'cmcp-row' });
    var accessBox = el('input', { type: 'checkbox', id: 'collabnet-mcp-access-mirror' });
    add(accessRow, accessBox, el('span', null, ' AI access to this session\'s data (same switch as the page checkbox)'));
    add(panel, el('h4', null, 'Status'), stList, accessRow);

    // pairing
    var pairIn = el('input', { type: 'text', id: 'collabnet-mcp-pair', placeholder: 'Paste one-click link, or ws://127.0.0.1:PORT TOKEN', autocomplete: 'off', spellcheck: 'false', 'aria-label': 'Bridge pairing link' });
    var conBtn = el('button', { type: 'button', class: 'btn btn-primary btn-sm' }, 'Connect');
    var disBtn = el('button', { type: 'button', class: 'btn btn-default btn-sm' }, 'Disconnect');
    var pairMsg = el('div', { class: 'cmcp-note', 'aria-live': 'polite' });
    add(panel, el('h4', null, 'Local bridge (desktop AI apps)'), pairIn, add(el('div', { class: 'cmcp-actions' }), conBtn, disBtn), pairMsg);

    // consent
    var pol = el('select', { id: 'collabnet-mcp-policy', 'aria-label': 'Policy for tools that change the app' });
    [['ask', 'Ask me every time'], ['read_only', 'Read-only (deny changes)'], ['allow_session', 'Allow changes for 30 min']].forEach(function (o) { add(pol, el('option', { value: o[0] }, o[1])); });
    var killBtn = el('button', { type: 'button', class: 'btn btn-danger btn-sm cmcp-kill' }, 'Kill switch: stop all AI access');
    add(panel, el('h4', null, 'Changes made by AI'), add(el('div', { class: 'cmcp-actions' }), pol, killBtn),
      el('div', { class: 'cmcp-note' }, 'Read-only tools never prompt. Tools that change filters or run code follow this policy; R also checks the AI access switch.'));

    // log
    var log = el('ol', { class: 'cmcp-log', 'aria-label': 'AI activity log' });
    add(panel, el('h4', null, 'Activity'), log);

    // configs
    var cfg = el('details', { class: 'cmcp-configs' });
    add(cfg, el('summary', null, 'Client configuration (copy & paste)'),
      el('div', { class: 'cmcp-note' }, 'Start the bridge (npx -y collabnet-mcp), then click its one-click link or paste it above. HTTP endpoint: ' + HTTP_URL + ' with header Authorization: Bearer <token>.'));
    CONFIGS.forEach(function (c) {
      var pre = el('pre', { class: 'cmcp-pre', tabindex: '0' }, c[1]);
      var copy = el('button', { type: 'button', class: 'btn btn-default btn-xs' }, 'Copy');
      copy.addEventListener('click', function () {
        var done = function () { copy.textContent = 'Copied'; setTimeout(function () { copy.textContent = 'Copy'; }, 1500); };
        if (navigator.clipboard && navigator.clipboard.writeText) navigator.clipboard.writeText(c[1]).then(done, function () {});
      });
      add(cfg, add(el('div', { class: 'cmcp-cfg-head' }), el('span', null, c[0]), copy), pre);
    });
    panel.appendChild(cfg);

    // consent modal
    var modal = el('div', { class: 'cmcp-modal', role: 'alertdialog', 'aria-modal': 'true', 'aria-labelledby': 'cmcp-modal-title', hidden: '' });
    var box = el('div', { class: 'cmcp-modal-box' });
    var mTitle = el('h4', { id: 'cmcp-modal-title' }), mBody = el('div', { class: 'cmcp-note' }), mArgs = el('pre', { class: 'cmcp-pre' });
    var allowBtn = el('button', { type: 'button', class: 'btn btn-primary' }, 'Allow'), denyBtn = el('button', { type: 'button', class: 'btn btn-default' }, 'Deny');
    add(modal, add(box, mTitle, mBody, mArgs, add(el('div', { class: 'cmcp-actions' }), allowBtn, denyBtn)));

    add(document.body, fab, panel, modal);

    /* behaviour */
    function open() { panel.hidden = false; fab.setAttribute('aria-expanded', 'true'); render(); }
    function close() { panel.hidden = true; fab.setAttribute('aria-expanded', 'false'); }
    fab.addEventListener('click', function () { if (panel.hidden) open(); else close(); });
    closeBtn.addEventListener('click', close);
    document.addEventListener('click', function (e) {
      if (e.target && e.target.closest && e.target.closest('#connect_ai_btn')) { e.preventDefault(); open(); }
    });

    accessBox.addEventListener('change', function () { core.setEnabled(accessBox.checked); });
    pol.addEventListener('change', function () { core.setPolicy(pol.value); });
    killBtn.addEventListener('click', function () { if (core.snapshot().killed) core.resume(); else core.kill(); render(); });
    conBtn.addEventListener('click', function () {
      var p = parsePairing(pairIn.value);
      if (!p) { pairMsg.textContent = 'Could not read that link.'; return; }
      try { core.connect(p.url, p.token); pairIn.value = ''; pairMsg.textContent = 'Connecting to ' + p.url + '…'; }
      catch (e) { pairMsg.textContent = e.message; }
    });
    disBtn.addEventListener('click', function () { core.disconnect(); pairMsg.textContent = ''; });
    var last = core.lastUrl && core.lastUrl();
    if (last) pairMsg.textContent = 'Last bridge: ' + last + ' (paste the link again to reconnect; tokens are not stored).';

    function render() {
      var s = core.snapshot(), b = s.status;
      st.access.textContent = 'AI access: ' + (s.killed ? 'STOPPED (kill switch)' : s.enabled ? 'ON' : 'OFF') + ' · ' + s.toolCount + ' tools';
      st.bridge.textContent = (BRIDGE_LABEL[b.bridge] || b.bridge) + (b.detail && !b.lna ? ' (' + b.detail + ')' : b.lna && b.nextDelay ? ' (retrying)' : '');
      var hints = (root.CollabnetMCP && root.CollabnetMCP.bridgeHints) ? root.CollabnetMCP.bridgeHints(b) : [];
      st.hint.textContent = hints.join(' '); st.hint.parentNode.style.display = hints.length ? '' : 'none';
      st.clients.textContent = 'Agents connected via bridge: ' + (b.bridge === 'connected' ? (b.clients == null ? 'waiting for bridge…' : String(b.clients)) : 0);
      st.webmcp.textContent = 'WebMCP: ' + (b.webmcp ? 'registered' : b.webmcpAvailable ? 'available, not registered' : 'not available in this browser');
      st.policy.textContent = 'Policy: ' + pol.querySelector('option[value="' + s.policy + '"]').textContent +
        (s.policy === 'allow_session' ? ' (until ' + hhmm(s.expires) + ')' : '');
      pol.value = s.policy; accessBox.checked = s.enabled; accessBox.disabled = s.killed;
      killBtn.textContent = s.killed ? 'Resume AI access' : 'Kill switch: stop all AI access';
      fab.classList.toggle('is-on', s.enabled && !s.killed);
      fab.classList.toggle('is-linked', b.bridge === 'connected' || b.webmcp);
      // legacy status widgets in the page panel, if present
      var led = document.getElementById('collabnet-mcp-led'), txt = document.getElementById('collabnet-mcp-status-text'), cnt = document.getElementById('collabnet-mcp-connection-count');
      if (led) { led.classList.toggle('is-connected', b.bridge === 'connected' || b.webmcp); led.classList.toggle('is-disconnected', !(b.bridge === 'connected' || b.webmcp)); }
      if (txt) txt.textContent = b.bridge === 'connected' ? 'Bridge connected' : b.webmcp ? 'WebMCP registered' : 'In-page only';
      if (cnt) cnt.textContent = (b.bridge === 'connected' ? 1 : 0) + (b.webmcp ? 1 : 0) + ' external transports';
    }
    core.on('state', render); core.on('status', render);
    setInterval(function () { if (!panel.hidden) render(); }, 15000);

    core.on('audit', function (ev) {
      var li = el('li', { class: ev.decision === 'allowed' ? 'is-ok' : 'is-deny' },
        hhmm(ev.time) + '  ' + ev.client + ' [' + ev.transport + ']  ' + ev.tool + '  ' + ev.decision + (ev.consent === 'user' ? ' (by you)' : '') +
        (ev.ms != null ? '  ' + ev.ms + ' ms' : '') + (ev.reason ? '  — ' + ev.reason : ''));
      log.prepend(li);
      while (log.children.length > 100) log.removeChild(log.lastChild);
    });

    // Allow/Deny prompts, queued one at a time.
    var queue = [], current = null;
    function showNext() {
      if (current || !queue.length) return;
      current = queue.shift();
      var q = current.info;
      mTitle.textContent = 'Allow ' + q.client + ' to run "' + q.title + '"?';
      mBody.textContent = 'This tool can change the app state (via ' + q.transport + '). It is denied automatically in ' + Math.round(q.timeoutMs / 1000) + ' s.';
      var a = JSON.stringify(q.args || {}, null, 2); mArgs.textContent = a.length > 2000 ? a.slice(0, 2000) + '…' : a;
      modal.hidden = false; denyBtn.focus();
      current.timer = setTimeout(function () { answer(false); }, q.timeoutMs);
    }
    function answer(ok) { if (!current) return; clearTimeout(current.timer); modal.hidden = true; var c = current; current = null; c.resolve(ok); showNext(); }
    allowBtn.addEventListener('click', function () { answer(true); });
    denyBtn.addEventListener('click', function () { answer(false); });
    modal.addEventListener('keydown', function (e) { if (e.key === 'Escape') answer(false); });
    core.on('state', function (s) { if (s.killed) { queue.splice(0).forEach(function (q) { q.resolve(false); }); answer(false); } });
    core.setPromptHandler(function (info) { return new Promise(function (resolve) { queue.push({ info: info, resolve: resolve }); showNext(); }); });

    render();
    return { open: open, close: close, toggle: function () { if (panel.hidden) open(); else close(); }, parsePairing: parsePairing };
  }

  function start() {
    var C = root.CollabnetMCP;
    if (!C || !C.instance) return;
    var ui = build(C.instance);
    root.CollabnetMCPUI.open = ui.open; root.CollabnetMCPUI.close = ui.close; root.CollabnetMCPUI.toggle = ui.toggle;
  }
  root.CollabnetMCPUI = { open: function () {}, close: function () {}, toggle: function () {}, parsePairing: parsePairing };
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start); else start();
})(typeof globalThis !== 'undefined' ? globalThis : this);
