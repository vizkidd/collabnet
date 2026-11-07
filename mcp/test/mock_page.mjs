// mock_page.mjs: a fake CollabNET page for bridge tests. It connects to the bridge WebSocket (Node >= 22
// global WebSocket) and acts as the MCP server: tools/list (2 tools, plus collabnet_r_eval when enabled),
// tools/call (echo, with a small random delay so responses come back out of order), resources/list,
// and it emits notifications/tools/list_changed when r_eval is toggled.
const BASE_TOOLS = [
  { name: 'collabnet_get_status', title: 'Status', description: 'Echo status', inputSchema: { type: 'object', properties: {}, additionalProperties: true },
    annotations: { readOnlyHint: true, destructiveHint: false, idempotentHint: true, openWorldHint: false } },
  { name: 'collabnet_echo', title: 'Echo', description: 'Echo the arguments', inputSchema: { type: 'object', properties: {}, additionalProperties: true },
    annotations: { readOnlyHint: true, destructiveHint: false, idempotentHint: true, openWorldHint: false } },
];
const R_EVAL = { name: 'collabnet_r_eval', title: 'Evaluate R', description: 'Debug only', inputSchema: { type: 'object', properties: { code: { type: 'string' } }, required: ['code'], additionalProperties: false },
  annotations: { readOnlyHint: false, destructiveHint: true, idempotentHint: false, openWorldHint: false } };

export function connectMockPage(wsUrl, { origin = 'http://localhost:8000', jitterMs = 20 } = {}) {
  let rEval = false;
  const seen = []; // raw requests received (to check id rewriting)
  const notes = []; // notifications from the bridge (e.g. notifications/collabnet/clients)
  const ws = new WebSocket(wsUrl, { headers: { Origin: origin } });
  const send = (o) => ws.send(JSON.stringify(o));
  ws.addEventListener('message', (ev) => {
    const m = JSON.parse(ev.data);
    if (m.id === undefined) { notes.push(m); return; } // notifications
    seen.push(m);
    const answer = () => {
      switch (m.method) {
        case 'tools/list': return send({ jsonrpc: '2.0', id: m.id, result: { tools: rEval ? [...BASE_TOOLS, R_EVAL] : BASE_TOOLS } });
        case 'tools/call': {
          const p = m.params || {};
          if (p.name === 'collabnet_sleep') return; // never answers -> exercises the bridge timeout
          if (![...BASE_TOOLS, ...(rEval ? [R_EVAL] : []), { name: 'collabnet_sleep' }].some((t) => t.name === p.name))
            return send({ jsonrpc: '2.0', id: m.id, error: { code: -32601, message: `Unknown tool: ${p.name}` } });
          const structured = { tool: p.name, echo: p.arguments ?? null, page_saw_id: m.id };
          return send({ jsonrpc: '2.0', id: m.id, result: { content: [{ type: 'text', text: JSON.stringify(structured) }], structuredContent: structured } });
        }
        case 'resources/list': return send({ jsonrpc: '2.0', id: m.id, result: { resources: [] } });
        default: return send({ jsonrpc: '2.0', id: m.id, error: { code: -32601, message: `Method not found: ${m.method}` } });
      }
    };
    setTimeout(answer, Math.floor(Math.random() * jitterMs));
  });
  const opened = new Promise((resolve) => {
    ws.addEventListener('open', () => resolve(true));
    ws.addEventListener('error', () => resolve(false));
    ws.addEventListener('close', () => resolve(false));
  });
  return {
    ws, opened, seen, notes,
    setREval(on) { rEval = !!on; send({ jsonrpc: '2.0', method: 'notifications/tools/list_changed' }); },
    close() { return new Promise((r) => { if (ws.readyState === WebSocket.CLOSED) return r(); ws.addEventListener('close', () => r()); ws.close(); }); },
  };
}
