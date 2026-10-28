/*
 * CollabNET browser-native MCP adapter.
 *
 * This implements the MCP JSON-RPC methods initialize, ping, tools/list, and
 * tools/call over an explicitly supplied MessagePort. It does NOT open a
 * public network endpoint and is not a drop-in Streamable HTTP transport.
 * A same-page host must attach a port with CollabNETMCP.attachPort(port).
 */
(() => {
  "use strict";

  if (window.CollabNETMCP) return;

  const SERVER_INFO = { name: "collabnet-browser-mcp", version: "0.1.0" };
  const FALLBACK_PROTOCOL_VERSION = "2026-07-28";
  const SUPPORTED_PROTOCOL_VERSIONS = new Set(["2026-07-28", "2025-11-25", "2025-06-18"]);
  const MAX_PENDING = 8;
  const MAX_RESULT_CHARS = 250_000;
  const REQUEST_TIMEOUT_MS = 30_000;
  let enabled = false;
  let nextRequestId = 0;
  const pending = new Map();
  const attachedPorts = new Map();

  const tool = (name, description, properties = {}, required = []) => ({
    name,
    description,
    inputSchema: {
      type: "object",
      properties,
      required,
      additionalProperties: false
    }
  });

  const stringProp = (description, extra = {}) => ({
    type: "string", description, ...extra
  });
  const integerProp = (description, minimum, maximum, extra = {}) => ({
    type: "integer", description, minimum, maximum, ...extra
  });

  const TOOLS = [
    tool("collabnet_get_status",
      "Return readiness and active-session status for the current CollabNET browser tab."),
    tool("collabnet_get_dataset_schema",
      "Describe columns and types in the full, extended, and currently filtered CollabNET publication tables."),
    tool("collabnet_get_dataset_summary",
      "Summarize the requested table in the current CollabNET session.", {
        dataset: { type: "string", enum: ["full", "extended", "year_filtered"], default: "year_filtered" }
      }),
    tool("collabnet_search_publications",
      "Search the current CollabNET publication table using literal, case-insensitive substring matching; results are paginated.", {
        query: stringProp("Search text; treated as literal text, not a regular expression.", { minLength: 1, maxLength: 300 }),
        dataset: { type: "string", enum: ["full", "extended", "year_filtered"], default: "year_filtered" },
        fields: { type: "array", items: { type: "string" }, maxItems: 30, description: "Optional column names to search. Defaults to the app's most relevant bibliographic columns." },
        year_from: integerProp("Inclusive minimum publication year.", 1000, 3000),
        year_to: integerProp("Inclusive maximum publication year.", 1000, 3000),
        limit: integerProp("Maximum number of records to return (default 20).", 1, 50, { default: 20 }),
        offset: integerProp("Number of matching records to skip (default 0).", 0, 100000, { default: 0 })
      }, ["query"]),
    tool("collabnet_get_bibliometrics",
      "Calculate bibliometric/collaboration summary metrics on an existing table; does not modify the app's filters.", {
        dataset: { type: "string", enum: ["full", "extended", "year_filtered"], default: "year_filtered" },
        entity_column: stringProp("Multi-value column containing entities, usually Authors."),
        year_column: stringProp("Year column name (default Year).", { default: "Year" }),
        citation_column: stringProp("Citation count column name (default Citations).", { default: "Citations" })
      }),
    tool("collabnet_get_network_status",
      "Report whether the active CollabNET graph is available and return bounded graph metadata."),
    tool("collabnet_get_network_metrics",
      "Return graph-level metrics for the already-built active network; does not rebuild it."),
    tool("collabnet_get_centrality_leaderboard",
      "Return top nodes from the active graph using CollabNET's existing centrality leaderboard.", {
        top_n: integerProp("Maximum rows to return (default 20).", 1, 100, { default: 20 })
      }),
    tool("collabnet_get_node_metrics",
      "Return existing centrality, degree, community, and cut-vertex fields for one active graph node.", {
        node_id: stringProp("Exact node ID/label from the active graph.", { minLength: 1, maxLength: 500 })
      }, ["node_id"]),
    tool("collabnet_get_neighbors",
      "Return a bounded 1- or 2-hop neighborhood from the active graph. min_weight filters direct edges when hops=1.", {
        node_id: stringProp("Exact node ID/label from the active graph.", { minLength: 1, maxLength: 500 }),
        hops: integerProp("Neighborhood radius; only 1 or 2 is supported (default 1).", 1, 2, { default: 1 }),
        limit: integerProp("Maximum nodes to return (default 50).", 1, 100, { default: 50 }),
        min_weight: { type: "number", minimum: 0, description: "Optional minimum direct edge weight; applied only when hops=1." }
      }, ["node_id"]),
    tool("collabnet_get_communities",
      "Summarize communities in the active graph with size and strongest representative nodes.", {
        min_size: integerProp("Minimum community size (default 2).", 1, 100000, { default: 2 }),
        limit: integerProp("Maximum communities to return (default 50).", 1, 100, { default: 50 })
      }),
    tool("collabnet_get_ego_metrics",
      "Return existing ego-network and brokerage metrics for up to 20 target nodes in the active graph.", {
        targets: { type: "array", minItems: 1, maxItems: 20, items: { type: "string", minLength: 1, maxLength: 500 }, description: "Target names/IDs; CollabNET's existing word-boundary matching is used." }
      }, ["targets"]),
    tool("collabnet_get_graph_diagnostics",
      "Return the existing Hamiltonian/topological diagnostic table for the active graph."),
    tool("collabnet_classify_identifier",
      "Classify a DOI, Scopus EID, PMID, PMCID, arXiv ID, ISBN, URL, or other identifier using CollabNET's existing classifier.", {
        raw_id: stringProp("Identifier or identifier URL to classify.", { minLength: 1, maxLength: 2000 })
      }, ["raw_id"]),
    tool("collabnet_normalize_doi",
      "Extract a DOI from text or a URL and return a normalized comparison form when one is present.", {
        raw_id: stringProp("DOI, DOI URL, or text containing a DOI.", { minLength: 1, maxLength: 2000 })
      }, ["raw_id"])
  ];

  function publishStatus() {
    const clients = [...attachedPorts.values()].map((item) => ({
      clientName: item.clientName,
      connectedAt: item.connectedAt
    }));
    const detail = {
      enabled,
      connections: attachedPorts.size,
      clients,
      connected: attachedPorts.size > 0
    };
    window.dispatchEvent(new CustomEvent("collabnet:mcp-status", { detail }));
    // Small status-only message for the optional extension content script.
    // It contains no dataset values, tool arguments, or API credentials.
    try { window.postMessage({ type: "COLLABNET_MCP_STATUS", detail }, window.location.origin); } catch (_) { /* best effort */ }
    const getElement = document && typeof document.getElementById === "function"
      ? (id) => document.getElementById(id)
      : () => null;
    const dot = getElement("collabnet-mcp-led");
    const label = getElement("collabnet-mcp-status-text");
    const count = getElement("collabnet-mcp-connection-count");
    if (dot) {
      dot.classList.toggle("is-connected", detail.connected);
      dot.classList.toggle("is-disconnected", !detail.connected);
      dot.setAttribute("aria-label", detail.connected ? "MCP connected" : "MCP disconnected");
    }
    if (label) label.textContent = detail.connected ? "MCP connected" : "MCP disconnected";
    if (count) count.textContent = `${detail.connections} connection${detail.connections === 1 ? "" : "s"}`;
    return detail;
  }

  function setEnabled(value) {
    enabled = value === true;
    publishStatus();
  }

  function detachPort(port) {
    if (!attachedPorts.has(port)) return;
    attachedPorts.delete(port);
    try { port.onmessage = null; } catch (_) { /* best effort */ }
    publishStatus();
  }

  function requestR(toolName, args) {
    if (!enabled) return Promise.reject(new Error("Enable CollabNET browser MCP in the page first."));
    if (pending.size >= MAX_PENDING) return Promise.reject(new Error("Too many pending CollabNET MCP requests."));
    if (!window.Shiny || typeof window.Shiny.setInputValue !== "function") {
      return Promise.reject(new Error("Shiny has not finished initializing."));
    }

    const id = `cnet-${Date.now().toString(36)}-${++nextRequestId}`;
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        pending.delete(id);
        reject(new Error("CollabNET request timed out."));
      }, REQUEST_TIMEOUT_MS);
      pending.set(id, { resolve, reject, timer });
      window.Shiny.setInputValue("collabnet_mcp_request", {
        id,
        tool: toolName,
        arguments: args || {}
      }, { priority: "event" });
    });
  }

  function addShinyHandlers() {
    if (!window.Shiny || typeof window.Shiny.addCustomMessageHandler !== "function") {
      document.addEventListener("shiny:connected", addShinyHandlers, { once: true });
      return;
    }

    window.Shiny.addCustomMessageHandler("collabnet_mcp_response", (message) => {
      const entry = pending.get(message.id);
      if (!entry) return;
      clearTimeout(entry.timer);
      pending.delete(message.id);
      if (message.ok) entry.resolve(message.result);
      else entry.reject(new Error(message.error?.message || "CollabNET tool failed."));
    });

    window.Shiny.addCustomMessageHandler("collabnet_mcp_enabled", (message) => {
      setEnabled(message && message.enabled === true);
    });
  }

  function jsonRpcError(id, code, message, data) {
    const result = { jsonrpc: "2.0", id: id ?? null, error: { code, message } };
    if (data !== undefined) result.error.data = data;
    return result;
  }

  async function handleRpc(message) {
    if (!message || message.jsonrpc !== "2.0" || typeof message.method !== "string") {
      return jsonRpcError(message?.id ?? null, -32600, "Invalid JSON-RPC request.");
    }
    const id = Object.prototype.hasOwnProperty.call(message, "id") ? message.id : undefined;
    const isNotification = id === undefined;
    const params = message.params || {};

    if (message.method === "notifications/initialized" || message.method === "notifications/cancelled") {
      return null;
    }
    if (message.method === "initialize") {
      return {
        jsonrpc: "2.0", id,
        result: {
          protocolVersion: SUPPORTED_PROTOCOL_VERSIONS.has(params.protocolVersion)
            ? params.protocolVersion
            : FALLBACK_PROTOCOL_VERSION,
          capabilities: { tools: { listChanged: false } },
          serverInfo: SERVER_INFO,
          instructions: "Tools act on the current CollabNET browser session. Data stays in this page unless results are sent to the connected AI host. Enable access in the page before calling tools."
        }
      };
    }
    if (message.method === "ping") return isNotification ? null : { jsonrpc: "2.0", id, result: {} };
    if (message.method === "tools/list") {
      return isNotification ? null : { jsonrpc: "2.0", id, result: { tools: TOOLS } };
    }
    if (message.method === "tools/call") {
      if (isNotification) return null;
      const name = params.name;
      const args = params.arguments || {};
      if (!TOOLS.some((t) => t.name === name)) return jsonRpcError(id, -32602, `Unknown tool: ${String(name)}`);
      try {
        const value = await requestR(name, args);
        const textValue = JSON.stringify(value, null, 2);
        if (typeof textValue === "string" && textValue.length > MAX_RESULT_CHARS) {
          throw new Error("Tool result exceeded the browser MCP output limit. Narrow the query or request fewer fields/rows.");
        }
        const result = {
          content: [{ type: "text", text: textValue === undefined ? "null" : textValue }],
          structuredContent: value
        };
        return { jsonrpc: "2.0", id, result };
      } catch (error) {
        return {
          jsonrpc: "2.0", id,
          result: {
            isError: true,
            content: [{ type: "text", text: String(error?.message || error) }]
          }
        };
      }
    }

    return isNotification ? null : jsonRpcError(id, -32601, `Method not found: ${message.method}`);
  }

  function attachPort(port, metadata = {}) {
    if (!port || typeof port.postMessage !== "function") {
      throw new TypeError("attachPort requires a MessagePort-like object.");
    }
    if (attachedPorts.has(port)) return;
    attachedPorts.set(port, {
      clientName: String(metadata.clientName || "MCP client").slice(0, 100),
      connectedAt: new Date().toISOString()
    });
    const onMessage = async (event) => {
      const message = event.data;
      // Private transport teardown notification; not an MCP protocol method.
      if (message && message.jsonrpc === "2.0" && message.method === "collabnet/disconnect") {
        try {
          port.postMessage({ jsonrpc: "2.0", id: message.id ?? null, result: { disconnected: true } });
        } catch (_) { /* port may already be closing */ }
        detachPort(port);
        try { port.close(); } catch (_) { /* best effort */ }
        return;
      }
      const reply = await handleRpc(message);
      if (reply !== null) {
        try { port.postMessage(reply); } catch (_) { detachPort(port); }
      }
    };
    port.onmessage = onMessage;
    if (typeof port.addEventListener === "function") {
      port.addEventListener("messageerror", () => detachPort(port));
      port.addEventListener("close", () => detachPort(port));
    }
    if (typeof port.start === "function") port.start();
    publishStatus();
  }

  function handleMessage(message) {
    return handleRpc(message);
  }

  window.CollabNETMCP = Object.freeze({
    version: SERVER_INFO.version,
    serverInfo: SERVER_INFO,
    tools: TOOLS,
    attachPort,
    handleMessage,
    setEnabled,
    get enabled() { return enabled; },
    get connectionCount() { return attachedPorts.size; },
    get clients() { return [...attachedPorts.values()].map((item) => ({ ...item })); }
  });

  // The browser extension transfers a MessagePort through same-origin
  // postMessage. The app's explicit consent switch remains the authoritative
  // R-side gate for every data request. Do not expose a general-purpose
  // window.postMessage RPC endpoint.
  window.addEventListener("message", (event) => {
    if (event.source !== window || event.origin !== window.location.origin) return;
    if (!event.data || event.data.type !== "COLLABNET_MCP_CONNECT" || !event.ports || !event.ports[0]) return;
    const clientName = typeof event.data.clientName === "string"
      ? event.data.clientName.slice(0, 100)
      : "Browser extension / agentic IDE";
    attachPort(event.ports[0], { clientName });
  });

  // Keep all tool calls disabled until the Shiny checkbox is initialized.
  setEnabled(false);
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", addShinyHandlers, { once: true });
  } else {
    addShinyHandlers();
  }
})();
