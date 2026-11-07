/*
 * collabnet-ai-providers.js -- D2-B3: provider-neutral core for the in-page AI chat (no DOM, unit-testable in Node).
 *
 * Providers: OpenAI-compatible Chat Completions (any base URL: OpenAI, OpenRouter, LM Studio, vLLM...), Anthropic
 * Messages, Google Gemini generateContent, Ollama native /api/chat. One neutral conversation format is converted per
 * provider; the MCP tool registry (window.collabnetMCP.listTools) is converted per provider.
 * Request/response shapes were checked against ellmer (MIT) @0f06878: provider-claude.R (x-api-key, anthropic-version,
 * input_schema, tool_result), provider-openai-compatible.R (type:function tools), provider-google.R (x-goog-api-key,
 * functionDeclarations, functionCall/functionResponse), provider-ollama.R (/api/chat). Own implementation.
 * Markdown link safety follows shinychat's rules (MIT; urlSanitize.ts safeProtocol, rehypeExternalLinks rel=noopener)
 * but is our own escape-everything renderer: no raw HTML from the model is ever emitted.
 */
(function (root) {
  "use strict";

  var DEFAULT_LIMITS = { maxRounds: 8, requestTimeoutMs: 60000, turnTimeoutMs: 300000, maxToolOutputChars: 60000 };

  var PROVIDERS = {
    openai: { label: "OpenAI-compatible (custom base URL)", base: "https://api.openai.com/v1", model: "gpt-4o-mini" },
    anthropic: { label: "Anthropic (Messages API)", base: "https://api.anthropic.com", model: "claude-sonnet-5-5" },
    gemini: { label: "Google Gemini", base: "https://generativelanguage.googleapis.com", model: "gemini-2.5-flash" },
    ollama: { label: "Ollama (local, native API)", base: "http://localhost:11434", model: "llama3.1" }
  };

  function trimSlash(u) { return String(u || "").trim().replace(/\/+$/, ""); }

  function endpointUrl(provider, cfg) {
    var base = trimSlash(cfg.baseUrl || PROVIDERS[provider].base);
    if (provider === "openai") return /\/chat\/completions$/.test(base) ? base : base + "/chat/completions";
    if (provider === "anthropic") return /\/v1\/messages$/.test(base) ? base : base + "/v1/messages";
    if (provider === "gemini") return base + "/v1beta/models/" + encodeURIComponent(cfg.model) + ":generateContent";
    if (provider === "ollama") return base + "/api/chat";
    throw new Error("Unknown provider: " + provider);
  }

  function checkUrl(u) {
    var url = new URL(u);
    var local = ["localhost", "127.0.0.1", "[::1]", "::1"].indexOf(url.hostname) >= 0;
    if (url.protocol !== "https:" && !(url.protocol === "http:" && local)) {
      throw new Error("Use HTTPS for remote model endpoints. HTTP is allowed only for localhost endpoints.");
    }
    return url;
  }

  // ---- tools: MCP registry -> provider schema ----
  function cleanSchemaForGemini(s) {
    if (Array.isArray(s)) return s.map(cleanSchemaForGemini);
    if (!s || typeof s !== "object") return s;
    var out = {};
    Object.keys(s).forEach(function (k) {
      if (k === "$schema" || k === "additionalProperties" || k === "default" || k === "examples") return; // not accepted by Gemini
      out[k] = cleanSchemaForGemini(s[k]);
    });
    return out;
  }

  function convertTools(provider, tools) {
    var list = (tools || []).map(function (t) {
      return { name: t.name, description: t.description || "", schema: t.inputSchema || { type: "object", properties: {} } };
    });
    if (!list.length) return undefined;
    if (provider === "anthropic") return list.map(function (t) { return { name: t.name, description: t.description, input_schema: t.schema }; });
    if (provider === "gemini") return [{ functionDeclarations: list.map(function (t) { return { name: t.name, description: t.description, parameters: cleanSchemaForGemini(t.schema) }; }) }];
    return list.map(function (t) { return { type: "function", function: { name: t.name, description: t.description, parameters: t.schema } }; });
  }

  // ---- neutral conversation -> provider messages ----
  // conv items: {role:"user", text} | {role:"assistant", text, toolCalls:[{id,name,args}]} | {role:"tool", results:[{id,name,content,isError}]}
  function buildRequest(provider, cfg, system, conv, tools) {
    var headers = { "Content-Type": "application/json" };
    var body;
    var ptools = convertTools(provider, tools);
    if (provider === "openai" || provider === "ollama") {
      var msgs = system ? [{ role: "system", content: system }] : [];
      conv.forEach(function (m) {
        if (m.role === "user") msgs.push({ role: "user", content: m.text });
        else if (m.role === "assistant") {
          var am = { role: "assistant", content: m.text || "" };
          if (m.toolCalls && m.toolCalls.length) am.tool_calls = m.toolCalls.map(function (c) {
            return provider === "ollama" ? { function: { name: c.name, arguments: c.args } }
              : { id: c.id, type: "function", function: { name: c.name, arguments: JSON.stringify(c.args || {}) } };
          });
          msgs.push(am);
        } else m.results.forEach(function (r) {
          msgs.push(provider === "ollama" ? { role: "tool", tool_name: r.name, content: r.content } : { role: "tool", tool_call_id: r.id, content: r.content });
        });
      });
      body = { model: cfg.model, messages: msgs };
      if (ptools) body.tools = ptools;
      if (provider === "openai") {
        if (ptools) body.tool_choice = "auto";
        if (cfg.maxTokens) body.max_tokens = cfg.maxTokens;
        if (cfg.key) headers.Authorization = "Bearer " + cfg.key;
      }
      else body.stream = false;
    } else if (provider === "anthropic") {
      var am2 = conv.map(function (m) {
        if (m.role === "user") return { role: "user", content: m.text };
        if (m.role === "assistant") {
          var blocks = [];
          if (m.text) blocks.push({ type: "text", text: m.text });
          (m.toolCalls || []).forEach(function (c) { blocks.push({ type: "tool_use", id: c.id, name: c.name, input: c.args || {} }); });
          return { role: "assistant", content: blocks };
        }
        return { role: "user", content: m.results.map(function (r) { return { type: "tool_result", tool_use_id: r.id, content: r.content, is_error: !!r.isError }; }) };
      });
      body = { model: cfg.model, max_tokens: cfg.maxTokens || 4096, messages: am2 };
      if (system) body.system = system;
      if (ptools) body.tools = ptools;
      headers["x-api-key"] = cfg.key || "";
      headers["anthropic-version"] = "2023-06-01";
      headers["anthropic-dangerous-direct-browser-access"] = "true"; // required for CORS from a browser page
    } else if (provider === "gemini") {
      var contents = conv.map(function (m) {
        if (m.role === "user") return { role: "user", parts: [{ text: m.text }] };
        if (m.role === "assistant") {
          var parts = [];
          if (m.text) parts.push({ text: m.text });
          (m.toolCalls || []).forEach(function (c) { parts.push({ functionCall: { name: c.name, args: c.args || {} } }); });
          return { role: "model", parts: parts };
        }
        return { role: "user", parts: m.results.map(function (r) { return { functionResponse: { name: r.name, response: { content: r.content, isError: !!r.isError } } }; }) };
      });
      body = { contents: contents };
      if (system) body.systemInstruction = { parts: [{ text: system }] };
      if (ptools) body.tools = ptools;
      headers["x-goog-api-key"] = cfg.key || "";
    } else throw new Error("Unknown provider: " + provider);
    return { url: endpointUrl(provider, cfg), init: { method: "POST", headers: headers, body: JSON.stringify(body), mode: "cors", credentials: "omit" } };
  }

  function parseArgs(a) {
    if (a && typeof a === "object") return a;
    try { var v = JSON.parse(a || "{}"); return v && typeof v === "object" ? v : {}; } catch (e) { return { __invalid_json: String(a).slice(0, 200) }; }
  }

  // provider JSON -> {text, toolCalls}
  function parseResponse(provider, data) {
    var text = "", calls = [];
    if (provider === "openai") {
      var msg = data && data.choices && data.choices[0] && data.choices[0].message;
      if (!msg) throw new Error("The endpoint did not return choices[0].message (expected an OpenAI-compatible Chat Completions API).");
      text = msg.content || "";
      (msg.tool_calls || []).forEach(function (c, i) { calls.push({ id: c.id || "call_" + i, name: c.function && c.function.name, args: parseArgs(c.function && c.function.arguments) }); });
    } else if (provider === "ollama") {
      var om = data && data.message;
      if (!om) throw new Error("The endpoint did not return an Ollama /api/chat message.");
      text = om.content || "";
      (om.tool_calls || []).forEach(function (c, i) { calls.push({ id: "ollama_" + i, name: c.function && c.function.name, args: parseArgs(c.function && c.function.arguments) }); });
    } else if (provider === "anthropic") {
      if (!data || !Array.isArray(data.content)) throw new Error("The endpoint did not return an Anthropic Messages content array.");
      data.content.forEach(function (b) {
        if (b.type === "text") text += b.text;
        else if (b.type === "tool_use") calls.push({ id: b.id, name: b.name, args: parseArgs(b.input) });
      });
    } else if (provider === "gemini") {
      var cand = data && data.candidates && data.candidates[0];
      if (!cand) {
        var br = data && data.promptFeedback && data.promptFeedback.blockReason;
        throw new Error(br ? "Gemini blocked the prompt (" + br + ")." : "The endpoint did not return Gemini candidates.");
      }
      ((cand.content && cand.content.parts) || []).forEach(function (p, i) {
        if (typeof p.text === "string") text += p.text;
        if (p.functionCall) calls.push({ id: "gemini_" + i, name: p.functionCall.name, args: parseArgs(p.functionCall.args) });
      });
    }
    return { text: text, toolCalls: calls };
  }

  // ---- errors ----
  function ProviderError(kind, message, extra) {
    var e = new Error(message); e.name = "ProviderError"; e.kind = kind;
    if (extra) Object.keys(extra).forEach(function (k) { e[k] = extra[k]; });
    return e;
  }

  var CORS_HINT = "The browser could not reach the endpoint (network error or CORS). Many providers block direct browser calls. " +
    "Options: an OpenAI-compatible gateway that allows browser CORS (e.g. OpenRouter); local Ollama started with " +
    "OLLAMA_ORIGINS set to this page's origin; or use the MCP bridge with a desktop AI client instead of the in-page chat. " +
    "Also check that the host is allowed by the page's Content-Security-Policy connect-src.";

  function mapHttpError(status, bodyText, headers) {
    var body = String(bodyText || "").slice(0, 600), low = body.toLowerCase();
    var get = function (h) { return headers && typeof headers.get === "function" ? headers.get(h) : headers && headers[h]; };
    if (/context[_ ]length|too many tokens|prompt is too long|maximum context|context window|exceeds the (maximum|limit)/.test(low)) {
      return ProviderError("context", "The conversation is too long for this model's context window. Clear the chat or ask a narrower question. (HTTP " + status + ")");
    }
    if (status === 401 || status === 403) return ProviderError("auth", "Authentication failed (HTTP " + status + "): check the API key and that it is valid for this provider/model.");
    if (status === 429) {
      var ra = get("retry-after"), secs = ra && isFinite(Number(ra)) ? Number(ra) : null;
      return ProviderError("rate_limit", "Rate limited by the provider (HTTP 429)." + (secs !== null ? " Retry after " + secs + " s." : " Wait a moment and retry."), { retryAfter: secs });
    }
    if (status === 404 || /model.{0,40}(not found|does not exist|not_found)|unknown model|no such model/.test(low)) {
      return ProviderError("model", "Model or endpoint not found (HTTP " + status + "): check the model name and base URL. " + body.slice(0, 200));
    }
    if (status === 529 || /overloaded/.test(low)) return ProviderError("overloaded", "The provider is overloaded (HTTP " + status + "); retry shortly.");
    return ProviderError("http", "Model endpoint returned HTTP " + status + ": " + body.slice(0, 300));
  }

  // fetch with a per-request timeout and an outer (cancel / turn) signal
  async function timedFetch(fetchFn, url, init, timeoutMs, outerSignal) {
    var ac = new AbortController(), timedOut = false;
    var t = setTimeout(function () { timedOut = true; ac.abort(); }, timeoutMs);
    var onOuter = function () { ac.abort(); };
    if (outerSignal) { if (outerSignal.aborted) ac.abort(); else outerSignal.addEventListener("abort", onOuter); }
    try {
      return await fetchFn(url, Object.assign({}, init, { signal: ac.signal }));
    } catch (e) {
      if (timedOut) throw ProviderError("timeout", "The model request timed out after " + Math.round(timeoutMs / 1000) + " s.");
      if (outerSignal && outerSignal.aborted) throw ProviderError("cancelled", (outerSignal.reason && outerSignal.reason.message) || "Cancelled.");
      if (e && e.name === "TypeError") throw ProviderError("network", CORS_HINT + " (" + e.message + ")");
      throw e;
    } finally {
      clearTimeout(t);
      if (outerSignal) outerSignal.removeEventListener("abort", onOuter);
    }
  }

  async function callModel(opts) {
    var req = buildRequest(opts.provider, opts.cfg, opts.system, opts.conv, opts.tools);
    checkUrl(req.url);
    var res = await timedFetch(opts.fetch, req.url, req.init, opts.limits.requestTimeoutMs, opts.signal);
    if (!res.ok) throw mapHttpError(res.status, await res.text().catch(function () { return ""; }), res.headers);
    return parseResponse(opts.provider, await res.json());
  }

  function capOutput(s, max) {
    s = String(s == null ? "" : s);
    if (s.length <= max) return s;
    return s.slice(0, max) + "\n\n[CollabNET: tool output truncated from " + s.length + " to " + max + " characters. Ask for a narrower query (filters, limit) to see more.]";
  }

  function isReadOnly(tool) { return !!(tool && tool.annotations && tool.annotations.readOnlyHint === true); }

  function resultText(result) {
    if (result && Array.isArray(result.content)) return result.content.map(function (c) { return c && c.text != null ? c.text : ""; }).join("\n");
    return JSON.stringify(result == null ? null : result);
  }

  /*
   * One user turn: model <-> tools loop with limits and the approval gate.
   * opts: provider, cfg {baseUrl, model, key}, system, conv (mutated), tools (MCP list), fetch, callTool(name,args),
   *       approve(tool, args) -> Promise<boolean> (asked for every non-read-only tool), limits, signal (cancel), onEvent(type, data)
   */
  async function runTurn(opts) {
    var limits = Object.assign({}, DEFAULT_LIMITS, opts.limits || {});
    var ev = opts.onEvent || function () {};
    var turnAc = new AbortController();
    var turnTimer = setTimeout(function () { turnAc.abort(new Error("The whole turn exceeded " + Math.round(limits.turnTimeoutMs / 1000) + " s and was stopped.")); }, limits.turnTimeoutMs);
    var onCancel = function () { turnAc.abort(new Error("Cancelled by user.")); };
    if (opts.signal) { if (opts.signal.aborted) onCancel(); else opts.signal.addEventListener("abort", onCancel); }
    var byName = {};
    (opts.tools || []).forEach(function (t) { byName[t.name] = t; });
    try {
      for (var round = 0; round < limits.maxRounds; round++) {
        var out = await callModel({ provider: opts.provider, cfg: opts.cfg, system: opts.system, conv: opts.conv, tools: opts.tools, fetch: opts.fetch, limits: limits, signal: turnAc.signal });
        opts.conv.push({ role: "assistant", text: out.text, toolCalls: out.toolCalls });
        if (!out.toolCalls.length) { ev("assistant", out.text); return { text: out.text, rounds: round + 1 }; }
        if (out.text) ev("assistant", out.text);
        var results = [];
        for (var i = 0; i < out.toolCalls.length; i++) {
          var c = out.toolCalls[i], tool = byName[c.name], content, isError = false;
          if (turnAc.signal.aborted) throw ProviderError("cancelled", (turnAc.signal.reason && turnAc.signal.reason.message) || "Cancelled.");
          if (!tool) { content = "Tool rejected: unknown tool '" + String(c.name) + "'."; isError = true; }
          else if (!isReadOnly(tool) && !(await opts.approve(tool, c.args))) {
            content = "The user declined this call to '" + c.name + "'. Do not retry it; ask the user how to proceed."; isError = true;
            ev("tool-declined", { name: c.name, args: c.args });
          } else {
            ev("tool-call", { name: c.name, args: c.args, readOnly: isReadOnly(tool) });
            try {
              var r = await opts.callTool(c.name, c.args);
              content = resultText(r); isError = !!(r && r.isError);
            } catch (e) { content = "Tool error: " + (e && e.message || e); isError = true; }
            content = capOutput(content, limits.maxToolOutputChars);
            ev("tool-result", { name: c.name, content: content, isError: isError });
          }
          results.push({ id: c.id, name: c.name, content: content, isError: isError });
        }
        opts.conv.push({ role: "tool", results: results });
      }
      var msg = "Stopped after " + limits.maxRounds + " model/tool rounds (the per-turn limit). Ask a narrower follow-up question.";
      ev("limit", msg);
      return { text: msg, rounds: limits.maxRounds, limited: true };
    } finally {
      clearTimeout(turnTimer);
      if (opts.signal) opts.signal.removeEventListener("abort", onCancel);
    }
  }

  // ---- safe markdown -> HTML string (everything escaped; only our own tags are produced) ----
  function esc(s) {
    return String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;").replace(/'/g, "&#39;");
  }
  var SAFE_PROTOCOL = /^(https?|mailto):$/i;
  function safeHref(u) {
    var raw = String(u || "").trim();
    // decode entities/whitespace tricks first: "java\tscript:", "&#106;avascript:"
    if (/[\u0000-\u001f\u007f]/.test(raw) || /&#|%[0-9a-f]{2}.*:/i.test(raw.split("/")[0])) return null;
    try { var p = new URL(raw); return SAFE_PROTOCOL.test(p.protocol) ? p.href : null; } catch (e) { return null; }
  }
  function inline(s) {
    // s is already escaped; tokens are applied on the escaped text so no raw HTML can appear
    var codes = [];
    s = s.replace(/`([^`]+)`/g, function (_, c) { codes.push(c); return "\u0000" + (codes.length - 1) + "\u0000"; });
    s = s.replace(/\[([^\]]{1,300})\]\(([^)\s]{1,2000})\)/g, function (m, label, href) {
      var h = safeHref(href.replace(/&amp;/g, "&"));
      return h ? '<a href="' + esc(h) + '" target="_blank" rel="noopener noreferrer nofollow">' + label + "</a>" : label + " (" + href + ")";
    });
    s = s.replace(/\*\*([^*]+)\*\*/g, "<strong>$1</strong>").replace(/(^|[^*])\*([^*\s][^*]*)\*/g, "$1<em>$2</em>");
    return s.replace(/\u0000(\d+)\u0000/g, function (_, i) { return "<code>" + codes[+i] + "</code>"; });
  }
  function renderMarkdown(md) {
    var lines = String(md == null ? "" : md).replace(/\r\n?/g, "\n").split("\n");
    var html = [], para = [], list = null, inCode = false, code = [];
    var flushPara = function () { if (para.length) { html.push("<p>" + inline(esc(para.join(" "))) + "</p>"); para = []; } };
    var flushList = function () { if (list) { html.push("<" + list.tag + ">" + list.items.map(function (i) { return "<li>" + inline(esc(i)) + "</li>"; }).join("") + "</" + list.tag + ">"); list = null; } };
    lines.forEach(function (ln) {
      if (/^\s*```/.test(ln)) {
        if (inCode) { html.push("<pre><code>" + esc(code.join("\n")) + "</code></pre>"); code = []; inCode = false; }
        else { flushPara(); flushList(); inCode = true; }
        return;
      }
      if (inCode) { code.push(ln); return; }
      var m;
      if (!ln.trim()) { flushPara(); flushList(); return; }
      if ((m = /^(#{1,4})\s+(.*)$/.exec(ln))) { flushPara(); flushList(); var lv = Math.min(6, m[1].length + 2); html.push("<h" + lv + ">" + inline(esc(m[2])) + "</h" + lv + ">"); return; }
      if ((m = /^\s*([-*]|\d+\.)\s+(.*)$/.exec(ln))) {
        flushPara(); var tag = /\d/.test(m[1]) ? "ol" : "ul";
        if (!list || list.tag !== tag) { flushList(); list = { tag: tag, items: [] }; }
        list.items.push(m[2]); return;
      }
      flushList(); para.push(ln.trim());
    });
    if (inCode) html.push("<pre><code>" + esc(code.join("\n")) + "</code></pre>");
    flushPara(); flushList();
    return html.join("\n");
  }

  var api = {
    PROVIDERS: PROVIDERS, DEFAULT_LIMITS: DEFAULT_LIMITS, endpointUrl: endpointUrl, checkUrl: checkUrl, convertTools: convertTools,
    buildRequest: buildRequest, parseResponse: parseResponse, mapHttpError: mapHttpError, timedFetch: timedFetch,
    callModel: callModel, runTurn: runTurn, capOutput: capOutput, isReadOnly: isReadOnly, renderMarkdown: renderMarkdown,
    safeHref: safeHref, escapeHtml: esc, CORS_HINT: CORS_HINT
  };
  if (typeof module !== "undefined" && module.exports) module.exports = api;
  else root.CollabNETAIProviders = api;
})(typeof window !== "undefined" ? window : this);
