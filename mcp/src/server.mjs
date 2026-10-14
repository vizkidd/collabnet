#!/usr/bin/env node
// -----------------------------------------------------------------------------
// collabnet-mcp
//
// An MCP server that gives an assistant two things it cannot otherwise have
// for this project:
//
//   1. A live R interpreter running in the SAME WebAssembly runtime the app
//      ships in, with the CollabNET sources loaded. So "does this R code
//      actually work in webR" stops being a guess.
//   2. A headless, cross-origin-isolated browser running the built shinylive
//      app, so UI behaviour, console errors and interaction latency are
//      observable.
//
// Transport: stdio.
// -----------------------------------------------------------------------------

import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { z } from "zod";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { WebRSession } from "./webr-session.mjs";
import { ShinyliveHarness } from "./browser.mjs";

const HERE = path.dirname(fileURLToPath(import.meta.url));

const REPO_PATH =
  process.env.COLLABNET_REPO ?? path.resolve(HERE, "..", "..");
const BUILD_DIR =
  process.env.COLLABNET_WASM_BUILD ?? path.join(REPO_PATH, "wasm_build");
const WEBR_BASE_URL = process.env.WEBR_BASE_URL || undefined;
const PORT = Number(process.env.COLLABNET_PORT ?? 8788);
const ARTIFACT_DIR =
  process.env.COLLABNET_ARTIFACTS ?? path.join(REPO_PATH, ".mcp-artifacts");

const R = new WebRSession({ repoPath: REPO_PATH, baseUrl: WEBR_BASE_URL });
const UI = new ShinyliveHarness({ buildDir: BUILD_DIR, port: PORT });

const server = new McpServer({ name: "collabnet", version: "0.1.0" });

// ---- helpers ---------------------------------------------------------------

const text = (s) => ({ content: [{ type: "text", text: String(s) }] });
const json = (o) => text(JSON.stringify(o, null, 2));

function renderEval(r) {
  const parts = [];
  if (r.error) parts.push(`## error\n${r.error}`);
  if (r.stdout?.trim()) parts.push(`## stdout\n${r.stdout.trim()}`);
  if (r.stderr?.trim()) parts.push(`## stderr\n${r.stderr.trim()}`);
  if (r.messages?.length) parts.push(`## conditions\n${r.messages.join("\n")}`);
  if (r.value !== null && r.value !== undefined)
    parts.push(`## value\n${typeof r.value === "string" ? r.value : JSON.stringify(r.value)}`);
  return parts.length ? parts.join("\n\n") : "(no output)";
}

// =============================================================================
// R / webR tools
// =============================================================================

server.registerTool(
  "r_eval",
  {
    title: "Evaluate R in webR",
    description:
      "Run R code inside the webR (R-on-WebAssembly) session. This is the same runtime the deployed app uses, so a package that is missing here is missing in production. Returns stdout, stderr, warnings/messages and the visible value. Calls are serialised; the session persists between calls.",
    inputSchema: {
      code: z.string().describe("R source to evaluate."),
      as_json: z
        .boolean()
        .default(false)
        .describe("Serialise the result with jsonlite::toJSON instead of printing it. Use for data frames and lists you want to read programmatically."),
      timeout_ms: z.number().int().default(120000),
    },
  },
  async ({ code, as_json, timeout_ms }) => {
    const r = await R.eval(code, { json: as_json, timeoutMs: timeout_ms });
    return text(renderEval(r));
  },
);

server.registerTool(
  "r_load_collabnet",
  {
    title: "Load the CollabNET sources into webR",
    description:
      "Sources the GScholarLENS-*.R files (with source() lines stripped, as buildWASM.R does) in a deliberate order, then loads the test/benchmark harness. Reports which files loaded and which failed with their error. Run this after editing any R file to pick up the change — the repo is mounted read-through via NODEFS, so no copy step is needed.",
    inputSchema: {
      order: z
        .array(z.string())
        .optional()
        .describe("Explicit file order. Omit for the known-good default order (Fixes last, so its definitions win)."),
      include_server_ui: z
        .boolean()
        .default(false)
        .describe("Also source ui.R and server.R. These define a Shiny app object and will fail outside a Shiny session; only useful for parse checking."),
    },
  },
  async ({ order, include_server_ui }) => {
    const res = await R.loadCollabnet({
      order,
      skipServerUi: !include_server_ui,
    });
    return json(res);
  },
);

server.registerTool(
  "r_install_packages",
  {
    title: "Install R packages into webR",
    description:
      "Install binary R packages from the WASM repositories (repo.r-wasm.org, r-multiverse). Use this to confirm whether a dependency actually exists for WebAssembly before committing to it — several CRAN packages do not.",
    inputSchema: { packages: z.array(z.string()).min(1) },
  },
  async ({ packages }) => json(await R.installPackages(packages)),
);

server.registerTool(
  "r_scan_dependencies",
  {
    title: "Scan CollabNET for R package dependencies",
    description:
      "Static scan of every .R file for require()/library()/pkg:: usage, cross-checked against what is actually installed in this webR session. Also reports packages referenced only via pkg:: with no require() anywhere — the failure mode that makes shinylive omit them from the bundle.",
    inputSchema: {},
  },
  async () => {
    const a = await R.eval("mcp_scan_deps()", { json: true });
    const b = await R.eval("mcp_undeclared_deps()", { json: true });
    return text(
      `## declared vs available\n${a.value ?? a.error}\n\n## referenced via :: but never require()d\n${b.value ?? b.error}`,
    );
  },
);

server.registerTool(
  "r_run_tests",
  {
    title: "Run the CollabNET invariant tests",
    description:
      "Runs the assertions in harness.R: h-index boundary, author-string splitting for Scopus-style input, Newman edge weighting arithmetic, hyperauthored-paper folding, sparse triplet extraction, and word-bounded target matching. Returns a pass/fail table.",
    inputSchema: {},
  },
  async () => {
    const r = await R.eval("mcp_tests()", { json: true });
    return text(r.value ?? renderEval(r));
  },
);

server.registerTool(
  "r_make_fixture",
  {
    title: "Generate a synthetic publication corpus",
    description:
      "Builds a data frame shaped like real CollabNET input, assigned to an R variable. Author counts are log-normal (matching observed bibliometric distributions), with an optional set of hyperauthored papers to reproduce the high-energy-physics case that causes the performance problem.",
    inputSchema: {
      var_name: z.string().default("fx"),
      n_papers: z.number().int().default(500),
      pool: z.number().int().default(2000).describe("Size of the ordinary author pool."),
      n_mega: z.number().int().default(0).describe("Number of hyperauthored papers."),
      mega_size: z.number().int().default(1000).describe("Authors on each hyperauthored paper."),
      seed: z.number().int().default(1),
    },
  },
  async ({ var_name, n_papers, pool, n_mega, mega_size, seed }) => {
    const code = `${var_name} <- mcp_fixture(n_papers = ${n_papers}L, pool = ${pool}L, n_mega = ${n_mega}L, mega_size = ${mega_size}L, seed = ${seed}L); list(rows = nrow(${var_name}), max_authors = max(lengths(strsplit(${var_name}$Authors, ", ", fixed = TRUE))))`;
    const r = await R.eval(code, { json: true });
    return text(r.value ?? renderEval(r));
  },
);

server.registerTool(
  "r_benchmark_network",
  {
    title: "Benchmark build_collaboration_network variants",
    description:
      "Times the graph builder across weighting schemes, hyperauthored-paper handling and backbone filtering on the same fixture, reporting wall time, node/edge counts and peak memory per variant. Each variant is isolated so one blowing the heap does not lose the others. This is the tool to use when judging whether a change to the graph engine actually helped.",
    inputSchema: {
      data_var: z.string().default("fx").describe("Name of an R data frame in the session."),
      target_col: z.string().default("Authors"),
      target_delim: z.string().default(", "),
      reps: z.number().int().default(1),
    },
  },
  async ({ data_var, target_col, target_delim, reps }) => {
    const r = await R.eval(
      `mcp_benchmark_network(${data_var}, target_col = ${JSON.stringify(target_col)}, target_delim = ${JSON.stringify(target_delim)}, reps = ${reps}L)`,
      { json: true, timeoutMs: 900000 },
    );
    return text(r.value ?? renderEval(r));
  },
);

server.registerTool(
  "r_session_info",
  {
    title: "webR session status",
    description:
      "R version, whether the machine really is wasm, working directory, which CollabNET files are visible, and which key functions are currently defined. Use this to confirm which definition of a duplicated function (compute_h_index, build_collaboration_network) actually won.",
    inputSchema: {},
  },
  async () => {
    const r = await R.eval("mcp_sysinfo()", { json: true });
    return text(r.value ?? renderEval(r));
  },
);

server.registerTool(
  "r_reset",
  {
    title: "Restart the webR session",
    description:
      "Tears down and reinitialises the R interpreter. Use after a heap exhaustion, or when you need a clean namespace to test source ordering.",
    inputSchema: {},
  },
  async () => json(await R.reset()),
);

// =============================================================================
// shinylive / browser tools
// =============================================================================

server.registerTool(
  "app_serve",
  {
    title: "Serve the built shinylive app",
    description:
      "Starts a local static server over the shinylive export directory with the Cross-Origin-Opener-Policy and Cross-Origin-Embedder-Policy headers that webR's SharedArrayBuffer channel requires. Without those headers the app boots to a blank page. Returns the URL.",
    inputSchema: {},
  },
  async () => text(`Serving ${BUILD_DIR} at ${await UI.serve()}`),
);

server.registerTool(
  "app_open",
  {
    title: "Open the app in a headless browser",
    description:
      "Launches Chromium against the served app and waits for the Shiny session to connect and bind its first outputs. Reports whether boot succeeded. Every console message, page error and failed request from here on is collected and retrievable with app_logs — this is how you see webR errors that never reach the R side.",
    inputSchema: {
      timeout_ms: z.number().int().default(180000).describe("webR cold boot can take 60-120s."),
      headless: z.boolean().default(true),
    },
  },
  async ({ timeout_ms, headless }) => {
    try {
      const res = await UI.open({ timeoutMs: timeout_ms, headless });
      const logs = UI.drainLogs();
      const bad = logs.filter((l) =>
        ["error", "pageerror", "requestfailed"].includes(l.level),
      );
      return text(
        `${JSON.stringify(res, null, 2)}\n\n${bad.length ? `## ${bad.length} error(s) during boot\n` + bad.map((l) => `[${l.level}] ${l.text}`).join("\n") : "No errors during boot."}`,
      );
    } catch (e) {
      return text(`Failed to open: ${e.message}`);
    }
  },
);

server.registerTool(
  "app_set_input",
  {
    title: "Set a Shiny input value",
    description:
      "Calls Shiny.setInputValue(id, value) in the page, driving the app as a user would. Use the input IDs from ui.R, e.g. enable_network, net_col_filtered, edge_freq_range, custom_max_keywords, fr_iterations.",
    inputSchema: {
      id: z.string(),
      value: z.any().describe("JSON value. Range sliders take a two-element array."),
    },
  },
  async ({ id, value }) => text(await UI.setInput(id, value)),
);

server.registerTool(
  "app_read_output",
  {
    title: "Read a rendered output element",
    description: "Returns the visible text of the DOM node with the given output id, e.g. network_summary_table or summary_table.",
    inputSchema: { id: z.string() },
  },
  async ({ id }) => text(await UI.readOutput(id)),
);

server.registerTool(
  "app_time_interaction",
  {
    title: "Measure how long an interaction takes to settle",
    description:
      "Sets one or more inputs, then waits until Shiny has been idle (no shiny-busy class) for a settle window, and reports the elapsed milliseconds. This measures what the user actually feels, including the webR round trip and canvas repaint — unlike an R-side timing, which misses both.",
    inputSchema: {
      inputs: z
        .array(z.object({ id: z.string(), value: z.any() }))
        .min(1),
      settle_ms: z.number().int().default(400),
      max_ms: z.number().int().default(300000),
    },
  },
  async ({ inputs, settle_ms, max_ms }) => {
    const ms = await UI.timeInteraction(
      async () => {
        for (const i of inputs) await UI.setInput(i.id, i.value);
      },
      { settleMs: settle_ms, maxMs: max_ms },
    );
    return text(`${ms} ms to settle after setting ${inputs.map((i) => i.id).join(", ")}`);
  },
);

server.registerTool(
  "app_screenshot",
  {
    title: "Screenshot the app",
    description:
      "Captures the page (or one selector) to a PNG under the artifacts directory and returns the path. Use it to check that a plot or network actually rendered rather than silently producing an empty trace.",
    inputSchema: {
      name: z.string().default("app"),
      selector: z.string().optional().describe("CSS selector to crop to, e.g. #network_filtered"),
      full_page: z.boolean().default(true),
    },
  },
  async ({ name, selector, full_page }) => {
    const out = path.join(ARTIFACT_DIR, `${name}-${Date.now()}.png`);
    return text(await UI.screenshot(out, { selector, fullPage: full_page }));
  },
);

server.registerTool(
  "app_logs",
  {
    title: "Drain collected browser logs",
    description:
      "Returns and clears every console message, uncaught page error and failed network request since the last drain. webR reports most R-level failures here rather than in the UI, so check this whenever something renders blank.",
    inputSchema: {
      level: z
        .enum(["all", "errors"])
        .default("errors")
        .describe("'errors' filters to error / pageerror / requestfailed."),
    },
  },
  async ({ level }) => {
    let logs = UI.drainLogs();
    if (level === "errors")
      logs = logs.filter((l) =>
        ["error", "pageerror", "requestfailed", "warning"].includes(l.level),
      );
    return text(
      logs.length
        ? logs.map((l) => `[${l.level}] ${l.text}`).join("\n")
        : "(no matching log entries)",
    );
  },
);

server.registerTool(
  "app_close",
  {
    title: "Close the browser and static server",
    description: "Releases the Chromium instance and the port.",
    inputSchema: {},
  },
  async () => {
    await UI.close();
    return text("closed");
  },
);

// ---- lifecycle -------------------------------------------------------------

async function shutdown() {
  try {
    await UI.close();
  } catch {
    /* ignore */
  }
  process.exit(0);
}
process.on("SIGINT", shutdown);
process.on("SIGTERM", shutdown);

await server.connect(new StdioServerTransport());
