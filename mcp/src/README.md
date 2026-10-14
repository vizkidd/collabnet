# collabnet-mcp

An MCP server that gives an assistant a real R interpreter for this project,
plus a headless browser running the built shinylive app.

## Why this shape

There is no way to "natively" run a webR/WASM Shiny app from a language model.
What you can do is expose the two runtimes it depends on as tools:

1. **webR in Node.** webR is R compiled to WebAssembly, and it ships as an npm
   package that runs outside the browser. That is the *same runtime* the
   deployed app uses, so "does this package exist for WASM", "does this code
   run", "how long does this take" all become answerable rather than guessed.
   This covers everything in CollabNET except Shiny itself: the graph engine,
   parsing, matching, metrics, plotting data preparation.

2. **Chromium with cross-origin isolation.** shinylive runs Shiny inside webR
   inside a browser and needs `SharedArrayBuffer`, which needs
   `Cross-Origin-Opener-Policy: same-origin` and
   `Cross-Origin-Embedder-Policy: require-corp`. The server sets both. Without
   them the app boots to a blank page with only a console warning — a failure
   mode worth having a tool for on its own.

**Honest limitation:** webR in Node cannot run Shiny. Reactive behaviour,
`plotlyProxy`, `visNetworkProxy` and the self-invalidating-reactive class of bug
are only observable through the browser tools. Split your work accordingly:
pure R logic → `r_*` tools (fast, seconds); reactivity and rendering →
`app_*` tools (slow, minutes, needs a build).

## Install

```bash
cd mcp
npm install
npx playwright install chromium   # optional, only for the app_* tools
```

Node 20+. First `r_eval` downloads the webR runtime and any R packages from
`repo.r-wasm.org`, so the first call is slow and needs network access. To run
fully offline, download a webR dist and point `WEBR_BASE_URL` at it.

Smoke test without an MCP client:

```bash
node src/smoke.mjs
```

## Configure

### Claude Code — `.mcp.json` at the repo root

```json
{
  "mcpServers": {
    "collabnet": {
      "command": "node",
      "args": ["mcp/src/server.mjs"],
      "env": {
        "COLLABNET_REPO": ".",
        "COLLABNET_WASM_BUILD": "wasm_build",
        "COLLABNET_PORT": "8788"
      }
    }
  }
}
```

### Claude Desktop — `claude_desktop_config.json`

```json
{
  "mcpServers": {
    "collabnet": {
      "command": "node",
      "args": ["/absolute/path/to/collabnet-main/mcp/src/server.mjs"],
      "env": {
        "COLLABNET_REPO": "/absolute/path/to/collabnet-main",
        "COLLABNET_WASM_BUILD": "/absolute/path/to/collabnet-main/wasm_build"
      }
    }
  }
}
```

| Variable | Default | Meaning |
|---|---|---|
| `COLLABNET_REPO` | parent of `mcp/` | Checkout, mounted read-through at `/collabnet` in webR |
| `COLLABNET_WASM_BUILD` | `<repo>/wasm_build` | `shinylive::export()` output |
| `COLLABNET_PORT` | `8788` | Static server port |
| `COLLABNET_ARTIFACTS` | `<repo>/.mcp-artifacts` | Screenshot destination |
| `WEBR_BASE_URL` | bundled npm dist | Local webR dist for offline use |

Add `.mcp-artifacts/` to `.gitignore`.

## Tools

### R / webR

| Tool | Purpose |
|---|---|
| `r_eval` | Evaluate R. Persistent session, serialised calls, returns stdout/stderr/conditions/value |
| `r_load_collabnet` | Source the `GScholarLENS-*.R` files in a deliberate order and report per-file failures |
| `r_install_packages` | Install from the WASM binary repos; confirms a dependency exists for wasm at all |
| `r_scan_dependencies` | Static scan for `require()`/`library()`/`pkg::`, plus packages used via `::` that are never declared |
| `r_run_tests` | Run the invariant suite in `R/harness.R` |
| `r_make_fixture` | Generate a synthetic corpus, optionally with hyperauthored papers |
| `r_benchmark_network` | Time the graph builder across variants; wall time, node/edge counts, peak memory |
| `r_session_info` | R version, wasm confirmation, which definition of each duplicated function won |
| `r_reset` | Restart the interpreter |

### App / browser

| Tool | Purpose |
|---|---|
| `app_serve` | Static server with the required COOP/COEP headers |
| `app_open` | Headless Chromium; waits for the Shiny session to bind outputs; reports boot errors |
| `app_set_input` | `Shiny.setInputValue(id, value)` |
| `app_read_output` | Text of a rendered output node |
| `app_time_interaction` | Set inputs, wait for Shiny to go idle, report elapsed ms |
| `app_screenshot` | PNG of the page or one selector |
| `app_logs` | Drain console messages, page errors and failed requests |
| `app_close` | Release Chromium and the port |

## Worked example: proving out the graph rework

```
r_load_collabnet
r_run_tests
r_make_fixture  var_name=fx  n_papers=800  n_mega=2  mega_size=1200
r_benchmark_network  data_var=fx  target_delim=", "
```

`r_benchmark_network` compares `newman+star`, `count+drop`,
`newman+star+backbone` and `newman+clique` on the same data. The `clique`
variant is the old behaviour; expect it to be the slow one or to error on heap
exhaustion, which is itself the result you want recorded.

To compare directly against the pre-rework implementation, rename the original
before loading:

```r
build_collaboration_network_legacy <- build_collaboration_network
```

then `r_eval` → `mcp_benchmark_vs_legacy(fx, target_delim = ", ")`.

## Worked example: catching the reactive loop

```
app_open
app_set_input      id=enable_network  value=true
app_logs           level=errors
app_time_interaction inputs=[{"id":"edge_freq_range","value":[1,5]}]
app_screenshot     selector="#network_filtered"
```

If `app_time_interaction` never settles, or `app_logs` shows repeated webR
activity with no input changes, that is the self-invalidating
`network_cache()` reactive described in `REPORT.md` §1.4.

## Notes and gotchas

- **Serialisation.** webR is one interpreter. Concurrent `r_eval` calls would
  interleave into the same REPL, so `WebRSession.run()` chains every operation.
  Long benchmarks block other R tools; they do not block the `app_*` tools.
- **NODEFS.** The checkout is mounted read-through, so editing a `.R` file on
  disk and re-running `r_load_collabnet` picks up the change with no copy.
  The R working directory is `/scratch`, not the mount, so the app's
  `dir.create("keys")` does not write into your repo.
- **Source order matters.** Several functions are defined more than once
  (`compute_h_index` in both `Data2GLENS.R` and `helpers.R`;
  `build_collaboration_network` in both `PlotGLENS.R` and `Graph.R`). The
  loader forces `Fixes.R` last. Use `r_session_info` to confirm which
  definition actually won — `buildWASM.R` concatenates alphabetically and can
  disagree with a local run.
- **Memory.** A `clique`-mode benchmark on a 3000-author paper can exhaust the
  webR heap. That is the point of the benchmark, but you will need `r_reset`
  afterwards.
- **`app_*` needs a build.** Run `buildWASM.R` (or just the
  `shinylive::export()` call in it, without the `runStaticServer` at the end)
  before using them.
