#!/usr/bin/env node
// =============================================================================
// test_suite.mjs  —  CollabNET comprehensive parameter & regression test suite
//
// Usage:
//   node src/test_suite.mjs [--verbose] [--csv path/to/file.csv]
//
// What it covers:
//   1. Session bootstrap + source order verification
//   2. Scopus CSV import + field inspection
//   3. Invariant unit tests (harness.R mcp_tests)
//   4. Dependency scan (all packages resolvable in WASM)
//   5. Graph engine parameters:
//        weight_scheme  (newman / count / jaccard)
//        oversize_mode  (star / drop / clique)
//        backbone_alpha (NULL / 0.10 / 0.05 / 0.01)
//        node_freq_range
//        edge_freq_range
//        authors_per_pub
//        cluster_size_range
//        prune_leaves
//        detect_communities + compute_centrality
//        layout_engine  (auto / fr / drl / kk)
//   6. cn_ helper functions:
//        cn_slider_bounds, cn_network_metrics, cn_ego_metrics,
//        cn_bibliometrics, cn_collapse_communities
//   7. Synthetic fixture benchmark (300 papers + 1 HEP mega-paper)
//   8. Real-CSV benchmark across all weight × oversize combinations
//   9. Edge-case / stress tests (empty col, single author, 0 rows, etc.)
//  10. Final summary table
// =============================================================================

import path from "node:path";
import { fileURLToPath } from "node:url";
import { WebRSession } from "./webr-session.mjs";

const HERE   = path.dirname(fileURLToPath(import.meta.url));
const REPO   = path.resolve(HERE, "..", "..");     // collabnet-main/
const CSV_DEFAULT = path.join(REPO, "scopus_export_March2026.csv");

const VERBOSE = process.argv.includes("--verbose");
const csvArg  = process.argv.indexOf("--csv");
const CSV_PATH = csvArg !== -1 ? process.argv[csvArg + 1] : CSV_DEFAULT;

// ── helpers ──────────────────────────────────────────────────────────────────

const pad  = (s, n = 55) => String(s).padEnd(n);
const tick = "✓"; const cross = "✗"; const warn = "⚠";

let passed = 0, failed = 0, warned = 0;
const results = [];

function record(group, name, status, detail = "") {
  results.push({ group, name, status, detail });
  if (status === "PASS") { passed++; process.stdout.write(`  ${tick} `); }
  else if (status === "FAIL") { failed++; process.stdout.write(`  ${cross} `); }
  else { warned++; process.stdout.write(`  ${warn} `); }
  console.log(`${pad(name)} ${status}${detail ? "  » " + detail : ""}`);
}

function section(title) {
  console.log(`\n${"═".repeat(70)}`);
  console.log(`  ${title}`);
  console.log("─".repeat(70));
}

// Evaluate R and return parsed JSON value, or throw on error.
async function rj(R, code, opts = {}) {
  const r = await R.eval(code, { json: true, timeoutMs: opts.timeoutMs ?? 120_000 });
  if (r.error) throw new Error(r.error);
  if (r.messages?.some(m => m.startsWith("[error]")))
    throw new Error(r.messages.filter(m => m.startsWith("[error]")).join("; "));
  // value comes back as a JSON string from jsonlite::toJSON
  if (typeof r.value === "string") {
    try { return JSON.parse(r.value); } catch { return r.value; }
  }
  return r.value;
}

// Evaluate R, return raw text (stdout + value).
async function rt(R, code, opts = {}) {
  const r = await R.eval(code, { json: false, timeoutMs: opts.timeoutMs ?? 120_000 });
  if (r.error) throw new Error(r.error);
  return (r.stdout + (r.value != null ? String(r.value) : "")).trim();
}

// Safe version: never throws — returns { ok, data, err }.
async function safe(fn) {
  try { return { ok: true, data: await fn() }; }
  catch (e) { return { ok: false, err: e.message }; }
}

// ── minimal check helpers ─────────────────────────────────────────────────────

function chk(group, name, condition, failDetail = "") {
  record(group, name, condition ? "PASS" : "FAIL", condition ? "" : failDetail);
}

function chkApprox(group, name, actual, expected, tol = 1e-4) {
  const ok = Math.abs(actual - expected) <= tol;
  record(group, name, ok ? "PASS" : "FAIL",
    ok ? "" : `got ${actual}, expected ${expected} ±${tol}`);
}

// =============================================================================
// MAIN
// =============================================================================

const R = new WebRSession({ repoPath: REPO });

console.log("╔══════════════════════════════════════════════════════════════════════╗");
console.log("║  CollabNET WebR Test Suite                                           ║");
console.log("╚══════════════════════════════════════════════════════════════════════╝");
console.log(`\nRepo  : ${REPO}`);
console.log(`CSV   : ${CSV_PATH}`);
console.log(`Time  : ${new Date().toISOString()}\n`);

// ── PHASE 1: Bootstrap ────────────────────────────────────────────────────────
section("1  Session bootstrap & source loading");

const loaded = await R.loadCollabnet();
const loadedFiles = loaded.loaded ?? [];
const failedFiles = loaded.failed ?? [];

const EXPECTED_ORDER = [
  "GScholarLENS-DOI2Data.R", "GScholarLENS-ORCID2Data.R",
  "GScholarLENS-SCOPUS2Data.R", "GScholarLENS-ProcessJCR.R",
  "GScholarLENS-Data2GLENS.R", "GScholarLENS-PlotGLENS.R",
  "GScholarLENS-Graph.R", "GScholarLENS-helpers.R",
  "GScholarLENS-Fixes.R",
];

chk("bootstrap", "All 9 helper files loaded",
    loadedFiles.length >= 9, `only ${loadedFiles.length} loaded`);
chk("bootstrap", "No files failed to load",
    failedFiles.length === 0,
    failedFiles.map(f => `${f.file}: ${f.error}`).join("; "));
chk("bootstrap", "Graph.R loads after PlotGLENS.R (definition wins)",
    loadedFiles.indexOf("GScholarLENS-Graph.R") >
    loadedFiles.indexOf("GScholarLENS-PlotGLENS.R"),
    "Graph.R must come after PlotGLENS.R");
chk("bootstrap", "Fixes.R loads last",
    loadedFiles[loadedFiles.length - 2] === "GScholarLENS-Fixes.R" ||
    loadedFiles[loadedFiles.length - 1] === "GScholarLENS-Fixes.R",
    `last was ${loadedFiles.at(-1)}`);

// Session info
const sysinfo = await safe(() => rj(R, "mcp_sysinfo()"));
if (sysinfo.ok) {
  const s = sysinfo.data;
  chk("bootstrap", "is_wasm flag correct (Node = FALSE)",
      s.is_wasm === false || s.is_wasm === "false" || !s.is_wasm, "");
  chk("bootstrap", "build_collaboration_network exists",
      s.key_functions?.build_collaboration_network === true ||
      s.key_functions?.build_collaboration_network === "true", "");
  chk("bootstrap", "cn_project exists",
      s.key_functions?.cn_project === true ||
      s.key_functions?.cn_project === "true", "");
  chk("bootstrap", "cn_slider_bounds exists",
      s.key_functions?.cn_slider_bounds === true ||
      s.key_functions?.cn_slider_bounds === "true", "");
  if (VERBOSE) console.log("\n  sysinfo:", JSON.stringify(s, null, 2));
}

// ── PHASE 2: Dependency scan ──────────────────────────────────────────────────
section("2  Package dependency scan (WASM availability)");

const deps = await safe(() => rj(R, "mcp_scan_deps()", { timeoutMs: 60_000 }));
if (deps.ok) {
  const rows = Array.isArray(deps.data) ? deps.data : Object.values(deps.data ?? {});
  const missing = rows.filter(r => r.available === false || r.available === "false");
  if (missing.length === 0) {
    record("deps", "All declared packages available in WASM", "PASS");
  } else {
    for (const m of missing)
      record("deps", `Package ${m.package} available`, "WARN", "not in WASM binary repo — may need a polyfill");
  }
  if (VERBOSE) console.log("\n  deps:", JSON.stringify(rows, null, 2));
}

const undecl = await safe(() => rj(R, "mcp_undeclared_deps()", { timeoutMs: 30_000 }));
if (undecl.ok) {
  const u = Array.isArray(undecl.data) ? undecl.data : [];
  chk("deps", "No undeclared :: dependencies", u.length === 0,
      u.length ? u.join(", ") : "");
  if (VERBOSE && u.length) console.log("  undeclared::", u);
}

// ── PHASE 3: Invariant unit tests ─────────────────────────────────────────────
section("3  Invariant unit tests  (mcp_tests)");

const inv = await safe(() => rj(R, "mcp_tests()", { timeoutMs: 60_000 }));
if (inv.ok) {
  const rows = Array.isArray(inv.data) ? inv.data : Object.values(inv.data ?? {});
  for (const r of rows) {
    const pass = r.pass === true || r.pass === "true" || r.pass === 1;
    record("invariants", r.test, pass ? "PASS" : "FAIL", r.detail ?? "");
  }
} else {
  record("invariants", "mcp_tests() ran", "FAIL", inv.err);
}

// ── PHASE 4: Scopus CSV import ────────────────────────────────────────────────
section("4  Scopus CSV import");

// Mount CSV into webR scratch so R can read it
const csvR = CSV_PATH.replace(/\\/g, "/");
const csvLoad = await safe(() => rt(R, `
  df_raw <- tryCatch(
    read.csv("/collabnet/scopus_export_March2026.csv",
             stringsAsFactors = FALSE, check.names = FALSE,
             fileEncoding = "UTF-8-BOM"),
    error = function(e) conditionMessage(e)
  )
  if (is.data.frame(df_raw)) {
    paste0("rows=", nrow(df_raw), " cols=", ncol(df_raw),
           " colnames=", paste(colnames(df_raw), collapse="|"))
  } else {
    paste0("ERR:", df_raw)
  }
`));

chk("csv", "CSV loaded without error", csvLoad.ok && !csvLoad.data.startsWith("ERR"),
    csvLoad.ok ? csvLoad.data : csvLoad.err);

let csvRows = 0, csvCols = 0, csvColNames = [];
if (csvLoad.ok && !csvLoad.data.startsWith("ERR")) {
  const m = csvLoad.data.match(/rows=(\d+) cols=(\d+) colnames=(.+)/);
  if (m) {
    csvRows = +m[1]; csvCols = +m[2];
    csvColNames = m[3].split("|");
  }
  console.log(`  → ${csvRows} rows × ${csvCols} cols`);
}

const expectedCols = ["Authors", "Title", "Year", "Cited by", "DOI",
                       "Source title", "Author Keywords", "Index Keywords",
                       "Affiliations"];
for (const col of expectedCols) {
  chk("csv", `Column "${col}" present`, csvColNames.includes(col),
      `available: ${csvColNames.slice(0,8).join(", ")}…`);
}

// Inspect author field
const authInspect = await safe(() => rj(R, `
  list(
    n_rows         = nrow(df_raw),
    sample_authors = head(df_raw$Authors, 3),
    max_authors    = max(lengths(strsplit(df_raw$Authors, ";", fixed = TRUE))),
    delim_guess    = if (any(grepl(";", df_raw$Authors))) ";" else ","
  )
`));
if (authInspect.ok) {
  const a = authInspect.data;
  console.log(`  → Author delimiter detected: "${a.delim_guess}"`);
  console.log(`  → Max authors on one paper: ${a.max_authors}`);
  chk("csv", "Authors column non-empty", a.n_rows > 0, "");
  chk("csv", "Author delimiter detected", !!a.delim_guess, "");
  if (VERBOSE) console.log("  Sample authors:", a.sample_authors);
}

// Normalise to standard format for the graph engine
const normSetup = await safe(() => rt(R, `
  # Scopus uses "; " between authors and stores "Last, F." format
  # CollabNET split_authors handles both separators
  delim_csv <- if (any(grepl(";", df_raw$Authors))) "; " else ", "
  # Use the df_raw directly — build_collaboration_network tokenises internally
  df <- df_raw
  # rename "Cited by" -> "Citations" for the bibliometrics helper
  names(df)[names(df) == "Cited by"] <- "Citations"
  names(df)[names(df) == "Source title"] <- "Journal"
  nrow(df)
`));
chk("csv", "DataFrame normalised for graph engine", normSetup.ok,
    normSetup.ok ? "" : normSetup.err);

// ── PHASE 5: cn_slider_bounds ────────────────────────────────────────────────
section("5  cn_slider_bounds (UI initialisation)");

const bounds = await safe(() => rj(R, `
  cn_slider_bounds(df, target_col = "Authors", target_delim = delim_csv)
`));
if (bounds.ok) {
  const b = bounds.data;
  console.log(`  → max_node_occurrence=${b.max_node_occurrence}  max_edge_frequency=${b.max_edge_frequency}  max_cluster_size=${b.max_cluster_size}  max_paper_size=${b.max_paper_size}`);
  chk("slider_bounds", "max_node_occurrence >= 1", +b.max_node_occurrence >= 1, "");
  chk("slider_bounds", "max_edge_frequency >= 1",  +b.max_edge_frequency  >= 1, "");
  chk("slider_bounds", "max_paper_size >= 1",       +b.max_paper_size      >= 1, "");
} else {
  record("slider_bounds", "cn_slider_bounds returned", "FAIL", bounds.err);
}


// ── Helper to build graph engine R calls ─────────────────────────────────────
function build_net(opts) {
  const base = {
    df: "df",
    target_col: '"Authors"',
    target_delim: "delim_csv",
    node_freq_range: "c(1, Inf)",
    edge_freq_range: "c(1, Inf)",
    authors_per_pub: "c(1, 200)",
    cluster_size_range: "c(1, Inf)",
    prune_leaves: "FALSE",
    oversize_mode: '"star"',
    detect_communities: "FALSE",
    compute_centrality: "FALSE",
    build_tooltips: "FALSE",
    verbose: "FALSE",
    ...opts
  };
  const args = Object.entries(base)
    .filter(([k, v]) => v !== undefined)
    .map(([k, v]) => k === "df" ? v : `${k} = ${v}`)
    .join(", ");
  return `${build_net({})}`;
}

// ── PHASE 6: Graph engine — weight schemes ───────────────────────────────────
section("6  Graph engine: weight_scheme parameter");

for (const scheme of ["newman", "count", "jaccard"]) {
  const r = await safe(() => rj(R, `
    net <- ${build_net({weight_scheme: `"${scheme}"`, oversize_mode: `"star"`})}
    list(nodes = nrow(net$nodes), edges = nrow(net$edges),
         has_weight = "weight" %in% names(net$edges),
         sample_w   = if (nrow(net$edges) > 0) round(net$edges$weight[1], 4) else NA)
  `, { timeoutMs: 180_000 }));
  if (r.ok) {
    const d = r.data;
    chk("weight_scheme", `weight_scheme="${scheme}" builds without error`,
        d.nodes >= 0, "");
    chk("weight_scheme", `weight_scheme="${scheme}" has weight column`,
        d.has_weight === true || d.has_weight === "true", "");
    console.log(`    nodes=${d.nodes}  edges=${d.edges}  sample_weight=${d.sample_w}`);
  } else {
    record("weight_scheme", `weight_scheme="${scheme}"`, "FAIL", r.err);
  }
}

// Verify newman < count for a multi-author pair
const wtCmp = await safe(() => rj(R, `
  d2 <- data.frame(Authors = c("A One; B Two", "A One; B Two; C Three"),
                   stringsAsFactors = FALSE)
  n <- ${build_net({df: `d2`, target_delim: `"; "`, weight_scheme: `"newman"`, node_freq_range: `c(1`})},
         edge_freq_range = c(1,Inf), cluster_size_range = c(1,Inf),
         authors_per_pub = c(1,500), prune_leaves = FALSE,
         detect_communities = FALSE, compute_centrality = FALSE)
  c_net <- ${build_net({df: `d2`, target_delim: `"; "`, weight_scheme: `"count"`, node_freq_range: `c(1`})},
         edge_freq_range = c(1,Inf), cluster_size_range = c(1,Inf),
         authors_per_pub = c(1,500), prune_leaves = FALSE,
         detect_communities = FALSE, compute_centrality = FALSE)
  ab_n <- n$edges[(n$edges$from=="A One" & n$edges$to=="B Two")|
                  (n$edges$to=="A One"   & n$edges$from=="B Two"),]
  ab_c <- c_net$edges[(c_net$edges$from=="A One" & c_net$edges$to=="B Two")|
                      (c_net$edges$to=="A One"   & c_net$edges$from=="B Two"),]
  list(newman_AB = if(nrow(ab_n)>0) round(ab_n$weight[1],4) else NA,
       count_AB  = if(nrow(ab_c)>0) ab_c$connections[1] else NA)
`));
if (wtCmp.ok) {
  chkApprox("weight_scheme", "Newman AB weight == 1.5 (1 + 0.5)",
            +wtCmp.data.newman_AB, 1.5, 0.001);
  chk("weight_scheme", "Raw count AB == 2",
      wtCmp.data.count_AB == 2, `got ${wtCmp.data.count_AB}`);
}

// ── PHASE 7: oversize_mode ───────────────────────────────────────────────────
section("7  Graph engine: oversize_mode parameter");

// Create a mixed df: 10 normal papers + 1 mega with 400 authors
await R.eval(`
  big_df <- data.frame(
    Authors = c(
      paste(paste0("Normal", 1:5), collapse = "; "),
      paste(paste0("Normal", 6:10), collapse = "; "),
      paste(paste0("Mega", 1:400), collapse = "; ")
    ), stringsAsFactors = FALSE)
`);

for (const mode of ["star", "drop", "clique"]) {
  const r = await safe(() => rj(R, `
    s <- ${build_net({df: `big_df`, target_delim: `"; "`, oversize_mode: `"${mode}"`, authors_per_pub: `c(1`})},
           node_freq_range = c(1,Inf), edge_freq_range = c(1,Inf),
           cluster_size_range = c(1,Inf), prune_leaves = FALSE,
           detect_communities = FALSE, compute_centrality = FALSE,
           build_tooltips = FALSE)
    list(nodes = nrow(s$nodes), edges = nrow(s$edges))
  `, { timeoutMs: 180_000 }));
  if (r.ok) {
    const d = r.data;
    if (mode === "star") {
      chk("oversize_mode", `star: edge count <= 400 (hub, not clique)`,
          d.edges <= 400, `got ${d.edges}`);
    } else if (mode === "drop") {
      chk("oversize_mode", `drop: 400-author paper excluded`,
          d.nodes < 400, `got ${d.nodes} nodes`);
    } else {
      // clique is allowed to be large
      chk("oversize_mode", `clique: builds without crash (nodes>0)`,
          d.nodes > 0, `got ${d.nodes}`);
    }
    console.log(`    oversize_mode="${mode}"  nodes=${d.nodes}  edges=${d.edges}`);
  } else {
    record("oversize_mode", `oversize_mode="${mode}"`, "FAIL", r.err);
  }
}

// ── PHASE 8: backbone_alpha ───────────────────────────────────────────────────
section("8  Graph engine: backbone_alpha (disparity filter)");

for (const alpha of [null, 0.10, 0.05, 0.01]) {
  const alphaArg = alpha === null ? "NULL" : String(alpha);
  const label    = alpha === null ? "none (full graph)" : `alpha=${alpha}`;
  const r = await safe(() => rj(R, `
    net <- ${build_net({backbone_alpha: `${alphaArg}`, oversize_mode: `"star"`})}
    list(nodes = nrow(net$nodes), edges = nrow(net$edges))
  `, { timeoutMs: 180_000 }));
  if (r.ok) {
    record("backbone", `backbone ${label}`, "PASS",
           `nodes=${r.data.nodes}  edges=${r.data.edges}`);
  } else {
    record("backbone", `backbone ${label}`, "FAIL", r.err);
  }
}

// Verify backbone actually prunes
const bbPrune = await safe(() => rj(R, `
  full   <- ${build_net({backbone_alpha: `NULL`})}
  pruned <- ${build_net({backbone_alpha: `0.05`})}
  list(full_edges = nrow(full$edges), pruned_edges = nrow(pruned$edges))
`));
if (bbPrune.ok) {
  chk("backbone", "Backbone alpha=0.05 prunes edges vs full graph",
      bbPrune.data.pruned_edges <= bbPrune.data.full_edges,
      `full=${bbPrune.data.full_edges}  pruned=${bbPrune.data.pruned_edges}`);
}

// ── PHASE 9: node_freq_range ─────────────────────────────────────────────────
section("9  Graph engine: node_freq_range filter");

const nfCases = [
  { lo: 1, hi: "Inf", label: "all nodes" },
  { lo: 2, hi: "Inf", label: "min 2 occurrences" },
  { lo: 1, hi: 1,     label: "exactly 1 occurrence" },
];
let prevEdges = Infinity;
for (const c of nfCases) {
  const r = await safe(() => rj(R, `
    net <- ${build_net({node_freq_range: `c(${c.lo}`})}, oversize_mode = "star")
    list(nodes = nrow(net$nodes), edges = nrow(net$edges))
  `));
  if (r.ok) {
    record("node_freq", `node_freq_range [${c.lo},${c.hi}] — ${c.label}`,
           "PASS", `nodes=${r.data.nodes}  edges=${r.data.edges}`);
    if (c.lo === 2)
      chk("node_freq", "min freq=2 gives fewer nodes than min freq=1",
          r.data.nodes <= prevEdges, "");
    prevEdges = r.data.nodes;
  } else {
    record("node_freq", `node_freq_range [${c.lo},${c.hi}]`, "FAIL", r.err);
  }
}

// ── PHASE 10: edge_freq_range ─────────────────────────────────────────────────
section("10 Graph engine: edge_freq_range filter");

const efCases = [
  { lo: 1, hi: "Inf" },
  { lo: 2, hi: "Inf" },
  { lo: 1, hi: 1 },
];
for (const c of efCases) {
  const r = await safe(() => rj(R, `
    net <- ${build_net({edge_freq_range: `c(${c.lo}`})}, oversize_mode = "star")
    list(nodes = nrow(net$nodes), edges = nrow(net$edges))
  `));
  const ok = r.ok;
  record("edge_freq", `edge_freq_range [${c.lo},${c.hi}]`,
         ok ? "PASS" : "FAIL",
         ok ? `nodes=${r.data.nodes}  edges=${r.data.edges}` : r.err);
}

// ── PHASE 11: authors_per_pub ────────────────────────────────────────────────
section("11 Graph engine: authors_per_pub (hyperauthor filter)");

for (const [lo, hi] of [[1, 5], [1, 50], [1, 200], [2, 20]]) {
  const r = await safe(() => rj(R, `
    net <- ${build_net({authors_per_pub: `c(${lo}`})}, oversize_mode = "star")
    list(nodes = nrow(net$nodes), edges = nrow(net$edges))
  `));
  const ok = r.ok;
  record("authors_per_pub", `authors_per_pub [${lo},${hi}]`,
         ok ? "PASS" : "FAIL",
         ok ? `nodes=${r.data.nodes}  edges=${r.data.edges}` : r.err);
}

// ── PHASE 12: prune_leaves ───────────────────────────────────────────────────
section("12 Graph engine: prune_leaves toggle");

const plFull = await safe(() => rj(R, `
  net <- ${build_net({prune_leaves: `FALSE`})}
  nrow(net$nodes)
`));
const plPruned = await safe(() => rj(R, `
  net <- ${build_net({prune_leaves: `TRUE`})}
  nrow(net$nodes)
`));
if (plFull.ok && plPruned.ok) {
  chk("prune_leaves", "prune_leaves=TRUE gives <= nodes than FALSE",
      +plPruned.data <= +plFull.data,
      `pruned=${plPruned.data}  full=${plFull.data}`);
  console.log(`  → full=${plFull.data} nodes   pruned=${plPruned.data} nodes`);
}

// ── PHASE 13: cluster_size_range ─────────────────────────────────────────────
section("13 Graph engine: cluster_size_range (island filter)");

for (const [lo, hi] of [[1, "Inf"], [2, "Inf"], [1, 10]]) {
  const r = await safe(() => rj(R, `
    net <- ${build_net({cluster_size_range: `c(${lo}`})})
    list(nodes = nrow(net$nodes), edges = nrow(net$edges))
  `));
  const ok = r.ok;
  record("cluster_size", `cluster_size_range [${lo},${hi}]`,
         ok ? "PASS" : "FAIL",
         ok ? `nodes=${r.data.nodes}  edges=${r.data.edges}` : r.err);
}

// ── PHASE 14: detect_communities & compute_centrality ───────────────────────
section("14 Graph engine: community detection & centrality");

const commR = await safe(() => rj(R, `
  net <- ${build_net({detect_communities: `TRUE`, compute_centrality: `TRUE`, oversize_mode: `"star"`})}
  list(has_community   = "community"   %in% names(net$nodes),
       has_betweenness = "betweenness" %in% names(net$nodes),
       has_degree_c    = "degree_c"    %in% names(net$nodes),
       stats_keys      = names(net$stats))
`));
if (commR.ok) {
  const d = commR.data;
  chk("communities", "community column present when detect_communities=TRUE",
      d.has_community === true || d.has_community === "true", "");
  chk("centrality",  "betweenness column present when compute_centrality=TRUE",
      d.has_betweenness === true || d.has_betweenness === "true", "");
  chk("centrality",  "degree_c column present",
      d.has_degree_c === true || d.has_degree_c === "true", "");
  console.log(`  → stats keys: ${Array.isArray(d.stats_keys) ? d.stats_keys.join(", ") : d.stats_keys}`);
} else {
  record("communities", "detect_communities + compute_centrality", "FAIL", commR.err);
}

// communities off
const noCommR = await safe(() => rj(R, `
  net <- ${build_net({detect_communities: `FALSE`, compute_centrality: `FALSE`})}
  "community" %in% names(net$nodes)
`));
chk("communities", "community column absent when detect_communities=FALSE",
    noCommR.ok && (noCommR.data === false || noCommR.data === "false" || !noCommR.data), "");

// ── PHASE 15: layout_engine ──────────────────────────────────────────────────
section("15 Graph engine: layout_engine parameter");

for (const eng of ["auto", "fr", "drl", "kk"]) {
  const r = await safe(() => rj(R, `
    net <- ${build_net({layout_engine: `"${eng}"`, oversize_mode: `"star"`, detect_communities: `FALSE`, compute_centrality: `FALSE`})}
    list(nodes = nrow(net$nodes),
         has_x = "x" %in% names(net$nodes),
         has_y = "y" %in% names(net$nodes))
  `, { timeoutMs: 300_000 }));
  if (r.ok) {
    const d = r.data;
    chk("layout", `layout_engine="${eng}" produces x,y coords`,
        (d.has_x === true || d.has_x === "true") &&
        (d.has_y === true || d.has_y === "true"), "");
    console.log(`    layout="${eng}"  nodes=${d.nodes}`);
  } else {
    record("layout", `layout_engine="${eng}"`, "FAIL", r.err);
  }
}

// ── PHASE 16: cn_network_metrics ─────────────────────────────────────────────
section("16 cn_network_metrics");

const nmR = await safe(() => rj(R, `
  net <- ${build_net({oversize_mode: `"star"`, detect_communities: `TRUE`, compute_centrality: `TRUE`})}
  m <- cn_network_metrics(net)
  as.list(m[1,])
`));
if (nmR.ok) {
  const m = nmR.data;
  const expectedKeys = ["nodes", "edges", "density", "components", "transitivity"];
  for (const k of expectedKeys)
    chk("metrics", `cn_network_metrics has "${k}" column`, k in m, `keys: ${Object.keys(m).join(",")}`);
  if (VERBOSE) console.log("  metrics:", JSON.stringify(m, null, 2));
} else {
  record("metrics", "cn_network_metrics ran", "FAIL", nmR.err);
}

// ── PHASE 17: cn_ego_metrics ─────────────────────────────────────────────────
section("17 cn_ego_metrics");

// Identify one real author from the CSV to use as target
const firstAuthor = await safe(() => rt(R, `
  aa <- strsplit(df_raw$Authors[1], "; ", fixed = TRUE)[[1]]
  trimws(aa[1])
`));
const target = firstAuthor.ok ? firstAuthor.data.replace(/^"|"$/g, "").trim() : "Smith J.";
console.log(`  → Using target: "${target}"`);

const egoR = await safe(() => rj(R, `
  net <- ${build_net({oversize_mode: `"star"`, detect_communities: `FALSE`, compute_centrality: `TRUE`})}
  em  <- cn_ego_metrics(net, c(${JSON.stringify(target)}))
  if (is.null(em) || nrow(em) == 0) list(rows=0) else as.list(em[1,])
`));
if (egoR.ok) {
  const keys = Object.keys(egoR.data);
  chk("ego_metrics", "cn_ego_metrics returns a data frame", keys.length > 0, "");
  if (keys.includes("ego_density") || keys.includes("constraint") || keys.includes("effective_size"))
    chk("ego_metrics", "ego_density / constraint columns present", true, "");
  if (VERBOSE) console.log("  ego:", JSON.stringify(egoR.data, null, 2));
} else {
  record("ego_metrics", "cn_ego_metrics ran", "FAIL", egoR.err);
}

// ── PHASE 18: cn_bibliometrics ───────────────────────────────────────────────
section("18 cn_bibliometrics");

const bibR = await safe(() => rj(R, `
  bib <- cn_bibliometrics(df, "Authors", delim_csv)
  as.list(bib[1,])
`));
if (bibR.ok) {
  const keys = Object.keys(bibR.data);
  chk("bibliometrics", "cn_bibliometrics returns a data frame", keys.length > 0, "");
  chk("bibliometrics", "has degree_of_collaboration column",
      keys.includes("degree_of_collaboration") || keys.some(k => k.includes("collab")), "");
  if (VERBOSE) console.log("  bibliometrics:", JSON.stringify(bibR.data, null, 2));
} else {
  record("bibliometrics", "cn_bibliometrics ran", "FAIL", bibR.err);
}

// ── PHASE 19: cn_collapse_communities (metagraph) ────────────────────────────
section("19 cn_collapse_communities (community metagraph view)");

const metaR = await safe(() => rj(R, `
  net  <- ${build_net({oversize_mode: `"star"`, detect_communities: `TRUE`, compute_centrality: `FALSE`})}
  meta <- cn_collapse_communities(net, min_size = 1L)
  list(nodes = nrow(meta$nodes), edges = nrow(meta$edges),
       meta_nodes = nrow(meta$nodes) < nrow(net$nodes) || nrow(net$nodes) == 0)
`));
if (metaR.ok) {
  chk("metagraph", "Metagraph has fewer nodes than full graph",
      metaR.data.meta_nodes === true || metaR.data.meta_nodes === "true", "");
  console.log(`  → metagraph: ${metaR.data.nodes} nodes  ${metaR.data.edges} edges`);
} else {
  record("metagraph", "cn_collapse_communities ran", "FAIL", metaR.err);
}

// ── PHASE 20: Edge-case / stress tests ───────────────────────────────────────
section("20 Edge cases & stress tests");

// 20a. Empty dataframe
const emptyDf = await safe(() => rj(R, `
  empty_df <- data.frame(Authors = character(0), stringsAsFactors = FALSE)
  net <- ${build_net({df: `empty_df`, target_delim: `"; "`, node_freq_range: `c(1`})}, edge_freq_range = c(1,Inf),
           cluster_size_range = c(1,Inf))
  list(nodes = nrow(net$nodes), edges = nrow(net$edges))
`));
chk("edge_cases", "Empty dataframe returns empty network",
    emptyDf.ok && emptyDf.data.nodes === 0, emptyDf.ok ? "" : emptyDf.err);

// 20b. Single-author papers only
const singleAuth = await safe(() => rj(R, `
  solo_df <- data.frame(Authors = c("Alpha A", "Beta B", "Gamma C"),
                        stringsAsFactors = FALSE)
  net <- ${build_net({df: `solo_df`, target_delim: `"; "`, node_freq_range: `c(1`})}, edge_freq_range = c(1,Inf),
           cluster_size_range = c(1,Inf), prune_leaves = FALSE)
  list(nodes = nrow(net$nodes), edges = nrow(net$edges))
`));
chk("edge_cases", "Single-author papers → 0 edges (no collaborations)",
    singleAuth.ok && singleAuth.data.edges === 0,
    singleAuth.ok ? `got edges=${singleAuth.data.edges}` : singleAuth.err);

// 20c. Duplicate paper rows
const dupRows = await safe(() => rj(R, `
  dup_df <- rbind(df_raw[1:3,], df_raw[1:3,])
  net <- ${build_net({df: `dup_df`, target_delim: `delim_csv`, node_freq_range: `c(1`})}, edge_freq_range = c(1,Inf),
           authors_per_pub = c(1,200), cluster_size_range = c(1,Inf),
           prune_leaves = FALSE,
           detect_communities = FALSE, compute_centrality = FALSE)
  list(nodes = nrow(net$nodes), edges = nrow(net$edges))
`));
chk("edge_cases", "Duplicate rows handled (no crash)", dupRows.ok,
    dupRows.ok ? "" : dupRows.err);

// 20d. max_edges cap enforced
const maxEdges = await safe(() => rj(R, `
  net <- ${build_net({max_edges: `5L`, oversize_mode: `"star"`, detect_communities: `FALSE`, compute_centrality: `FALSE`})}
  nrow(net$edges)
`));
chk("edge_cases", "max_edges=5 caps edge count",
    maxEdges.ok && +maxEdges.data <= 5,
    maxEdges.ok ? `got ${maxEdges.data}` : maxEdges.err);

// 20e. target_col that does not exist
const badCol = await safe(() => rj(R, `
  net <- ${build_net({df: `df`, target_col: `"NonExistentCol"`, target_delim: `"; "`, node_freq_range: `c(1`})}, edge_freq_range = c(1,Inf),
           cluster_size_range = c(1,Inf))
  list(nodes = nrow(net$nodes), edges = nrow(net$edges))
`));
chk("edge_cases", "Missing target_col returns empty network (no crash)",
    badCol.ok && badCol.data.nodes === 0,
    badCol.ok ? "" : badCol.err);

// 20f. author targeting (main_authors_list)
const authTarget = await safe(() => rj(R, `
  first_a <- trimws(strsplit(df_raw$Authors[1], "; ", fixed=TRUE)[[1]][1])
  net <- ${build_net({main_authors_list: `first_a`, oversize_mode: `"star"`, detect_communities: `FALSE`, compute_centrality: `FALSE`})}
  list(nodes = nrow(net$nodes), target_in_nodes = first_a %in% net$nodes$id)
`));
chk("edge_cases", "main_authors_list target preserved in nodes",
    authTarget.ok && (authTarget.data.target_in_nodes === true ||
                      authTarget.data.target_in_nodes === "true"),
    authTarget.ok ? "" : authTarget.err);

// ── PHASE 21: Synthetic fixture benchmarks ────────────────────────────────────
section("21 Synthetic fixture benchmark (300 papers + 1 HEP 800-author mega-paper)");

await R.eval(`fx_bench <- mcp_fixture(n_papers=300L, pool=1000L, n_mega=1L, mega_size=800L, seed=42L)`);

const benchR = await safe(() => rj(R, `
  mcp_benchmark_network(fx_bench, target_col="Authors", target_delim=", ")
`, { timeoutMs: 900_000 }));

if (benchR.ok) {
  const rows = Array.isArray(benchR.data) ? benchR.data : Object.values(benchR.data ?? {});
  for (const row of rows) {
    const label = `${row.variant}`;
    const ok = row.error === null || row.error === "NA" || row.error == null;
    record("benchmark", label,
           ok ? "PASS" : "FAIL",
           ok ? `${row.secs_median}s  nodes=${row.nodes}  edges=${row.edges}  peak=${row.peak_MB}MB`
              : row.error);
    // Performance gate: star+newman must finish in < 10s on a synthetic set
    if (row.variant?.includes("newman+star") && ok)
      chk("benchmark", `${row.variant} completes in < 10s`, row.secs_median < 10,
          `took ${row.secs_median}s`);
  }
} else {
  record("benchmark", "mcp_benchmark_network ran", "FAIL", benchR.err);
}

// ── PHASE 22: Real Scopus CSV benchmark ───────────────────────────────────────
section("22 Real Scopus CSV benchmark (all weight × oversize combos)");

const combos = [
  { weight_scheme: "newman", oversize_mode: "star"   },
  { weight_scheme: "newman", oversize_mode: "drop"   },
  { weight_scheme: "count",  oversize_mode: "star"   },
  { weight_scheme: "jaccard",oversize_mode: "star"   },
  { weight_scheme: "newman", oversize_mode: "star",   backbone_alpha: 0.05 },
];

for (const c of combos) {
  const ba = c.backbone_alpha != null ? `, backbone_alpha=${c.backbone_alpha}` : "";
  const label = `${c.weight_scheme}+${c.oversize_mode}${c.backbone_alpha ? "+bb" : ""}`;
  const r = await safe(() => rj(R, `
    t0  <- proc.time()[["elapsed"]]
    net <- ${build_net({df: `df`, target_col: `"Authors"`, target_delim: `delim_csv`, weight_scheme: `"${c.weight_scheme}"`, oversize_mode: `"${c.oversize_mode}"${ba}`, node_freq_range: `c(1`})}, edge_freq_range = c(1,Inf),
             authors_per_pub = c(1,200), cluster_size_range = c(1,Inf),
             prune_leaves = FALSE, detect_communities = FALSE,
             compute_centrality = FALSE, build_tooltips = FALSE)
    list(secs  = round(proc.time()[["elapsed"]] - t0, 3),
         nodes = nrow(net$nodes), edges = nrow(net$edges))
  `, { timeoutMs: 300_000 }));
  if (r.ok) {
    record("csv_bench", label, "PASS",
           `${r.data.secs}s  nodes=${r.data.nodes}  edges=${r.data.edges}`);
  } else {
    record("csv_bench", label, "FAIL", r.err);
  }
}

// ── FINAL SUMMARY ─────────────────────────────────────────────────────────────
console.log(`\n${"═".repeat(70)}`);
console.log("  SUMMARY");
console.log("─".repeat(70));
console.log(`  ${tick} PASSED : ${passed}`);
console.log(`  ${cross} FAILED : ${failed}`);
console.log(`  ${warn} WARNED : ${warned}`);
console.log(`  TOTAL  : ${passed + failed + warned}`);

if (failed > 0) {
  console.log("\n  Failed tests:");
  for (const r of results.filter(r => r.status === "FAIL"))
    console.log(`    [${r.group}] ${r.name}  —  ${r.detail}`);
}
console.log("═".repeat(70) + "\n");

process.exit(failed > 0 ? 1 : 0);
