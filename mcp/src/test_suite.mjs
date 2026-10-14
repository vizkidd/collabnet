#!/usr/bin/env node
// =============================================================================
// test_suite.mjs  —  CollabNET comprehensive parameter & regression test suite
//
// Usage:
//   node src/test_suite.mjs [--verbose] [--csv path/to/file.csv]
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

async function rj(R, code, opts = {}) {
  const r = await R.eval(code, { json: true, timeoutMs: opts.timeoutMs ?? 120_000 });
  if (r.error) throw new Error(r.error);
  if (r.messages?.some(m => m.startsWith("[error]")))
    throw new Error(r.messages.filter(m => m.startsWith("[error]")).join("; "));
  if (typeof r.value === "string") {
    try { return JSON.parse(r.value); } catch { return r.value; }
  }
  return r.value;
}

async function rt(R, code, opts = {}) {
  const r = await R.eval(code, { json: false, timeoutMs: opts.timeoutMs ?? 120_000 });
  if (r.error) throw new Error(r.error);
  return (r.stdout + (r.value != null ? String(r.value) : "")).trim();
}

async function safe(fn) {
  try { return { ok: true, data: await fn() }; }
  catch (e) { return { ok: false, err: e.message }; }
}

function chk(group, name, condition, failDetail = "") {
  record(group, name, condition ? "PASS" : "FAIL", condition ? "" : failDetail);
}

function chkApprox(group, name, actual, expected, tol = 1e-4) {
  const ok = Math.abs(actual - expected) <= tol;
  record(group, name, ok ? "PASS" : "FAIL", ok ? "" : `got ${actual}, expected ${expected} ±${tol}`);
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
    .map(([k, v]) => k === "df" ? v : `${k} = ${v}`)
    .join(", ");
  return `build_collaboration_network(${args})`;
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
section("1  Session bootstrap & package installation");

await R.init();
const inst = await safe(() => R.installPackages(["jsonlite", "Matrix", "igraph", "stringr", "dplyr", "tidyr", "tibble", "future"]));
if (inst.ok) {
  record("bootstrap", "Packages installed", "PASS");
} else {
  record("bootstrap", "Package install failed", "FAIL", inst.err);
}

const loaded = await R.loadCollabnet();
const loadedFiles = loaded.loaded ?? [];
const failedFiles = loaded.failed ?? [];

chk("bootstrap", "All 9 helper files loaded", loadedFiles.length >= 9, `only ${loadedFiles.length} loaded`);
chk("bootstrap", "No files failed to load", failedFiles.length === 0, failedFiles.map(f => `${f.file}: ${f.error}`).join("; "));
chk("bootstrap", "Graph.R loads after PlotGLENS.R", loadedFiles.indexOf("GScholarLENS-Graph.R") > loadedFiles.indexOf("GScholarLENS-PlotGLENS.R"), "Order issue");
chk("bootstrap", "Fixes.R loads last", loadedFiles[loadedFiles.length - 2] === "GScholarLENS-Fixes.R" || loadedFiles[loadedFiles.length - 1] === "GScholarLENS-Fixes.R", "Order issue");

// ── PHASE 2: Dependency scan ──────────────────────────────────────────────────
section("2  Package dependency scan (WASM availability)");

const undecl = await safe(() => rj(R, "mcp_undeclared_deps()", { timeoutMs: 30_000 }));
if (undecl.ok) {
  const u = Array.isArray(undecl.data) ? undecl.data : [];
  chk("deps", "No undeclared :: dependencies", u.length === 0, u.length ? u.join(", ") : "");
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

chk("csv", "CSV loaded without error", csvLoad.ok && !csvLoad.data.startsWith("ERR"), csvLoad.ok ? csvLoad.data : csvLoad.err);

let csvRows = 0, csvCols = 0, csvColNames = [];
if (csvLoad.ok && !csvLoad.data.startsWith("ERR")) {
  const m = csvLoad.data.match(/rows=(\d+) cols=(\d+) colnames=(.+)/);
  if (m) {
    csvRows = +m[1]; csvCols = +m[2];
    csvColNames = m[3].split("|");
  }
  console.log(`  → ${csvRows} rows × ${csvCols} cols`);
}

const expectedCols = ["Authors", "Title", "Year", "DOI"];
for (const col of expectedCols) {
  chk("csv", `Column "${col}" present`, csvColNames.includes(col), `available: ${csvColNames.slice(0,8).join(", ")}…`);
}

// Normalise to standard format for the graph engine
const normSetup = await safe(() => rt(R, `
  delim_csv <- if (any(grepl(";", df_raw$Authors))) "; " else ", "
  df <- df_raw
  names(df)[names(df) == "Cited by"] <- "Citations"
  names(df)[names(df) == "Source title"] <- "Journal"
  nrow(df)
`));
chk("csv", "DataFrame normalised for graph engine", normSetup.ok, normSetup.ok ? "" : normSetup.err);

// ── PHASE 5: cn_slider_bounds ────────────────────────────────────────────────
section("5  cn_slider_bounds (UI initialisation)");

const bounds = await safe(() => rj(R, `cn_slider_bounds(df, target_col = "Authors", target_delim = delim_csv)`));
if (bounds.ok) {
  const b = bounds.data;
  chk("slider_bounds", "max_node_occurrence >= 1", +b.max_node_occurrence >= 1, "");
  chk("slider_bounds", "max_edge_frequency >= 1",  +b.max_edge_frequency  >= 1, "");
} else {
  record("slider_bounds", "cn_slider_bounds returned", "FAIL", bounds.err);
}

// ── PHASE 6: Graph engine — weight schemes ───────────────────────────────────
section("6  Graph engine: weight_scheme parameter");

for (const scheme of ["newman", "count", "jaccard"]) {
  const r = await safe(() => rj(R, `
    net <- ${build_net({weight_scheme: `"${scheme}"`, oversize_mode: '"star"'})}
    list(nodes = nrow(net$nodes), edges = nrow(net$edges), has_weight = "weight" %in% names(net$edges), sample_w = if (nrow(net$edges) > 0) round(net$edges$weight[1], 4) else NA)
  `, { timeoutMs: 180_000 }));
  if (r.ok) {
    chk("weight_scheme", `weight_scheme="${scheme}" builds without error`, r.data.nodes >= 0, "");
  } else {
    record("weight_scheme", `weight_scheme="${scheme}"`, "FAIL", r.err);
  }
}

const wtCmp = await safe(() => rj(R, `
  d2 <- data.frame(Authors = c("A One; B Two", "A One; B Two; C Three"), stringsAsFactors = FALSE)
  n <- ${build_net({df: "d2", target_delim: '"; "', weight_scheme: '"newman"'})}
  c_net <- ${build_net({df: "d2", target_delim: '"; "', weight_scheme: '"count"'})}
  ab_n <- n$edges[(n$edges$from=="A One" & n$edges$to=="B Two") | (n$edges$to=="A One" & n$edges$from=="B Two"),]
  ab_c <- c_net$edges[(c_net$edges$from=="A One" & c_net$edges$to=="B Two") | (c_net$edges$to=="A One" & c_net$edges$from=="B Two"),]
  list(newman_AB = if(nrow(ab_n)>0) round(ab_n$weight[1],4) else NA, count_AB = if(nrow(ab_c)>0) ab_c$connections[1] else NA)
`));
if (wtCmp.ok) {
  chkApprox("weight_scheme", "Newman AB weight == 1.5", +wtCmp.data.newman_AB, 1.5, 0.001);
  chk("weight_scheme", "Raw count AB == 2", wtCmp.data.count_AB == 2, `got ${wtCmp.data.count_AB}`);
}

// ── PHASE 7: oversize_mode ───────────────────────────────────────────────────
section("7  Graph engine: oversize_mode parameter");

await R.eval(`
  big_df <- data.frame(
    Authors = c(paste(paste0("Normal", 1:5), collapse = "; "), paste(paste0("Normal", 6:10), collapse = "; "), paste(paste0("Mega", 1:400), collapse = "; ")),
    stringsAsFactors = FALSE)
`);

for (const mode of ["star", "drop", "clique"]) {
  const r = await safe(() => rj(R, `
    s <- ${build_net({df: "big_df", target_delim: '"; "', oversize_mode: `"${mode}"`})}
    list(nodes = nrow(s$nodes), edges = nrow(s$edges))
  `, { timeoutMs: 180_000 }));
  if (r.ok) {
    if (mode === "star") chk("oversize_mode", `star: edge count <= 400`, r.data.edges <= 400, `got ${r.data.edges}`);
    else if (mode === "drop") chk("oversize_mode", `drop: 400-author paper excluded`, r.data.nodes < 400, `got ${r.data.nodes}`);
    else chk("oversize_mode", `clique: builds without crash`, r.data.nodes > 0, "");
  } else {
    record("oversize_mode", `oversize_mode="${mode}"`, "FAIL", r.err);
  }
}

// ── PHASE 8: backbone_alpha ───────────────────────────────────────────────────
section("8  Graph engine: backbone_alpha");

for (const alpha of [null, 0.10, 0.05, 0.01]) {
  const alphaArg = alpha === null ? "NULL" : String(alpha);
  const r = await safe(() => rj(R, `
    net <- ${build_net({backbone_alpha: alphaArg})}
    list(nodes = nrow(net$nodes), edges = nrow(net$edges))
  `, { timeoutMs: 180_000 }));
  if (r.ok) record("backbone", `backbone alpha=${alphaArg}`, "PASS", `edges=${r.data.edges}`);
  else record("backbone", `backbone alpha=${alphaArg}`, "FAIL", r.err);
}

// ── PHASE 9: node_freq_range ─────────────────────────────────────────────────
section("9  Graph engine: node_freq_range filter");

for (const c of [{lo: 1, hi: "Inf"}, {lo: 2, hi: "Inf"}]) {
  const r = await safe(() => rj(R, `
    net <- ${build_net({node_freq_range: `c(${c.lo}, ${c.hi})`})}
    list(nodes = nrow(net$nodes), edges = nrow(net$edges))
  `));
  if (r.ok) record("node_freq", `node_freq_range [${c.lo},${c.hi}]`, "PASS", `nodes=${r.data.nodes}`);
  else record("node_freq", `node_freq_range [${c.lo},${c.hi}]`, "FAIL", r.err);
}

// ── PHASE 10: edge_freq_range ─────────────────────────────────────────────────
section("10 Graph engine: edge_freq_range filter");

for (const c of [{lo: 1, hi: "Inf"}, {lo: 2, hi: "Inf"}]) {
  const r = await safe(() => rj(R, `
    net <- ${build_net({edge_freq_range: `c(${c.lo}, ${c.hi})`})}
    list(nodes = nrow(net$nodes), edges = nrow(net$edges))
  `));
  if (r.ok) record("edge_freq", `edge_freq_range [${c.lo},${c.hi}]`, "PASS", `edges=${r.data.edges}`);
}

// ── PHASE 11: authors_per_pub ────────────────────────────────────────────────
section("11 Graph engine: authors_per_pub");

for (const [lo, hi] of [[1, 5], [1, 200]]) {
  const r = await safe(() => rj(R, `
    net <- ${build_net({authors_per_pub: `c(${lo}, ${hi})`})}
    list(nodes = nrow(net$nodes), edges = nrow(net$edges))
  `));
  if (r.ok) record("authors_per_pub", `authors_per_pub [${lo},${hi}]`, "PASS", `nodes=${r.data.nodes}`);
}

// ── PHASE 12: prune_leaves ───────────────────────────────────────────────────
section("12 Graph engine: prune_leaves toggle");

const plFull = await safe(() => rj(R, `nrow(${build_net({prune_leaves: "FALSE"})}$nodes)`));
const plPruned = await safe(() => rj(R, `nrow(${build_net({prune_leaves: "TRUE"})}$nodes)`));
if (plFull.ok && plPruned.ok) {
  chk("prune_leaves", "prune_leaves=TRUE gives <= nodes than FALSE", +plPruned.data <= +plFull.data, "");
}

// ── PHASE 13: cluster_size_range ─────────────────────────────────────────────
section("13 Graph engine: cluster_size_range");

const clus = await safe(() => rj(R, `nrow(${build_net({cluster_size_range: "c(2, Inf)"})}$nodes)`));
if (clus.ok) record("cluster_size", `cluster_size_range [2,Inf]`, "PASS", `nodes=${clus.data}`);

// ── PHASE 14: detect_communities & compute_centrality ───────────────────────
section("14 Graph engine: community detection & centrality");

const commR = await safe(() => rj(R, `
  net <- ${build_net({detect_communities: "TRUE", compute_centrality: "TRUE"})}
  list(has_community = "community" %in% names(net$nodes), has_betweenness = "betweenness" %in% names(net$nodes))
`));
if (commR.ok) {
  chk("communities", "community column present", commR.data.has_community === true || commR.data.has_community === "true", "");
} else record("communities", "detect_communities", "FAIL", commR.err);

// ── PHASE 15: layout_engine ──────────────────────────────────────────────────
section("15 Graph engine: layout_engine parameter");

for (const eng of ["auto", "fr"]) {
  const r = await safe(() => rj(R, `
    net <- ${build_net({layout_engine: `"${eng}"`})}
    list(has_x = "x" %in% names(net$nodes), has_y = "y" %in% names(net$nodes))
  `, { timeoutMs: 300_000 }));
  if (r.ok) chk("layout", `layout_engine="${eng}"`, (r.data.has_x === true || r.data.has_x === "true"), "");
  else record("layout", `layout_engine="${eng}"`, "FAIL", r.err);
}

// ── PHASE 16: cn_network_metrics ─────────────────────────────────────────────
section("16 cn_network_metrics");

const nmR = await safe(() => rj(R, `
  net <- ${build_net({detect_communities: "TRUE", compute_centrality: "TRUE"})}
  as.list(cn_network_metrics(net)[1,])
`));
if (nmR.ok) chk("metrics", `cn_network_metrics has "nodes" column`, "nodes" in nmR.data, "");
else record("metrics", "cn_network_metrics ran", "FAIL", nmR.err);

// ── PHASE 17: cn_ego_metrics ─────────────────────────────────────────────────
section("17 cn_ego_metrics");

const egoR = await safe(() => rj(R, `
  first_a <- trimws(strsplit(df_raw$Authors[1], "; ", fixed=TRUE)[[1]][1])
  net <- ${build_net({compute_centrality: "TRUE"})}
  em <- cn_ego_metrics(net, c(first_a))
  if (is.null(em) || nrow(em) == 0) list(rows=0) else as.list(em[1,])
`));
if (egoR.ok) chk("ego_metrics", "cn_ego_metrics returns a data frame", Object.keys(egoR.data).length > 0, "");

// ── PHASE 18: cn_bibliometrics ───────────────────────────────────────────────
section("18 cn_bibliometrics");

const bibR = await safe(() => rj(R, `as.list(cn_bibliometrics(df, "Authors", delim_csv)[1,])`));
if (bibR.ok) chk("bibliometrics", "cn_bibliometrics returns a data frame", Object.keys(bibR.data).length > 0, "");

// ── PHASE 19: cn_collapse_communities ────────────────────────────────────────────
section("19 cn_collapse_communities");

const metaR = await safe(() => rj(R, `
  net <- ${build_net({detect_communities: "TRUE"})}
  meta <- cn_collapse_communities(net, min_size = 1L)
  nrow(meta$nodes) <= nrow(net$nodes)
`));
if (metaR.ok) chk("metagraph", "Metagraph has fewer nodes than full graph", metaR.data === true || metaR.data === "true", "");

// ── PHASE 20: Edge-case / stress tests ───────────────────────────────────────
section("20 Edge cases & stress tests");

const emptyDf = await safe(() => rj(R, `
  empty_df <- data.frame(Authors = character(0), stringsAsFactors = FALSE)
  nrow(${build_net({df: "empty_df", target_delim: '"; "'})}$nodes)
`));
chk("edge_cases", "Empty dataframe returns empty network", emptyDf.ok && emptyDf.data === 0, emptyDf.err);

const maxEdges = await safe(() => rj(R, `nrow(${build_net({max_edges: "5L"})}$edges)`));
chk("edge_cases", "max_edges=5 caps edge count", maxEdges.ok && +maxEdges.data <= 5, maxEdges.err);

// ── PHASE 21: Synthetic fixture benchmarks ────────────────────────────────────
section("21 Synthetic fixture benchmark");

await R.eval(`fx_bench <- mcp_fixture(n_papers=100L, pool=200L, n_mega=0L)`);
const benchR = await safe(() => rj(R, `mcp_benchmark_network(fx_bench, target_col="Authors", target_delim=", ")`, { timeoutMs: 300_000 }));
if (benchR.ok) {
  const rows = Array.isArray(benchR.data) ? benchR.data : Object.values(benchR.data ?? {});
  for (const row of rows) {
    const ok = row.error === null || row.error === "NA" || row.error == null;
    record("benchmark", row.variant, ok ? "PASS" : "FAIL", ok ? `${row.secs_median}s` : row.error);
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
