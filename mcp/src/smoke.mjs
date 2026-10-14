#!/usr/bin/env node
// -----------------------------------------------------------------------------
// smoke.mjs — run the whole pipeline once, without an MCP client.
//
//   node src/smoke.mjs
//
// Useful because MCP transport problems and webR problems look identical from
// inside a client. If this passes, the server is fine and any remaining issue
// is configuration.
// -----------------------------------------------------------------------------

import path from "node:path";
import { fileURLToPath } from "node:url";
import { WebRSession } from "./webr-session.mjs";

const HERE = path.dirname(fileURLToPath(import.meta.url));
const REPO = process.env.COLLABNET_REPO ?? path.resolve(HERE, "..", "..");

const log = (...a) => console.log(...a);
const step = async (name, fn) => {
  process.stdout.write(`- ${name} ... `);
  const t = Date.now();
  try {
    const r = await fn();
    console.log(`ok (${Date.now() - t}ms)`);
    return r;
  } catch (e) {
    console.log(`FAILED (${Date.now() - t}ms)\n  ${e.message}`);
    throw e;
  }
};

const R = new WebRSession({ repoPath: REPO });

log(`repo: ${REPO}\n`);

await step("boot webR", () => R.init());

const info = await step("session info", () =>
  R.eval(`c(R.version.string, unname(Sys.info()[["machine"]]))`, { json: true }),
);
log(`  ${info.value}`);

await step("install base deps", () =>
  R.installPackages(["jsonlite", "Matrix", "igraph", "stringi", "stringr", "dplyr", "tidyr", "tibble","future"]),
);

const loaded = await step("source CollabNET", () => R.loadCollabnet());
log(`  loaded: ${loaded.loaded.join(", ")}`);
if (loaded.failed.length) {
  log("  FAILED TO LOAD:");
  for (const f of loaded.failed) log(`    ${f.file}: ${f.error}`);
}

const deps = await step("undeclared :: dependencies", () =>
  R.eval("mcp_undeclared_deps()", { json: true }),
);
log(`  ${deps.value}`);

const tests = await step("invariant tests", () =>
  R.eval("mcp_tests()", { json: true, timeoutMs: 300000 }),
);
log(`  ${tests.value ?? tests.error}`);

await step("fixture: 300 papers + one 800-author paper", () =>
  R.eval("fx <<- mcp_fixture(n_papers = 300L, n_mega = 1L, mega_size = 800L); nrow(fx)", {
    json: true,
  }),
);

const bench = await step("benchmark", () =>
  R.eval("mcp_benchmark_network(fx)", { json: true, timeoutMs: 900000 }),
);
log(`  ${bench.value ?? bench.error}`);

log("\ndone.");
process.exit(0);
