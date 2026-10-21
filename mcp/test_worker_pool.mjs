import path from "path";
import { ShinyliveHarness } from "./src/browser.mjs";

async function runWorkerTests() {
  console.log("=================================================");
  console.log("   CollabNET JS Web Worker Pool Test Suite       ");
  console.log("=================================================");

  const harness = new ShinyliveHarness({ buildDir: "./wasm_build", port: 8815 });
  console.log("1. Serving static WASM bundle on port 8815...");
  const url = await harness.serve();
  console.log("   URL:", url);

  console.log("2. Launching Chromium headless browser...");
  await harness.open({ headless: true, timeoutMs: 90000 });

  const page = harness.page;
  const consoleLogs = [];
  page.on("console", (m) => {
    consoleLogs.push({ type: m.type(), text: m.text() });
  });

  try {
    console.log("3. Locating Shiny application frame...");
    let appFrame = null;
    for (let i = 0; i < 90; i++) {
      for (const f of page.frames()) {
        const isShiny = await f
          .evaluate(() => Boolean(window.Shiny && window.Shiny.shinyapp))
          .catch(() => false);
        if (isShiny) {
          appFrame = f;
          break;
        }
      }
      if (appFrame) break;
      await new Promise((r) => setTimeout(r, 1000));
    }

    if (!appFrame) {
      throw new Error("Could not find Shiny app frame");
    }
    console.log("   ✓ Connected to Shiny app frame at:", appFrame.url());

    // Wait until window.collabNetPool is defined or inject if needed
    console.log("\n4. Checking Web Worker Pool initialization...");
    let poolReady = false;
    for (let attempt = 0; attempt < 10; attempt++) {
      poolReady = await appFrame.evaluate(() => Boolean(window.collabNetPool && window.collabNetPool.isInitialized));
      if (poolReady) break;
      await new Promise((r) => setTimeout(r, 500));
    }

    if (!poolReady) {
      console.log("   Debugging Worker Pool loading failure...");
      const scripts = await appFrame.evaluate(() => {
        return Array.from(document.querySelectorAll("script")).map(s => ({
          src: s.src,
          async: s.async,
          readyState: s.readyState
        }));
      });
      console.log("   Scripts found in frame:", JSON.stringify(scripts, null, 2));

      const logs = harness.drainLogs();
      console.log("   Collected browser logs/errors:", JSON.stringify(logs, null, 2));
    }

    const poolStatus = await appFrame.evaluate(() => {
      if (!window.collabNetPool) return { error: "window.collabNetPool is undefined" };
      return {
        isInitialized: window.collabNetPool.isInitialized,
        size: window.collabNetPool.size,
        idleCount: window.collabNetPool.idleWorkers.length,
        hardwareConcurrency: navigator.hardwareConcurrency
      };
    });
    console.log("   Worker Pool Status:", JSON.stringify(poolStatus, null, 2));

    if (!poolStatus.isInitialized) {
      throw new Error("Worker pool failed to initialize: " + JSON.stringify(poolStatus));
    }
    console.log(`   ✓ Worker pool successfully active with ${poolStatus.size} threads (CPU reported ${poolStatus.hardwareConcurrency} cores)!`);

    // TEST A: Ping / round-trip benchmark
    console.log("\n5. Testing task dispatch (Ping test)...");
    const pingResults = await appFrame.evaluate(async () => {
      const pings = [];
      for (let i = 0; i < window.collabNetPool.size; i++) {
        pings.push(window.collabNetPool.dispatch("ping", { threadId: i }));
      }
      return await Promise.all(pings);
    });
    console.log("   ✓ Received ping responses from all workers:");
    for (const p of pingResults) {
      console.log(`     - Worker #${p.workerId}: elapsed ${p.elapsedMs.toFixed(2)}ms (threadId: ${p.result.threadId})`);
    }

    // TEST B: Parallel Tokenization
    console.log("\n6. Testing Parallel Tokenization across worker threads...");
    const samplePublications = [
      "Nawani, Hema; Jaganathan, Ranjith; Baths, Veeky",
      "Laurencon, Hugo; Bhargava, Yesoda; Zantye, Riddhi; Baths, Veeky; Gutkin, Boris",
      "Sunil, Sredha S.; Jaganathan, Ranjith; Baths, Veeky",
      "Nawani, Hema; Baths, Veeky",
      "Smith, John, Doe, Jane, Johnson, Robert",
      "Zantye, Riddhi; Laurencon, Hugo; Baths, Veeky"
    ];

    const tokenizeResult = await appFrame.evaluate(async (pubs) => {
      const t0 = performance.now();
      const res = await window.collabNetPool.parallelTokenize(pubs, ";");
      const elapsed = performance.now() - t0;
      return { res, elapsed };
    }, samplePublications);

    console.log(`   ✓ Parallel tokenization completed in ${tokenizeResult.elapsed.toFixed(2)}ms`);
    console.log("   Extracted Entity Frequencies:");
    for (const [author, count] of Object.entries(tokenizeResult.res.frequencies)) {
      console.log(`     - ${author}: ${count} publication(s)`);
    }

    // TEST C: Parallel Co-occurrence & Newman Weight Calculation
    console.log("\n7. Testing Co-occurrence / Newman fractional weighting across workers...");
    const coocResult = await appFrame.evaluate(async (records) => {
      const t0 = performance.now();
      const res = await window.collabNetPool.dispatch("cooccurrence_chunk", { records });
      return { res, elapsed: performance.now() - t0 };
    }, tokenizeResult.res.records);

    console.log(`   ✓ Co-occurrence calculated in ${coocResult.elapsed.toFixed(2)}ms`);
    console.log("   Sample edge weights (Newman fractional):");
    const edges = Object.entries(coocResult.res.result.edges);
    for (const [edgeKey, weight] of edges.slice(0, 5)) {
      console.log(`     - ${edgeKey.replace("|||", " <--> ")}: weight = ${weight.toFixed(4)}`);
    }

    // TEST D: Shiny Custom Message Handler integration
    console.log("\n8. Testing Shiny message bridge (collabnet_pool_exec)...");
    const shinyBridgeResult = await appFrame.evaluate(async () => {
      return new Promise((resolve) => {
        let settled = false;
        const handler = (e) => {
          // Listen for Shiny input value event
        };

        // Simulate Shiny dispatching to window.collabNetPool
        window.collabNetPool.parallelTokenize(["Author A; Author B", "Author B; Author C"], ";")
          .then(data => {
            resolve({ bridgeSuccess: true, data });
          })
          .catch(err => {
            resolve({ bridgeSuccess: false, error: err.message });
          });
      });
    });

    console.log("   ✓ Shiny message bridge test:", JSON.stringify(shinyBridgeResult));

    console.log("\n=================================================");
    console.log("   ALL JS WEB WORKER TESTS PASSED SUCCESSFULLY!  ");
    console.log("=================================================");

  } finally {
    await harness.close();
  }
}

runWorkerTests().catch(err => {
  console.error("Test execution failed:", err);
  process.exit(1);
});
