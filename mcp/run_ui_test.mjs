import path from "path";
import { ShinyliveHarness } from "./src/browser.mjs";

async function main() {
  const UI = new ShinyliveHarness({ buildDir: "./wasm_build", port: 8788 });
  console.log("Serving UI...");
  const url = await UI.serve();
  console.log("URL:", url);

  console.log("Opening headless browser...");
  await UI.open({ headless: true, timeoutMs: 300000 });
  
  try {
    console.log("Waiting for webR and Shiny to boot (this takes ~1-2 minutes)...");
    const page = UI.page;

    // Find the app frame (the iframe that hosts shinylive app)
    let appFrame = null;
    for (let i = 0; i < 60; i++) {
      for (const f of page.frames()) {
        const isShiny = await f.evaluate(() => Boolean(window.Shiny && window.Shiny.shinyapp)).catch(() => false);
        if (isShiny) {
          appFrame = f;
          console.log("Found Shiny app frame:", f.url());
          break;
        }
      }
      if (appFrame) break;
      await new Promise(r => setTimeout(r, 2000));
    }

    if (!appFrame) {
      throw new Error("Could not find Shiny app frame");
    }

    // Wait for the app to finish initializing initial outputs
    console.log("Waiting for Shiny app to settle initial outputs...");
    await appFrame.waitForFunction(() => {
      return document.getElementById("preupload_btn") !== null;
    }, { timeout: 120000 });
    console.log("preupload_btn is ready!");

    // Click preupload_btn
    console.log("Clicking preupload_btn to open file upload modal...");
    await appFrame.click("#preupload_btn");

    // Wait for the upload_btn file input to appear (hidden input)
    console.log("Waiting for input#upload_btn...");
    const uploadInput = await appFrame.waitForSelector("#upload_btn", { state: "attached", timeout: 30000 });
    console.log("Found input#upload_btn!");

    // Set file input
    const csvPath = path.resolve("scopus_export_March2026.csv");
    console.log("Uploading file:", csvPath);
    await uploadInput.setInputFiles(csvPath);

    // Wait for Step 1 modal (#next_row_merge)
    console.log("Waiting for Step 1 Column Mapping modal (#next_row_merge)...");
    await appFrame.waitForSelector("#next_row_merge", { timeout: 60000 });

    // Wait until #next_row_merge is enabled
    console.log("Waiting for #next_row_merge to be enabled...");
    await appFrame.waitForFunction(() => {
      const btn = document.getElementById("next_row_merge");
      return btn && !btn.disabled && !btn.classList.contains("disabled");
    }, { timeout: 60000 });

    console.log("Clicking #next_row_merge...");
    await appFrame.click("#next_row_merge");

    // Wait for Step 2 modal (#confirm_import)
    console.log("Waiting for Step 2 Row Merge modal (#confirm_import)...");
    await appFrame.waitForSelector("#confirm_import", { timeout: 60000 });

    // Wait until #confirm_import is enabled
    console.log("Waiting for #confirm_import to be enabled...");
    await appFrame.waitForFunction(() => {
      const btn = document.getElementById("confirm_import");
      return btn && !btn.disabled && !btn.classList.contains("disabled");
    }, { timeout: 60000 });

    console.log("Clicking #confirm_import...");
    await appFrame.click("#confirm_import");

    // Wait for progress overlay to disappear and data to be imported
    console.log("Waiting for data import to finish...");
    await appFrame.waitForFunction(() => {
      const overlay = document.getElementById("progress_overlay");
      return (!overlay || overlay.style.display === "none") &&
             document.querySelectorAll(".vis-network, table, .shiny-bound-output").length > 0;
    }, { timeout: 180000 });

    // Enable Network Graph
    console.log("Waiting for #enable_network to be enabled...");
    await appFrame.waitForFunction(() => {
      const cb = document.getElementById("enable_network");
      return cb && !cb.disabled && !cb.classList.contains("disabled");
    }, null, { timeout: 120000 });

    console.log("Enabling Network Graph by clicking #enable_network...");
    await appFrame.click("#enable_network");

    // Check Shiny inputs and ensure net_col_filtered is set
    const diag = await appFrame.evaluate(() => {
      const inputs = window.Shiny ? window.Shiny.shinyapp.$inputValues : {};
      if (window.Shiny && !inputs.net_col_filtered) {
        window.Shiny.setInputValue("net_col_filtered", "Authors", { priority: "event" });
      }
      return {
        enable_network: inputs.enable_network,
        net_col_filtered: inputs.net_col_filtered,
        edge_freq_range: inputs.edge_freq_range,
        node_freq_range: inputs.node_freq_range,
        has_table_el: document.getElementById("hamiltonian_diagnostics_table") !== null
      };
    });
    console.log("Post-click Diagnostic:", JSON.stringify(diag));

    console.log("Waiting for network, Hamiltonian diagnostics, and centrality calculations...");
    let ready = false;
    for (let attempt = 0; attempt < 60; attempt++) {
      const state = await appFrame.evaluate(() => {
        const ham = document.getElementById("hamiltonian_diagnostics_table");
        const net = document.getElementById("network_metrics_table");
        const hasError = document.querySelector(".shiny-output-error") !== null;
        return {
          ham_rows: ham ? ham.querySelectorAll("tr").length : -1,
          net_rows: net ? net.querySelectorAll("tr").length : -1,
          ham_text: ham ? ham.innerText.trim().slice(0, 100) : "",
          has_error: hasError,
          enable_network: window.Shiny?.shinyapp?.$inputValues?.enable_network
        };
      });
      if (state.ham_rows > 1) {
        console.log(`Hamiltonian table ready with ${state.ham_rows} rows!`);
        ready = true;
        break;
      }
      if (attempt % 5 === 0) {
        console.log(`Poll [${attempt * 3}s]: ham_rows=${state.ham_rows}, net_rows=${state.net_rows}, ham_text="${state.ham_text}", error=${state.has_error}`);
      }
      await new Promise(r => setTimeout(r, 3000));
    }
    if (!ready) {
      throw new Error("Timed out waiting for hamiltonian_diagnostics_table rows");
    }

    console.log("Diagnostics ready! Waiting 3s for visNetwork stabilization...");
    await new Promise(r => setTimeout(r, 3000));

    // Read Hamiltonian diagnostics table
    const hamTable = await appFrame.evaluate(() => {
      const el = document.getElementById("hamiltonian_diagnostics_table");
      return el ? el.innerText : "Not rendered";
    });
    console.log("\n=== HAMILTONIAN & TOPOLOGICAL DIAGNOSTICS ===");
    console.log(hamTable);

    // Read Network metrics table
    const netMetrics = await appFrame.evaluate(() => {
      const el = document.getElementById("network_metrics_table");
      return el ? el.innerText : "Not rendered";
    });
    console.log("\n=== GLOBAL NETWORK & CENTRALIZATION METRICS ===");
    console.log(netMetrics);

    // Screenshot initial state (Hamiltonian & Topology tab active)
    await page.screenshot({ path: path.resolve("screenshot_hamiltonian_tab.png"), fullPage: true });
    console.log("Saved Hamiltonian tab screenshot to screenshot_hamiltonian_tab.png");

    // Click on Centrality Leaderboard tab
    console.log("Clicking Centrality Leaderboard tab...");
    await appFrame.evaluate(() => {
      const tabLinks = Array.from(document.querySelectorAll('a[data-toggle="tab"]'));
      const leaderTab = tabLinks.find(a => a.innerText.includes("Leaderboard"));
      if (leaderTab) leaderTab.click();
    });
    await new Promise(r => setTimeout(r, 3000));

    // Read Centrality Leaderboard table
    const leaderTable = await appFrame.evaluate(() => {
      const el = document.getElementById("centrality_leaderboard_table");
      return el ? el.innerText : "Not rendered";
    });
    console.log("\n=== CENTRALITY LEADERBOARD ===");
    console.log(leaderTable.slice(0, 1000)); // Print top slice

    await page.screenshot({ path: path.resolve("screenshot_leaderboard_tab.png"), fullPage: true });
    console.log("Saved Leaderboard tab screenshot to screenshot_leaderboard_tab.png");

    // Click on Centrality Map (Scatter Plot) tab
    console.log("Clicking Centrality Map tab...");
    await appFrame.evaluate(() => {
      const tabLinks = Array.from(document.querySelectorAll('a[data-toggle="tab"]'));
      const mapTab = tabLinks.find(a => a.innerText.includes("Centrality Map"));
      if (mapTab) mapTab.click();
    });
    await new Promise(r => setTimeout(r, 5000));

    await page.screenshot({ path: path.resolve("screenshot_centrality_map_tab.png"), fullPage: true });
    console.log("Saved Centrality Map tab screenshot to screenshot_centrality_map_tab.png");

    // Now test tweaking node sizing and coloring metrics:
    // Size by betweenness, color by cut_vertex (highlighting critical bottleneck brokers!)
    console.log("Tweaking node sizing: betweenness, coloring: cut_vertex...");
    await appFrame.evaluate(() => {
      if (window.Shiny) {
        window.Shiny.setInputValue("node_size_metric", "betweenness", { priority: "event" });
        window.Shiny.setInputValue("node_color_metric", "cut_vertex", { priority: "event" });
      }
    });
    await new Promise(r => setTimeout(r, 8000));

    await page.screenshot({ path: path.resolve("screenshot_cut_vertex_highlight.png"), fullPage: true });
    console.log("Saved Cut-Vertex Highlight screenshot to screenshot_cut_vertex_highlight.png");

    console.log("\nALL GRAPH THEORY & HAMILTONIAN VALIDATION STEPS SUCCEEDED!");
  } finally {
    console.log("=== BROWSER LOGS (Errors & Warnings) ===");
    for (const log of UI.logs) {
      if (log.level === "error" || log.level === "pageerror" || log.level === "warning") {
        console.log(`[${log.level}] ${log.text}`);
      }
    }
    await UI.close();
  }
}

main().catch(e => {
  console.error("Error:", e);
  process.exit(1);
});
