import path from "path";
import { ShinyliveHarness } from "./src/browser.mjs";

async function main() {
  const UI = new ShinyliveHarness({ buildDir: "./wasm_build", port: 8795 });
  console.log("Serving UI on port 8795...");
  const url = await UI.serve();
  console.log("URL:", url);

  console.log("Opening headless browser...");
  await UI.open({ headless: true, timeoutMs: 300000 });
  
  try {
    console.log("Waiting for webR and Shiny to boot...");
    const page = UI.page;

    let appFrame = null;
    for (let i = 0; i < 90; i++) {
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

    // Check that glens_global_loading element exists in DOM
    const hasLoadingEl = await appFrame.evaluate(() => Boolean(document.getElementById("glens_global_loading")));
    console.log("Global loading screen element exists:", hasLoadingEl);
    if (!hasLoadingEl) throw new Error("glens_global_loading element not found in DOM!");

    // Verify it is NOT active on a fresh start on an empty page
    const freshStartLoading = await appFrame.evaluate(() => {
      const el = document.getElementById("glens_global_loading");
      return {
        hasActiveClass: el ? el.classList.contains("active") : false,
        styleDisplay: el ? window.getComputedStyle(el).display : null
      };
    });
    console.log("Fresh start loading state:", JSON.stringify(freshStartLoading));
    if (freshStartLoading.hasActiveClass || freshStartLoading.styleDisplay !== "none") {
      throw new Error("FAILED: glens_global_loading is active/visible on fresh start on an empty page!");
    }
    console.log("VERIFIED: Global loading screen is properly INACTIVE on fresh start!");

    // Expand the "Fetch" accordion card
    console.log("Expanding Fetch card...");
    await appFrame.evaluate(() => {
      const headers = Array.from(document.querySelectorAll(".custom-card-header"));
      const fetchHeader = headers.find(h => h.innerText.includes("Fetch"));
      if (fetchHeader) fetchHeader.click();
    });

    // Wait for the submit_button to become visible
    console.log("Waiting for #submit_button to be visible...");
    await appFrame.waitForSelector("#submit_button", { state: "visible", timeout: 30000 });
    console.log("#submit_button is ready!");

    // Enter ORCiD: 0000-0002-2861-7446
    console.log("Entering ORCiD: 0000-0002-2861-7446...");
    await appFrame.fill("#orcid_text", "0000-0002-2861-7446");

    // Click Run CollabNET
    console.log("Clicking #submit_button...");
    await appFrame.click("#submit_button");

    console.log("Waiting for ORCID query and processing to complete...");
    let completed = false;
    for (let sec = 0; sec < 40; sec++) {
      await new Promise(r => setTimeout(r, 3000));
      const state = await appFrame.evaluate(() => {
        const overlay = document.getElementById("progress_overlay");
        const submitBtn = document.getElementById("submit_button");
        const logEl = document.getElementById("log_text");
        const summaryTable = document.getElementById("summary_table");
        return {
          overlayDisplay: overlay ? overlay.style.display : "null",
          submitDisabled: submitBtn ? submitBtn.disabled : true,
          hasSummary: summaryTable ? summaryTable.innerText.length : 0,
          logSnippet: logEl ? logEl.innerText.slice(-200) : ""
        };
      });
      console.log(`Poll [${(sec + 1) * 3}s]:`, JSON.stringify(state));
      if (state.overlayDisplay === "none" && !state.submitDisabled) {
        completed = true;
        break;
      }
    }

    if (!completed) {
      console.log("Warning: Timed out waiting for overlay hide, continuing anyway...");
    }

    // Enable Network Graph
    console.log("Enabling Network Graph checkbox...");
    await appFrame.evaluate(() => {
      const chk = document.getElementById("enable_network");
      if (chk && !chk.checked) {
        chk.click();
      }
    });

    // Expand Network Graph card if collapsed
    await appFrame.evaluate(() => {
      const headers = Array.from(document.querySelectorAll(".custom-card-header"));
      const netHeader = headers.find(h => h.innerText.includes("Network Graph"));
      if (netHeader) {
        const content = netHeader.nextElementSibling;
        if (content && (content.style.display === "none" || content.querySelector(".custom-card-content")?.style.display === "none")) {
          const btn = netHeader.querySelector(".btn-minimize");
          if (btn) btn.click();
        }
      }
    });

    // Wait 10s for reactive network calculation to settle
    console.log("Waiting 10s for Network Graph reactive updates...");
    await new Promise(r => setTimeout(r, 10000));

    // Inspect Network state in Shiny
    const netState = await appFrame.evaluate(() => {
      const visEl = document.getElementById("network_filtered");
      const metricsEl = document.getElementById("network_metrics_table");
      const hamEl = document.getElementById("hamiltonian_diagnostics_table");
      const leadEl = document.getElementById("centrality_leaderboard_table");
      const nodeSlider = window.Shiny?.shinyapp?.$inputValues?.node_freq_range;
      const edgeSlider = window.Shiny?.shinyapp?.$inputValues?.edge_freq_range;
      const netCol = window.Shiny?.shinyapp?.$inputValues?.net_col_filtered;
      const isBusy = document.querySelector("html").classList.contains("shiny-busy");
      return {
        visExists: Boolean(visEl),
        visHtmlLength: visEl ? visEl.innerHTML.length : 0,
        metricsRows: metricsEl ? metricsEl.querySelectorAll("tr").length : 0,
        hamRows: hamEl ? hamEl.querySelectorAll("tr").length : 0,
        leadRows: leadEl ? leadEl.querySelectorAll("tr").length : 0,
        nodeSlider,
        edgeSlider,
        netCol,
        isBusy
      };
    });
    console.log("Network State:", JSON.stringify(netState, null, 2));

    if (netState.metricsRows <= 1 && netState.leadRows <= 1) {
      throw new Error("FAILED: Network graph tables did not populate with rows!");
    }
    console.log("\nSUCCESS: Network graph rendered successfully for ORCiD input!");

  } finally {
    console.log("\n=== BROWSER LOGS ===");
    for (const log of UI.logs) {
      if (log.level === "error" || log.level === "pageerror" || log.level === "warning" || log.text.includes("Error") || log.text.includes("Network")) {
        console.log(`[${log.level}] ${log.text}`);
      }
    }
    await UI.close();
  }
}

main().catch(err => {
  console.error("Test failed:", err);
  process.exit(1);
});
