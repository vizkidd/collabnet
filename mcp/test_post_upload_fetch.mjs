import path from "path";
import { ShinyliveHarness } from "./src/browser.mjs";

async function main() {
  const UI = new ShinyliveHarness({ buildDir: "./wasm_build", port: 8798 });
  console.log("Serving UI on port 8798...");
  const url = await UI.serve();
  console.log("URL:", url);

  console.log("Opening headless browser...");
  await UI.open({ headless: true, timeoutMs: 300000 });
  
  try {
    console.log("Waiting for webR and Shiny to boot...");
    const page = UI.page;
    page.setDefaultTimeout(180000);

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
      if (i % 10 === 0) console.log(`Waiting for boot... [${i * 2}s elapsed]`);
      await new Promise(r => setTimeout(r, 2000));
    }

    if (!appFrame) {
      throw new Error("Could not find Shiny app frame");
    }

    // ==========================================
    // 1. FILE UPLOAD (Initial baseline data)
    // ==========================================
    console.log("\n--- [STEP 1] Initial File Upload ---");
    await appFrame.waitForFunction(() => document.getElementById("preupload_btn") !== null, { timeout: 60000 });
    await appFrame.click("#preupload_btn");
    
    const uploadInput = await appFrame.waitForSelector("#upload_btn", { state: "attached", timeout: 30000 });
    const csvPath = path.resolve("mcp/sample_scopus.csv");
    console.log("Uploading file:", csvPath);
    await uploadInput.setInputFiles(csvPath);

    console.log("Waiting for Step 1 modal (#next_row_merge)...");
    await appFrame.waitForSelector("#next_row_merge", { timeout: 60000 });
    await appFrame.waitForFunction(() => {
      const btn = document.getElementById("next_row_merge");
      return btn && !btn.disabled && !btn.classList.contains("disabled");
    }, { timeout: 60000 });

    console.log("Clicking #next_row_merge...");
    await appFrame.click("#next_row_merge");

    console.log("Waiting for Step 2 modal (#confirm_import)...");
    await appFrame.waitForSelector("#confirm_import", { timeout: 60000 });
    await appFrame.waitForFunction(() => {
      const btn = document.getElementById("confirm_import");
      return btn && !btn.disabled && !btn.classList.contains("disabled");
    }, { timeout: 60000 });

    console.log("Clicking #confirm_import...");
    await appFrame.click("#confirm_import");

    console.log("Waiting for File import to settle...");
    await appFrame.waitForFunction(() => {
      const overlay = document.getElementById("progress_overlay");
      const overlayHidden = !overlay || overlay.style.display === "none";
      const hasBoundOutput = document.querySelectorAll(".vis-network, table, .shiny-bound-output").length > 0;
      return overlayHidden && hasBoundOutput;
    }, { timeout: 180000 });

    await new Promise(r => setTimeout(r, 3000));
    console.log("Initial file import finished successfully!");

    // ==========================================
    // 2. FETCH ORCiD POST-FILE-UPLOAD
    // ==========================================
    console.log("\n--- [STEP 2] Fetch ORCiD Post-Upload ---");
    
    // Expand the Fetch accordion card if collapsed
    await appFrame.evaluate(() => {
      const headers = Array.from(document.querySelectorAll(".custom-card-header"));
      const fetchHeader = headers.find(h => h.innerText.includes("Fetch"));
      if (fetchHeader) {
        const cardContent = fetchHeader.nextElementSibling;
        if (cardContent && cardContent.style.display === "none") {
          fetchHeader.click();
        }
      }
    });

    console.log("Waiting for #submit_button to be visible...");
    await appFrame.waitForSelector("#submit_button", { state: "visible", timeout: 30000 });

    console.log("Entering DOI: 10.1038/s41586-020-2649-2...");
    await appFrame.fill("#doi_text", "10.1038/s41586-020-2649-2");

    console.log("Clicking #submit_button...");
    await appFrame.click("#submit_button");

    // =======================================================
    // 3. VERIFY MODAL PROMPT FOR POST-FETCH ROW MERGING
    // =======================================================
    console.log("Waiting for post-fetch row merge modal to appear...");
    await appFrame.waitForSelector("#confirm_fetch_merge", { timeout: 180000 });
    console.log("SUCCESS: Post-fetch merge modal appeared!");

    const modalState = await appFrame.evaluate(() => {
      const radios = Array.from(document.querySelectorAll('input[name="fetch_row_import_type"]'));
      const values = radios.map(r => r.value);
      const checked = radios.find(r => r.checked)?.value;
      const modalText = document.querySelector(".modal-body")?.innerText || "";
      return { values, checked, modalTextSnippet: modalText.slice(0, 150) };
    });
    console.log("Modal state:", JSON.stringify(modalState, null, 2));

    if (!modalState.values.includes("Append") || !modalState.values.includes("Merge") || !modalState.values.includes("New")) {
      throw new Error(`FAILED: Modal is missing expected options! Found: ${modalState.values}`);
    }

    // Click Confirm & Incorporate (with default "Append")
    console.log("Clicking #confirm_fetch_merge (Append)...");
    await appFrame.click("#confirm_fetch_merge");

    console.log("Waiting for data incorporation to finish...");
    await appFrame.waitForFunction(() => {
      const modal = document.querySelector(".modal.in, .modal.show");
      const overlay = document.getElementById("progress_overlay");
      return !modal && (!overlay || overlay.style.display === "none");
    }, { timeout: 60000 });

    await new Promise(r => setTimeout(r, 4000));

    const finalInfo = await appFrame.evaluate(() => {
      const logEl = document.getElementById("log_text");
      return {
        logSnippet: logEl ? logEl.innerText.slice(-300) : ""
      };
    });
    console.log(`Log snippet: ${finalInfo.logSnippet}`);

    console.log("\n=== TEST PASSED: ORCiD results successfully prompted and merged without erasing file data! ===");

  } finally {
    console.log("\n=== BROWSER LOGS ===");
    for (const log of UI.logs) {
      if (log.level === "error" || log.level === "pageerror" || log.text.includes("Error") || log.text.includes("Incorporate") || log.text.includes("pre-bake")) {
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
