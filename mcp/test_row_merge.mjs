import path from "path";
import { ShinyliveHarness } from "./src/browser.mjs";

async function main() {
  const UI = new ShinyliveHarness({ buildDir: "./wasm_build", port: 8793 });
  console.log("Serving UI on port 8793...");
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

    await appFrame.waitForFunction(() => {
      return document.getElementById("preupload_btn") !== null;
    }, { timeout: 120000 });
    console.log("preupload_btn is ready!");

    // 1. Initial file upload
    console.log("\n--- [TEST 1] Initial File Upload ---");
    await appFrame.click("#preupload_btn");
    const uploadInput1 = await appFrame.waitForSelector("#upload_btn", { state: "attached", timeout: 30000 });
    const csvPath = path.resolve("scopus_export_March2026.csv");
    await uploadInput1.setInputFiles(csvPath);

    await appFrame.waitForSelector("#next_row_merge", { timeout: 60000 });
    await appFrame.waitForFunction(() => {
      const btn = document.getElementById("next_row_merge");
      return btn && !btn.disabled && !btn.classList.contains("disabled");
    }, { timeout: 60000 });
    await appFrame.click("#next_row_merge");

    await appFrame.waitForSelector("#confirm_import", { timeout: 60000 });
    await appFrame.waitForFunction(() => {
      const btn = document.getElementById("confirm_import");
      return btn && !btn.disabled && !btn.classList.contains("disabled");
    }, { timeout: 60000 });
    await appFrame.click("#confirm_import");

    await appFrame.waitForFunction(() => {
      const overlay = document.getElementById("progress_overlay");
      return (!overlay || overlay.style.display === "none") &&
             document.querySelectorAll(".vis-network, table, .shiny-bound-output").length > 0;
    }, { timeout: 180000 });
    console.log("Initial file imported successfully!");

    await new Promise(r => setTimeout(r, 3000));

    // 2. Second upload with Merge
    console.log("\n--- [TEST 2] Second File Upload (Testing Merge) ---");
    await appFrame.click("#preupload_btn");
    const uploadInput2 = await appFrame.waitForSelector("#upload_btn", { state: "attached", timeout: 30000 });
    await uploadInput2.setInputFiles(csvPath);

    await appFrame.waitForSelector("#next_row_merge", { timeout: 60000 });
    await appFrame.waitForFunction(() => {
      const btn = document.getElementById("next_row_merge");
      return btn && !btn.disabled && !btn.classList.contains("disabled");
    }, { timeout: 60000 });
    await appFrame.click("#next_row_merge");

    await appFrame.waitForSelector("#confirm_import", { timeout: 60000 });

    // Select "Merge"
    console.log("Selecting 'Merge' radio option...");
    await appFrame.evaluate(() => {
      const mergeRadio = document.querySelector('input[name="row_import_type"][value="Merge"]');
      if (mergeRadio) {
        mergeRadio.checked = true;
        mergeRadio.dispatchEvent(new Event('change', { bubbles: true }));
      }
    });

    // Wait for row_merge_ui to paint (selectizeInput for join keys)
    console.log("Waiting for row_merge_keys selectize and confirm button to unlock...");
    await appFrame.waitForSelector("#row_merge_keys", { state: "attached", timeout: 30000 });
    await appFrame.waitForFunction(() => {
      const btn = document.getElementById("confirm_import");
      return btn && !btn.disabled && !btn.classList.contains("disabled") && btn.style.pointerEvents !== "none";
    }, { timeout: 15000 });
    console.log("Merge keys rendered and confirm_import is ready!");

    // Confirm Merge
    console.log("Clicking #confirm_import for Merge...");
    await appFrame.click("#confirm_import");

    // Wait for completion
    console.log("Waiting for Merge operation to finish...");
    await appFrame.waitForFunction(() => {
      const overlay = document.getElementById("progress_overlay");
      return !overlay || overlay.style.display === "none";
    }, { timeout: 180000 });
    console.log("Merge operation completed successfully!");

    console.log("\nALL ROW MERGE TESTS PASSED!");

  } finally {
    console.log("\n=== LOG SUMMARY (Checking for warnings/errors) ===");
    let hasMaxWarning = false;
    let hasTypeError = false;
    for (const log of UI.logs) {
      if (log.text.includes("returning -Inf") || log.text.includes("no non-missing arguments to max")) {
        console.error(`[DETECTED WARNING] ${log.text}`);
        hasMaxWarning = true;
      }
      if (log.text.includes("Can't combine") || log.text.includes("Can't join")) {
        console.error(`[DETECTED TYPE ERROR] ${log.text}`);
        hasTypeError = true;
      }
    }
    await UI.close();

    if (hasMaxWarning) {
      throw new Error("FAILED: Detected 'no non-missing arguments to max; returning -Inf' warning in console!");
    }
    if (hasTypeError) {
      throw new Error("FAILED: Detected column type mismatch error during merge!");
    }
    console.log("VERIFIED: Zero max() warnings, zero type errors.");
  }
}

main().catch(err => {
  console.error("Test failed with error:", err);
  process.exit(1);
});
