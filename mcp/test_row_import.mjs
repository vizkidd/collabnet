import path from "path";
import { ShinyliveHarness } from "./src/browser.mjs";

async function main() {
  const UI = new ShinyliveHarness({ buildDir: "./wasm_build", port: 8792 });
  console.log("Serving UI on port 8792...");
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
      if (i % 10 === 0) console.log(`Waiting for boot... [${i * 2}s elapsed]`);
      await new Promise(r => setTimeout(r, 2000));
    }

    if (!appFrame) {
      throw new Error("Could not find Shiny app frame");
    }

    await appFrame.waitForFunction(() => {
      return document.getElementById("preupload_btn") !== null;
    }, { timeout: 120000 });
    console.log("preupload_btn is ready!");

    // ==========================================
    // 1. FIRST FILE UPLOAD (Initial "New" import)
    // ==========================================
    console.log("\n--- [TEST 1] First File Upload ---");
    await appFrame.click("#preupload_btn");
    const uploadInput1 = await appFrame.waitForSelector("#upload_btn", { state: "attached", timeout: 30000 });
    const csvPath = path.resolve("scopus_export_March2026.csv");
    console.log("Uploading File 1:", csvPath);
    await uploadInput1.setInputFiles(csvPath);

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

    console.log("Clicking #confirm_import (New import)...");
    await appFrame.click("#confirm_import");

    console.log("Waiting for File 1 import to finish...");
    await appFrame.waitForFunction(() => {
      const overlay = document.getElementById("progress_overlay");
      return (!overlay || overlay.style.display === "none") &&
             document.querySelectorAll(".vis-network, table, .shiny-bound-output").length > 0;
    }, { timeout: 180000 });
    console.log("File 1 import finished successfully!");

    // Small pause to let state settle
    await new Promise(r => setTimeout(r, 3000));

    // ==========================================
    // 2. SECOND FILE UPLOAD (Testing "Append")
    // ==========================================
    console.log("\n--- [TEST 2] Second File Upload (Testing Append) ---");
    await appFrame.click("#preupload_btn");
    const uploadInput2 = await appFrame.waitForSelector("#upload_btn", { state: "attached", timeout: 30000 });
    console.log("Uploading File 2...");
    await uploadInput2.setInputFiles(csvPath);

    console.log("Waiting for Step 1 modal (#next_row_merge)...");
    await appFrame.waitForSelector("#next_row_merge", { timeout: 60000 });
    await appFrame.waitForFunction(() => {
      const btn = document.getElementById("next_row_merge");
      return btn && !btn.disabled && !btn.classList.contains("disabled");
    }, { timeout: 60000 });

    console.log("Clicking #next_row_merge...");
    await appFrame.click("#next_row_merge");

    console.log("Waiting for Step 2 Row Merge modal...");
    await appFrame.waitForSelector("#confirm_import", { timeout: 60000 });

    // Check that "Append" and "Merge" are ENABLED!
    const radioStatus = await appFrame.evaluate(() => {
      const appendRadio = document.querySelector('input[name="row_import_type"][value="Append"]');
      const mergeRadio = document.querySelector('input[name="row_import_type"][value="Merge"]');
      const newRadio = document.querySelector('input[name="row_import_type"][value="New"]');
      return {
        appendDisabled: appendRadio ? appendRadio.disabled : null,
        mergeDisabled: mergeRadio ? mergeRadio.disabled : null,
        newDisabled: newRadio ? newRadio.disabled : null
      };
    });
    console.log("Row Import Type Radio status with existing data:", JSON.stringify(radioStatus));

    if (radioStatus.appendDisabled === true) {
      throw new Error("FAILED: 'Append' option is still disabled despite existing data!");
    }
    if (radioStatus.mergeDisabled === true) {
      throw new Error("FAILED: 'Merge' option is still disabled despite existing data!");
    }

    // Select "Append"
    console.log("Selecting 'Append' radio option...");
    await appFrame.evaluate(() => {
      const appendRadio = document.querySelector('input[name="row_import_type"][value="Append"]');
      if (appendRadio) {
        appendRadio.checked = true;
        appendRadio.dispatchEvent(new Event('change', { bubbles: true }));
      }
    });

    // Wait for confirm button to be enabled
    console.log("Checking if #confirm_import is enabled for Append...");
    await appFrame.waitForFunction(() => {
      const btn = document.getElementById("confirm_import");
      return btn && !btn.disabled && !btn.classList.contains("disabled") && btn.style.pointerEvents !== "none";
    }, { timeout: 10000 });
    console.log("#confirm_import is ENABLED and ready to click for Append!");

    console.log("Clicking #confirm_import for Append...");
    await appFrame.click("#confirm_import");

    console.log("Waiting for Append operation to complete...");
    await appFrame.waitForFunction(() => {
      const overlay = document.getElementById("progress_overlay");
      return !overlay || overlay.style.display === "none";
    }, { timeout: 180000 });
    console.log("Append operation completed successfully!");

    console.log("\nALL ROW IMPORT MERGE/APPEND TESTS PASSED!");

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
      throw new Error("FAILED: Detected column type mismatch error during append/merge!");
    }
    console.log("VERIFIED: Zero max() warnings, zero type errors.");
  }
}

main().catch(err => {
  console.error("Test failed with error:", err);
  process.exit(1);
});
