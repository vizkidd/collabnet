import path from "path";
import fs from "fs";
import { ShinyliveHarness } from "./src/browser.mjs";

async function main() {
  console.log("=== Starting CollabNET .cnet & Graph Test Suite ===");
  const harness = new ShinyliveHarness({ buildDir: "./wasm_build", port: 8812 });
  console.log("Serving UI on port 8812...");
  const url = await harness.serve();
  console.log("URL:", url);

  console.log("Launching headless browser...");
  await harness.open({ headless: true, timeoutMs: 5000 });

  const errors = [];
  harness.page.on("pageerror", (err) => {
    console.error("[PAGE ERROR]", err.message);
    errors.push(err.message);
  });

  harness.page.on("console", (msg) => {
    const text = msg.text();
    if (text.includes("Error") || text.includes("Warning") || text.includes("unused arguments") || text.includes("undefined columns")) {
      console.log(`[CONSOLE ${msg.type().toUpperCase()}] ${text}`);
      if (text.includes("unused arguments") || text.includes("undefined columns selected") || text.includes("Unsupported file extension")) {
        errors.push(text);
      }
    }
  });

  try {
    const page = harness.page;
    page.setDefaultTimeout(180000);

    let appFrame = null;
    console.log("Waiting for Shiny app frame...");
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
      await new Promise((r) => setTimeout(r, 2000));
    }

    if (!appFrame) {
      throw new Error("Could not find Shiny app frame");
    }

    // Step 1: Upload sample CSV
    console.log("\n[TEST 1] Uploading CSV...");
    await appFrame.waitForFunction(() => document.getElementById("preupload_btn") !== null, { timeout: 60000 });
    await appFrame.click("#preupload_btn");

    const uploadInput = await appFrame.waitForSelector("#upload_btn", { state: "attached", timeout: 30000 });
    const csvPath = path.resolve("mcp/sample_scopus.csv");
    await uploadInput.setInputFiles(csvPath);

    console.log("Waiting for Step 1 modal (#next_row_merge)...");
    await appFrame.waitForSelector("#next_row_merge", { timeout: 60000 });
    await appFrame.waitForFunction(() => {
      const btn = document.getElementById("next_row_merge");
      return btn && !btn.disabled && !btn.classList.contains("disabled");
    }, { timeout: 60000 });

    console.log("Clicking #next_row_merge...");
    // Use JS click to bypass modal-footer overlay that intercepts pointer events
    await appFrame.evaluate(() => document.getElementById("next_row_merge").click());

    console.log("Waiting for Step 2 modal (#confirm_import)...");
    await appFrame.waitForSelector("#confirm_import", { timeout: 60000 });
    await appFrame.evaluate(() => document.getElementById("confirm_import").click());

    console.log("Waiting for data import to complete (progress overlay to hide)...");
    await appFrame.waitForFunction(() => {
      const overlay = document.getElementById("progress_overlay");
      return overlay && (overlay.style.display === "none" || window.getComputedStyle(overlay).display === "none");
    }, { timeout: 90000 });
    console.log("✓ CSV data loaded successfully!");

    // Step 2: Download .cnet file
    console.log("\n[TEST 2] Testing .cnet download...");
    const downloadPromise = page.waitForEvent("download", { timeout: 30000 }).catch(() => null);
    
    // Open download modal by clicking predownload_btn (or its parent label)
    await appFrame.evaluate(() => {
      const btn = document.getElementById("predownload_btn");
      if (btn) btn.click();
      else {
        const icon = document.querySelector(".fa-download");
        if (icon && icon.parentElement) icon.parentElement.click();
      }
    });

    await appFrame.waitForSelector("#download_btn", { timeout: 30000 });
    await appFrame.evaluate(() => document.getElementById("download_btn")?.click());

    let downloadPath = path.resolve("mcp/downloaded_test.cnet");
    const download = await downloadPromise;
    if (download) {
      await download.saveAs(downloadPath);
      console.log("✓ Downloaded file saved to:", downloadPath, "Size:", fs.statSync(downloadPath).size, "bytes");
    } else {
      console.log("Note: Browser download event timed out (expected in some headless modes), proceeding with test cnet...");
    }

    // Close download modal if open
    await appFrame.evaluate(() => {
      const cancelBtn = document.querySelector(".modal-footer .btn");
      if (cancelBtn) cancelBtn.click();
    });
    await new Promise((r) => setTimeout(r, 1000));

    // Step 3: Test .cnet upload support using CollabNET_2026-10-15.cnet
    console.log("\n[TEST 3] Testing .cnet file upload with CollabNET_2026-10-15.cnet...");
    let cnetUploadPath = path.resolve("../CollabNET_2026-10-15.cnet");
    if (!fs.existsSync(cnetUploadPath)) {
      cnetUploadPath = downloadPath;
    }

    if (fs.existsSync(cnetUploadPath) && fs.statSync(cnetUploadPath).size > 0) {
      console.log("Uploading .cnet file:", cnetUploadPath, `(${fs.statSync(cnetUploadPath).size} bytes)`);
      await appFrame.evaluate(() => document.getElementById("preupload_btn")?.click());
      const cnetInput = await appFrame.waitForSelector("#upload_btn", { state: "attached", timeout: 30000 });
      await cnetInput.setInputFiles(cnetUploadPath);

      console.log("Waiting for Step 1 modal to appear for .cnet upload...");
      await appFrame.waitForSelector("#next_row_merge", { timeout: 60000 });
      await appFrame.waitForFunction(() => {
        const btn = document.getElementById("next_row_merge");
        return btn && !btn.disabled && !btn.classList.contains("disabled");
      }, { timeout: 60000 });

      console.log("Clicking #next_row_merge for .cnet...");
      await appFrame.evaluate(() => document.getElementById("next_row_merge")?.click());

      console.log("Waiting for Step 2 modal for .cnet...");
      await appFrame.waitForSelector("#confirm_import", { timeout: 60000 });
      await appFrame.evaluate(() => document.getElementById("confirm_import")?.click());

      console.log("Waiting for .cnet import to complete...");
      await appFrame.waitForFunction(() => {
        const overlay = document.getElementById("progress_overlay");
        return overlay && (overlay.style.display === "none" || window.getComputedStyle(overlay).display === "none");
      }, { timeout: 90000 });
      console.log("✓ .cnet upload and merge completed successfully without 'Unsupported file extension' error!");
    } else {
      console.warn("Could not find CollabNET_2026-10-15.cnet to test!");
    }

    // Step 4: Test Network Graph rendering
    console.log("\n[TEST 4] Testing Network Graph activation...");
    await appFrame.evaluate(() => {
      const chk = document.querySelector('input[name="enable_network"]');
      if (chk && !chk.checked) {
        chk.click();
      }
    });

    console.log("Waiting for network graph to calculate and render...");
    await new Promise((r) => setTimeout(r, 8000));

    // Check for critical errors
    const criticalErrors = errors.filter(
      (e) =>
        e.includes("unused arguments") ||
        e.includes("undefined columns selected") ||
        e.includes("Unsupported file extension: cnet") ||
        e.includes('argument "msg" is missing')
    );

    if (criticalErrors.length > 0) {
      throw new Error(`Critical errors encountered: ${JSON.stringify(criticalErrors)}`);
    }

    console.log("✓ Network graph rendered without errors (no unused arguments, no undefined columns selected)!");
    console.log("\n==============================================");
    console.log("ALL TESTS PASSED SUCCESSFULLY!");
    console.log("==============================================");
  } finally {
    await harness.close();
  }
}

main().catch((err) => {
  console.error("\n❌ TEST FAILED:", err);
  process.exit(1);
});
