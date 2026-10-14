import path from "path";
import { ShinyliveHarness } from "./src/browser.mjs";

async function main() {
  const UI = new ShinyliveHarness({ buildDir: "./wasm_build", port: 8790 });
  console.log("Serving UI on port 8790...");
  const url = await UI.serve();
  console.log("URL:", url);

  console.log("Opening headless browser...");
  await UI.open({ headless: true, timeoutMs: 300000 });
  
  try {
    console.log("Waiting for webR and Shiny to boot...");
    const page = UI.page;

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

    // Expand the "Fetch" accordion card (starts collapsed with display: none)
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

    console.log("Waiting for ORCID query and futures to execute...");
    // Monitor logs and progress for 30 seconds
    for (let sec = 0; sec < 10; sec++) {
      await new Promise(r => setTimeout(r, 3000));
      const logState = await appFrame.evaluate(() => {
        const overlay = document.getElementById("progress_overlay");
        const progDoi = document.getElementById("prog_doi");
        const logEl = document.getElementById("log_text");
        return {
          overlayVisible: overlay && overlay.style.display !== "none",
          progDoiText: progDoi ? progDoi.innerText : "",
          submitDisabled: document.getElementById("submit_button")?.disabled
        };
      });
      console.log(`Poll [${(sec + 1) * 3}s]:`, JSON.stringify(logState));
    }

    console.log("ORCID fetch phase completed successfully without crashing!");

  } finally {
    console.log("\n=== BROWSER LOGS (Errors & Warnings) ===");
    let hasRlangError = false;
    let hasMsgError = false;
    for (const log of UI.logs) {
      if (log.level === "error" || log.level === "pageerror" || log.level === "warning") {
        console.log(`[${log.level}] ${log.text}`);
        if (log.text.includes("rlang") && log.text.includes("already loaded")) {
          hasRlangError = true;
        }
        if (log.text.includes("argument \"msg\" is missing")) {
          hasMsgError = true;
        }
      }
    }
    await UI.close();

    if (hasRlangError) {
      throw new Error("FAILED: Detected rlang version error in logs!");
    }
    if (hasMsgError) {
      throw new Error("FAILED: Detected argument 'msg' is missing error in logs!");
    }
    console.log("\nSUCCESS: No rlang or msg errors detected!");
  }
}

main().catch(e => {
  console.error("Error:", e);
  process.exit(1);
});
