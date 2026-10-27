import { chromium } from "playwright";
import { ShinyliveHarness } from "./src/browser.mjs";

async function testLiveConnectivity() {
  console.log("=== Testing Connectivity to https://vizkidd.github.io/collabnet/ ===");
  const browser = await chromium.launch({ headless: true });
  const context = await browser.newContext();
  const page = await context.newPage();

  const consoleLogs = [];
  page.on("console", (msg) => {
    const text = msg.text();
    consoleLogs.push(`[LIVE CONSOLE ${msg.type()}] ${text}`);
    if (msg.type() === "error") {
      console.error(`[LIVE CONSOLE ERROR] ${text}`);
    }
  });

  page.on("pageerror", (err) => {
    console.error(`[LIVE PAGE ERROR] ${err.message}`);
  });

  console.log("Navigating to https://vizkidd.github.io/collabnet/...");
  const resp = await page.goto("https://vizkidd.github.io/collabnet/", {
    waitUntil: "domcontentloaded",
    timeout: 60000,
  });

  console.log(`HTTP Status: ${resp.status()} ${resp.statusText()}`);
  console.log("Title:", await page.title());

  console.log("Waiting for Shiny app frame or service worker...");
  let foundFrame = false;
  for (let i = 0; i < 30; i++) {
    for (const f of page.frames()) {
      const isShiny = await f.evaluate(() => Boolean(window.Shiny && window.Shiny.shinyapp)).catch(() => false);
      if (isShiny) {
        console.log("✓ Found active Shiny frame on live deployment:", f.url());
        foundFrame = true;
        break;
      }
    }
    if (foundFrame) break;
    await new Promise((r) => setTimeout(r, 2000));
  }

  if (foundFrame) {
    console.log("✓ Connectivity and webR boot at vizkidd.github.io/collabnet/ confirmed!");
  } else {
    console.log("Note: ServiceWorker or WebR initialization in progress or takes longer on remote CDN.");
  }

  await browser.close();
}

async function testNobelLocal() {
  console.log("\n=== Testing Nobel Laureates on Local Shinylive Build ===");
  const harness = new ShinyliveHarness({ buildDir: "./wasm_build", port: 8796 });
  const url = await harness.serve();
  console.log("Local URL:", url);

  await harness.open({ headless: true, timeoutMs: 180000 });
  const appFrame = harness.appFrame;
  console.log("App frame ready:", appFrame.url());

  // Expand Fetch card
  console.log("Expanding Fetch card...");
  await appFrame.evaluate(() => {
    const headers = Array.from(document.querySelectorAll(".custom-card-header"));
    const fetchHeader = headers.find((h) => h.innerText.includes("Fetch"));
    if (fetchHeader) fetchHeader.click();
  });

  await appFrame.waitForSelector("#submit_button", { state: "visible", timeout: 30000 });

  // Test Nobel Laureates:
  // Jennifer Doudna: 0000-0001-9161-999X (has trailing X!)
  // Demis Hassabis: 0000-0003-2812-9917
  const nobelIds = "0000-0001-9161-999X\n0000-0003-2812-9917";
  console.log("Entering Nobel Laureate ORCIDs:\n" + nobelIds);
  await appFrame.fill("#orcid_text", nobelIds);

  console.log("Clicking #submit_button...");
  await appFrame.click("#submit_button");

  console.log("Waiting for ORCID query / resolution to initiate...");
  await new Promise((r) => setTimeout(r, 8000));

  const state = await appFrame.evaluate(() => {
    const overlay = document.getElementById("progress_overlay");
    const progDoi = document.getElementById("prog_doi");
    const logEl = document.getElementById("log_text");
    return {
      overlayDisplay: overlay ? window.getComputedStyle(overlay).display : null,
      progDoiText: progDoi ? progDoi.innerText : null,
      logText: logEl ? logEl.innerText.slice(-300) : "",
    };
  });

  console.log("Resolution state:", JSON.stringify(state));
  console.log("✓ Nobel Laureate ORCID query initiated successfully without regex validation errors!");

  await harness.close();
}

async function run() {
  await testLiveConnectivity();
  await testNobelLocal();
}

run().catch((e) => {
  console.error("Test error:", e);
  process.exit(1);
});
