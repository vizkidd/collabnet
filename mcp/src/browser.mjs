// -----------------------------------------------------------------------------
// browser.mjs
//
// webR alone cannot run Shiny: shinylive runs Shiny *inside* webR *inside* a
// browser, and needs SharedArrayBuffer, which needs cross-origin isolation.
// So the only way to actually exercise the CollabNET UI is a real browser with
// COOP/COEP headers set.
//
// This module provides:
//   - a static server that sets the two headers shinylive requires
//   - a Playwright driver that waits for the webR runtime to finish booting,
//     collects every console message and page error, and can drive inputs
//
// Playwright is an optionalDependency. If it is absent the tools degrade to
// "server only", which is still useful (you can open the URL yourself).
// -----------------------------------------------------------------------------

import http from "node:http";
import fs from "node:fs";
import fsp from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

const MIME = {
  ".html": "text/html; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".mjs": "text/javascript; charset=utf-8",
  ".css": "text/css; charset=utf-8",
  ".json": "application/json; charset=utf-8",
  ".wasm": "application/wasm",
  ".data": "application/octet-stream",
  ".vfs": "application/octet-stream",
  ".ttf": "font/ttf",
  ".woff2": "font/woff2",
  ".png": "image/png",
  ".svg": "image/svg+xml",
  ".map": "application/json",
};

export class ShinyliveHarness {
  constructor({ buildDir, port = 8788, targetUrl = null } = {}) {
    this.buildDir = buildDir ? path.resolve(buildDir) : null;
    this.port = port;
    this.targetUrl = targetUrl || process.env.COLLABNET_URL || null;
    this.server = null;
    this.browser = null;
    this.page = null;
    this.appFrame = null;
    this.logs = [];
  }

  get url() {
    return this.targetUrl || `http://127.0.0.1:${this.port}/`;
  }

  async serve() {
    if (this.targetUrl) return this.targetUrl;
    if (this.server) return this.url;
    if (!this.buildDir || !fs.existsSync(this.buildDir))
      throw new Error(
        `shinylive build directory not found: ${this.buildDir}. Run buildWASM.R (shinylive::export) first.`,
      );

    this.server = http.createServer(async (req, res) => {
      try {
        const u = new URL(req.url, "http://127.0.0.1");
        let rel = decodeURIComponent(u.pathname);
        if (rel.endsWith("/")) rel += "index.html";
        let abs = path.join(this.buildDir, path.normalize(rel));
        if (!fs.existsSync(abs) && /^\/app_[^/]+\//.test(rel)) {
          const stripped = rel.replace(/^\/app_[^/]+/, "");
          const altAbs = path.join(this.buildDir, path.normalize(stripped));
          if (fs.existsSync(altAbs)) {
            abs = altAbs;
          }
        }
        if (!abs.startsWith(this.buildDir)) {
          res.writeHead(403).end("forbidden");
          return;
        }
        const buf = await fsp.readFile(abs);
        res.writeHead(200, {
          "Content-Type": MIME[path.extname(abs)] ?? "application/octet-stream",
          // Required for SharedArrayBuffer, which webR's synchronous
          // request channel depends on. Without these the app boots to a
          // blank page and the only clue is a console warning.
          "Cross-Origin-Opener-Policy": "same-origin",
          "Cross-Origin-Embedder-Policy": "require-corp",
          "Cross-Origin-Resource-Policy": "cross-origin",
          "Cache-Control": "no-store",
        });
        res.end(buf);
      } catch {
        res.writeHead(404).end("not found");
      }
    });

    await new Promise((resolve, reject) => {
      this.server.once("error", reject);
      this.server.listen(this.port, "127.0.0.1", resolve);
    });
    return this.url;
  }

  async open({ timeoutMs = 180000, headless = true, url = null } = {}) {
    if (url) this.targetUrl = url;
    if (!this.targetUrl) {
      await this.serve();
    }
    let pw;
    try {
      pw = await import("playwright");
    } catch {
      throw new Error(
        "playwright is not installed. Run: npm i playwright && npx playwright install chromium",
      );
    }
    if (this.page && this.appFrame) return { url: this.url, reused: true, booted: await this.isBooted() };

    this.browser = await pw.chromium.launch({
      headless,
      args: ["--enable-features=SharedArrayBuffer", "--no-sandbox", "--disable-setuid-sandbox"],
    });
    const ctx = await this.browser.newContext({
      viewport: { width: 1600, height: 1200 },
      acceptDownloads: true,
    });
    this.page = await ctx.newPage();
    this.logs = [];

    this.page.on("console", (m) =>
      this.logs.push({ t: Date.now(), level: m.type(), text: m.text() }),
    );
    this.page.on("pageerror", (e) =>
      this.logs.push({ t: Date.now(), level: "pageerror", text: e.message }),
    );
    this.page.on("requestfailed", (r) =>
      this.logs.push({
        t: Date.now(),
        level: "requestfailed",
        text: `${r.url()} :: ${r.failure()?.errorText}`,
      }),
    );

    await this.page.goto(this.url, { waitUntil: "domcontentloaded", timeout: 90000 });

    // In Shinylive, the app can be rendered in top frame or an iframe.
    // Poll all frames until Shiny.shinyapp is found.
    const deadline = Date.now() + timeoutMs;
    this.appFrame = null;
    while (Date.now() < deadline && !this.appFrame) {
      for (const f of this.page.frames()) {
        const isShiny = await f
          .evaluate(() => Boolean(window.Shiny && window.Shiny.shinyapp))
          .catch(() => false);
        if (isShiny) {
          this.appFrame = f;
          break;
        }
      }
      if (!this.appFrame) {
        await new Promise((r) => setTimeout(r, 1000));
      }
    }

    if (!this.appFrame) {
      this.appFrame = this.page;
    }

    return { url: this.url, reused: false, booted: await this.isBooted() };
  }

  async isBooted() {
    const target = this.appFrame || this.page;
    if (!target) return false;
    return await target
      .evaluate(() => Boolean(window.Shiny?.shinyapp?.$socket || window.Shiny?.shinyapp))
      .catch(() => false);
  }

  async setInput(id, value) {
    const target = this.appFrame || this.page;
    if (!target) throw new Error("browser not open");
    return await target.evaluate(
      ([i, v]) => {
        if (!window.Shiny) return "no Shiny";
        window.Shiny.setInputValue(i, v, { priority: "event" });
        return "ok";
      },
      [id, value],
    );
  }

  async readOutput(id) {
    const target = this.appFrame || this.page;
    if (!target) throw new Error("browser not open");
    return await target
      .locator(`#${CSS.escape(id)}`)
      .innerText({ timeout: 15000 })
      .catch((e) => `<<${e.message}>>`);
  }

  async uploadFile(filePath) {
    const target = this.appFrame || this.page;
    if (!target) throw new Error("browser not open");
    const absPath = path.resolve(filePath);
    if (!fs.existsSync(absPath)) throw new Error(`File not found: ${absPath}`);

    // Click upload modal trigger
    await target.waitForFunction(() => document.getElementById("preupload_btn") !== null, { timeout: 30000 });
    await target.click("#preupload_btn");

    const input = await target.waitForSelector("#upload_btn", { state: "attached", timeout: 30000 });
    await input.setInputFiles(absPath);

    // Step 1: Wait for next_row_merge button to be active
    await target.waitForSelector("#next_row_merge", { timeout: 60000 });
    await target.waitForFunction(() => {
      const btn = document.getElementById("next_row_merge");
      return btn && !btn.disabled && !btn.classList.contains("disabled");
    }, { timeout: 60000 });
    await target.click("#next_row_merge");

    // Step 2: Confirm import
    await target.waitForSelector("#confirm_import", { timeout: 60000 });
    await target.click("#confirm_import");

    // Wait for data import to complete
    await target.waitForFunction(() => {
      const overlay = document.getElementById("progress_overlay");
      return overlay && (overlay.style.display === "none" || window.getComputedStyle(overlay).display === "none");
    }, { timeout: 120000 });

    return { status: "success", file: path.basename(absPath), size: fs.statSync(absPath).size };
  }

  async fetchRecords({ orcid = "", doi = "", flow = "new" } = {}) {
    const target = this.appFrame || this.page;
    if (!target) throw new Error("browser not open");
    if (orcid) await this.setInput("orcid_text", orcid);
    if (doi) await this.setInput("doi_text", doi);
    if (flow) await this.setInput("post_fetch_action_choice", flow);

    await target.click("#submit_button");
    await target.waitForFunction(() => {
      const overlay = document.getElementById("progress_overlay");
      return overlay && (overlay.style.display === "none" || window.getComputedStyle(overlay).display === "none");
    }, { timeout: 180000 });

    return { status: "success", orcid, doi, flow };
  }

  async getNetworkMetrics() {
    const target = this.appFrame || this.page;
    if (!target) throw new Error("browser not open");
    return await target.evaluate(() => {
      const summaryTable = document.getElementById("network_summary_table");
      const summaryText = summaryTable ? summaryTable.innerText : "";
      const bibTable = document.getElementById("bibliometrics_table");
      const bibText = bibTable ? bibTable.innerText : "";
      return {
        summary: summaryText,
        bibliometrics: bibText,
      };
    });
  }

  async exportSession(outPath) {
    if (!this.page) throw new Error("browser not open");
    const target = this.appFrame || this.page;
    const downloadPromise = this.page.waitForEvent("download", { timeout: 30000 });
    await target.evaluate(() => {
      const btn = document.getElementById("predownload_btn");
      if (btn) btn.click();
    });
    const dlBtn = await target.waitForSelector("#download_btn", { timeout: 30000 });
    await dlBtn.click();
    const download = await downloadPromise;
    const dest = path.resolve(outPath);
    await download.saveAs(dest);
    return dest;
  }

  async screenshot(outPath, { fullPage = true, selector = null } = {}) {
    if (!this.page) throw new Error("browser not open");
    await fsp.mkdir(path.dirname(outPath), { recursive: true });
    if (selector) await this.page.locator(selector).screenshot({ path: outPath });
    else await this.page.screenshot({ path: outPath, fullPage });
    return outPath;
  }

  /** Time a UI interaction end to end, the way a user would feel it. */
  async timeInteraction(fn, { settleMs = 400, maxMs = 300000 } = {}) {
    const t0 = Date.now();
    await fn();
    await this.page.waitForFunction(
      (ms) => {
        const now = Date.now();
        window.__lastBusy = document.documentElement.classList.contains(
          "shiny-busy",
        )
          ? now
          : window.__lastBusy ?? 0;
        return now - (window.__lastBusy || 0) > ms;
      },
      settleMs,
      { timeout: maxMs, polling: 100 },
    );
    return Date.now() - t0;
  }

  drainLogs() {
    const l = this.logs;
    this.logs = [];
    return l;
  }

  async close() {
    if (this.browser) await this.browser.close().catch(() => {});
    if (this.server) await new Promise((r) => this.server.close(r));
    this.browser = null;
    this.page = null;
    this.server = null;
  }
}

export const __dirname_of = (u) => path.dirname(fileURLToPath(u));
