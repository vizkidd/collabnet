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
  constructor({ buildDir, port = 8788 } = {}) {
    this.buildDir = buildDir ? path.resolve(buildDir) : null;
    this.port = port;
    this.server = null;
    this.browser = null;
    this.page = null;
    this.logs = [];
  }

  get url() {
    return `http://127.0.0.1:${this.port}/`;
  }

  async serve() {
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
        const abs = path.join(this.buildDir, path.normalize(rel));
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

  async open({ timeoutMs = 180000, headless = true } = {}) {
    await this.serve();
    let pw;
    try {
      pw = await import("playwright");
    } catch {
      throw new Error(
        "playwright is not installed. Run: npm i playwright && npx playwright install chromium",
      );
    }
    if (this.page) return { url: this.url, reused: true };

    this.browser = await pw.chromium.launch({
      headless,
      args: ["--enable-features=SharedArrayBuffer", "--no-sandbox", "--disable-setuid-sandbox"],
    });
    const ctx = await this.browser.newContext({
      viewport: { width: 1600, height: 1200 },
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

    await this.page.goto(this.url, { waitUntil: "domcontentloaded", timeout: 60000 });

    // shinylive marks the document busy until the Shiny session connects.
    // Waiting on a real Shiny output rather than a timer avoids screenshotting
    // the loading spinner.
    await this.page
      .waitForFunction(
        () =>
          window.Shiny &&
          window.Shiny.shinyapp &&
          document.querySelectorAll(".shiny-bound-output").length > 0,
        null,
        { timeout: timeoutMs },
      )
      .catch(() => {
        /* report via logs instead of throwing */
      });

    return { url: this.url, reused: false, booted: await this.isBooted() };
  }

  async isBooted() {
    if (!this.page) return false;
    return await this.page
      .evaluate(() => Boolean(window.Shiny?.shinyapp?.$socket || window.Shiny?.shinyapp))
      .catch(() => false);
  }

  async setInput(id, value) {
    if (!this.page) throw new Error("browser not open");
    return await this.page.evaluate(
      ([i, v]) => {
        if (!window.Shiny) return "no Shiny";
        window.Shiny.setInputValue(i, v, { priority: "event" });
        return "ok";
      },
      [id, value],
    );
  }

  async readOutput(id) {
    if (!this.page) throw new Error("browser not open");
    return await this.page
      .locator(`#${CSS.escape(id)}`)
      .innerText({ timeout: 15000 })
      .catch((e) => `<<${e.message}>>`);
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
