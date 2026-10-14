// -----------------------------------------------------------------------------
// webr-session.mjs
//
// One webR (R compiled to WebAssembly) instance, wrapped so that MCP tool calls
// can be serialised against it.
//
// Why a queue: webR is a single WASM instance with a single R interpreter.
// Two concurrent evalR() calls will interleave into the same REPL and corrupt
// each other. MCP clients absolutely will issue parallel tool calls, so every
// entry point funnels through `run()`.
// -----------------------------------------------------------------------------

import { WebR } from "webr";
import path from "node:path";
import fs from "node:fs/promises";

export class WebRSession {
  /**
   * @param {object} opts
   * @param {string} opts.repoPath   Host path to the CollabNET checkout.
   * @param {string} [opts.baseUrl]  webR dist URL or local directory. Leave
   *                                 undefined to use the bundled npm dist.
   * @param {string[]} [opts.repos]  CRAN-for-WASM binary repositories.
   */
  constructor({ repoPath, baseUrl, repos } = {}) {
    this.repoPath = repoPath ? path.resolve(repoPath) : null;
    this.baseUrl = baseUrl;
    this.repos = repos ?? [
      "https://repo.r-wasm.org",
      "https://community.r-multiverse.org",
    ];
    this.webR = null;
    this.shelter = null;
    this.ready = false;
    this.loaded = [];
    this.failed = [];
    this._chain = Promise.resolve();
  }

  /** Serialise every operation against the single R interpreter. */
  run(fn) {
    const next = this._chain.then(fn, fn);
    // Keep the chain alive even if one call rejects.
    this._chain = next.then(
      () => undefined,
      () => undefined,
    );
    return next;
  }

  async init() {
    if (this.ready) return;
    this.webR = new WebR({
      interactive: false,
      ...(this.baseUrl ? { baseUrl: this.baseUrl } : {}),
      repoUrl: this.repos[0],
    });
    await this.webR.init();
    this.shelter = await new this.webR.Shelter();

    // NODEFS gives R direct read access to the checkout without copying.
    // Edits you make on the host are visible to R immediately, which is what
    // makes an edit -> reload -> re-run loop usable.
    if (this.repoPath) {
      await this.webR.FS.mkdir("/collabnet").catch(() => {});
      await this.webR.FS.mount("NODEFS", { root: this.repoPath }, "/collabnet");
    }
    await this.webR.FS.mkdir("/scratch").catch(() => {});

    // A writable working directory. The app writes keys/ at load time and the
    // NODEFS mount should stay read-only in spirit.
    await this.evalRaw(`setwd("/scratch"); dir.create("keys", showWarnings = FALSE)`);

    this.ready = true;
  }

  /** Low-level capture. Returns { stdout, stderr, messages, value, error }. */
  async evalRaw(code, { json = false, timeoutMs = 120000 } = {}) {
    const src = json
      ? `local({\n  .__v <- withVisible(eval(parse(text = ${JSON.stringify(
          code,
        )})))\n  if (.__v$visible) jsonlite::toJSON(.__v$value, auto_unbox = TRUE, null = "null", na = "null", digits = 6, force = TRUE) else NULL\n})`
      : code;

    const exec = (async () => {
      const cap = await this.shelter.captureR(src, {
        withAutoprint: !json,
        captureStreams: true,
        captureConditions: true,
      });
      try {
        const stdout = [];
        const stderr = [];
        const messages = [];
        for (const o of cap.output ?? []) {
          if (o.type === "stdout") stdout.push(o.data);
          else if (o.type === "stderr") stderr.push(o.data);
	  else if (o.type === "message" || o.type === "warning")
            messages.push(`[${o.type}] ${o.data?.message ?? (typeof o.data === 'object' ? JSON.stringify(o.data) : String(o.data))}`);
          else if (o.type === "error")
            messages.push(`[error] ${o.data?.message ?? (typeof o.data === 'object' ? JSON.stringify(o.data) : String(o.data))}`);
        }
        let value = null;
        if (cap.result) {
          const t = await cap.result.type();
          if (t !== "null") {
            try {
              value = await cap.result.toJs({ depth: 0 });
              if (value && value.values && value.values.length === 1)
                value = value.values[0];
            } catch {
              value = null;
            }
          }
        }
        return {
          stdout: stdout.join("\n"),
          stderr: stderr.join("\n"),
          messages,
          value,
          error: null,
        };
      } finally {
        this.shelter.purge();
      }
    })();

    return await withTimeout(exec, timeoutMs, `R evaluation exceeded ${timeoutMs}ms`);
  }

  async eval(code, opts) {
    return this.run(async () => {
      await this.init();
      try {
        return await this.evalRaw(code, opts);
      } catch (e) {
        return {
          stdout: "",
          stderr: "",
          messages: [],
          value: null,
          error: e?.message ?? String(e),
        };
      }
    });
  }

  async installPackages(pkgs) {
    return this.run(async () => {
      await this.init();
      const out = [];
      for (const p of pkgs) {
        try {
          await this.webR.installPackages([p], { quiet: true });
          const chk = await this.evalRaw(
            `as.character(requireNamespace(${JSON.stringify(p)}, quietly = TRUE))`,
          );
          out.push({
            package: p,
            installed: (chk.value ?? "FALSE") === "TRUE",
            note: null,
          });
        } catch (e) {
          out.push({ package: p, installed: false, note: e?.message ?? String(e) });
        }
      }
      return out;
    });
  }

  /**
   * Source the CollabNET R files inside webR.
   *
   * `stripSource` reproduces what buildWASM.R does: it removes source() calls
   * so a file can be loaded standalone. `order` matters because several
   * functions are defined more than once across files (compute_h_index,
   * build_collaboration_network) and the last definition wins.
   */
  async loadCollabnet({
    order = null,
    stripSource = true,
    skipServerUi = true,
  } = {}) {
    return this.run(async () => {
      await this.init();
      if (!this.repoPath) throw new Error("repoPath not configured");

      let files = order;
      if (!files) {
        const all = await fs.readdir(this.repoPath);
        files = all
          .filter((f) => /^GScholarLENS-.*\.R$/.test(f))
          .sort();
        // Force the known-correct order; alphabetical puts PlotGLENS after
        // Graph, which would clobber build_collaboration_network().
        const preferred = [
          "GScholarLENS-DOI2Data.R",
          "GScholarLENS-ORCID2Data.R",
          "GScholarLENS-SCOPUS2Data.R",
          "GScholarLENS-ProcessJCR.R",
          "GScholarLENS-Data2GLENS.R",
          "GScholarLENS-PlotGLENS.R",
          "GScholarLENS-Graph.R",
          "GScholarLENS-helpers.R",
          "GScholarLENS-Fixes.R",
        ];
        files = [
          ...preferred.filter((f) => files.includes(f)),
          ...files.filter((f) => !preferred.includes(f)),
        ];
        if (!skipServerUi) files.push("ui.R", "server.R");
      }

      this.loaded = [];
      this.failed = [];

      for (const f of files) {
        const abs = path.join(this.repoPath, f);
        let text;
        try {
          text = await fs.readFile(abs, "utf8");
        } catch {
          this.failed.push({ file: f, error: "not found" });
          continue;
        }
        if (stripSource) {
          text = text
            .split("\n")
            .filter((l) => !/^\s*source\s*\(/.test(l))
            .join("\n");
        }
        const tmp = `/scratch/.load_${f}`;
        await this.webR.FS.writeFile(tmp, new TextEncoder().encode(text));
        const res = await this.evalRaw(
          `tryCatch({ source(${JSON.stringify(tmp)}, local = FALSE); "OK" },
                    error = function(e) paste0("ERR: ", conditionMessage(e)))`,
        );
        const status = String(res.value ?? "");
        if (status === "OK") this.loaded.push(f);
        else
          this.failed.push({
            file: f,
            error: status || res.stderr || "unknown",
          });
      }

      // The R-side test/benchmark harness.
      const harness = await fs.readFile(
        path.join(path.dirname(new URL(import.meta.url).pathname), "..", "R", "harness.R"),
        "utf8",
      );
      await this.webR.FS.writeFile(
        "/scratch/.harness.R",
        new TextEncoder().encode(harness),
      );
      const h = await this.evalRaw(
        `tryCatch({ source("/scratch/.harness.R", local = FALSE); "OK" },
                  error = function(e) paste0("ERR: ", conditionMessage(e)))`,
      );
      if (String(h.value ?? "") !== "OK")
        this.failed.push({ file: "harness.R", error: String(h.value) });

      return { loaded: this.loaded, failed: this.failed };
    });
  }

  async reset() {
    return this.run(async () => {
      if (this.webR) {
        try {
          await this.webR.close();
        } catch {
          /* ignore */
        }
      }
      this.webR = null;
      this.shelter = null;
      this.ready = false;
      this.loaded = [];
      this.failed = [];
      await this.init();
      return { ok: true };
    });
  }
}

function withTimeout(promise, ms, msg) {
  let t;
  return Promise.race([
    promise.finally(() => clearTimeout(t)),
    new Promise((_, rej) => {
      t = setTimeout(() => rej(new Error(msg)), ms);
    }),
  ]);
}
