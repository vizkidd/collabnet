/**
 * CollabNET Web Worker Pool Architecture
 * JS-Driven Multicore/Multithreaded Parallelism for WebAssembly Shiny Applications
 *
 * Implements a thread pool over navigator.hardwareConcurrency using Blob-based Web Workers,
 * offloading intensive data tokenisation, parallel API fetching, and graph metric calculations
 * from the single-threaded webR engine.
 */

(function(window) {
  'use strict';

  // Worker code bundle instantiated via Blob to guarantee zero CORS/path friction
  const WORKER_SCRIPT = `
    self.onmessage = async function(e) {
      const { jobId, task, payload } = e.data;
      const startTime = performance.now();

      try {
        let result;
        switch (task) {
          case 'ping':
            result = { pong: true, threadId: payload.threadId, timestamp: Date.now() };
            break;

          case 'tokenize_chunk': {
            // payload: { chunk: Array<{ id: number, raw: string }>, targetDelim: string }
            const { chunk, targetDelim } = payload;
            const isStandard = !targetDelim || !targetDelim.trim() || /[,;]/.test(targetDelim);
            const tokenMap = {};
            const pairs = [];

            for (let i = 0; i < chunk.length; i++) {
              const item = chunk[i];
              if (!item.raw) continue;
              const text = String(item.raw).trim();
              if (!text) continue;

              let tokens;
              if (isStandard) {
                if (text.includes(';')) {
                  tokens = text.split(';');
                } else if (text.includes(',')) {
                  tokens = text.split(',');
                } else {
                  tokens = [text];
                }
              } else {
                tokens = text.split(targetDelim);
              }

              const cleanedTokens = [];
              for (let j = 0; j < tokens.length; j++) {
                const tok = tokens[j].replace(/\\s+/g, ' ').trim();
                if (tok) {
                  cleanedTokens.push(tok);
                  tokenMap[tok] = (tokenMap[tok] || 0) + 1;
                }
              }

              if (cleanedTokens.length > 0) {
                pairs.push({ id: item.id, entities: cleanedTokens });
              }
            }

            result = {
              frequencies: tokenMap,
              records: pairs,
              count: chunk.length
            };
            break;
          }

          case 'cooccurrence_chunk': {
            // payload: { records: Array<{ id: number, entities: string[] }> }
            // Calculates pairwise co-occurrences and Newman fractional weights
            const { records } = payload;
            const edgeWeights = {};

            for (let r = 0; r < records.length; r++) {
              const ents = records[r].entities;
              const k = ents.length;
              if (k < 2) continue;
              const weight = 1.0 / (k - 1.0); // Newman fractional co-authorship weight

              for (let i = 0; i < k; i++) {
                for (let j = i + 1; j < k; j++) {
                  const a = ents[i];
                  const b = ents[j];
                  const key = a < b ? a + '|||' + b : b + '|||' + a;
                  edgeWeights[key] = (edgeWeights[key] || 0) + weight;
                }
              }
            }

            result = { edges: edgeWeights };
            break;
          }

          case 'fetch_doi_chunk': {
            // payload: { dois: string[], timeoutMs: number }
            const { dois, timeoutMs = 15000 } = payload;
            const results = [];
            const sleep = ms => new Promise(r => setTimeout(r, ms));

            for (let d = 0; d < dois.length; d++) {
              const rawDoi = dois[d].trim().replace(/^https?:\\/\\/(dx\\.)?doi\\.org\\//i, '');
              if (!rawDoi) continue;

              let fetchedOk = false;
              let lastStatus = 0;
              let lastError = null;

              // Step 1: OpenAlex with 429 / transient retry & backoff
              const openAlexUrl = 'https://api.openalex.org/works/https://doi.org/' + encodeURIComponent(rawDoi);
              for (let attempt = 0; attempt < 3 && !fetchedOk; attempt++) {
                try {
                  const controller = new AbortController();
                  const timeoutId = setTimeout(() => controller.abort(), timeoutMs);
                  const resp = await fetch(openAlexUrl, { signal: controller.signal });
                  clearTimeout(timeoutId);
                  lastStatus = resp.status;

                  if (resp.ok) {
                    const data = await resp.json();
                    const authors = (data.authorships || [])
                      .map(a => a.author ? a.author.display_name : null)
                      .filter(Boolean)
                      .join('; ');

                    results.push({
                      doi: rawDoi,
                      success: true,
                      title: data.title || '',
                      authors: authors,
                      year: data.publication_year || null,
                      citations: data.cited_by_count || 0,
                      source: data.primary_location?.source?.display_name || ''
                    });
                    fetchedOk = true;
                    break;
                  } else if (resp.status === 429 || resp.status >= 500) {
                    const retrySec = parseInt(resp.headers?.get('retry-after') || '0', 10);
                    const backoff = retrySec > 0 ? (retrySec + 1) * 1000 : Math.min(1000 * Math.pow(2, attempt), 4000);
                    await sleep(backoff);
                  } else {
                    // Non-retryable HTTP error (e.g. 404) -> break to Crossref fallback
                    break;
                  }
                } catch (err) {
                  lastError = err.message;
                  await sleep(500 * (attempt + 1));
                }
              }

              // Step 2: Crossref Fallback if OpenAlex failed or was rate-limited
              if (!fetchedOk) {
                try {
                  const crossrefUrl = 'https://api.crossref.org/works/' + encodeURIComponent(rawDoi);
                  const controller = new AbortController();
                  const timeoutId = setTimeout(() => controller.abort(), timeoutMs);
                  const resp = await fetch(crossrefUrl, {
                    headers: { 'User-Agent': 'CollabNET/1.0 (https://vizkidd.github.io/collabnet/)' },
                    signal: controller.signal
                  });
                  clearTimeout(timeoutId);
                  lastStatus = resp.status;

                  if (resp.ok) {
                    const crData = await resp.json();
                    const msg = crData.message || {};
                    const authors = (msg.author || [])
                      .map(a => a.family ? (a.given ? a.family + ', ' + a.given : a.family) : (a.name || ''))
                      .filter(Boolean)
                      .join('; ');
                    const year = msg['published-print']?.['date-parts']?.[0]?.[0] ||
                                 msg['published-online']?.['date-parts']?.[0]?.[0] ||
                                 msg.created?.['date-parts']?.[0]?.[0] || null;

                    results.push({
                      doi: rawDoi,
                      success: true,
                      title: Array.isArray(msg.title) ? (msg.title[0] || '') : (msg.title || ''),
                      authors: authors,
                      year: year ? parseInt(year, 10) : null,
                      citations: msg['is-referenced-by-count'] || 0,
                      source: Array.isArray(msg['container-title']) ? (msg['container-title'][0] || '') : (msg['container-title'] || '')
                    });
                    fetchedOk = true;
                  }
                } catch (crErr) {
                  lastError = crErr.message;
                }
              }

              if (!fetchedOk) {
                results.push({ doi: rawDoi, success: false, status: lastStatus, error: lastError || 'All resolvers failed' });
              }

              // Friendly pacing delay between sequential requests in the worker
              if (d < dois.length - 1) {
                await sleep(50);
              }
            }

            result = { fetched: results };
            break;
          }

          default:
            throw new Error('Unknown worker task: ' + task);
        }

        self.postMessage({
          jobId,
          success: true,
          result,
          elapsedMs: performance.now() - startTime
        });
      } catch (err) {
        self.postMessage({
          jobId,
          success: false,
          error: err.message,
          elapsedMs: performance.now() - startTime
        });
      }
    };
  `;

  class WorkerPool {
    constructor(options = {}) {
      const concurrency = navigator.hardwareConcurrency || 4;
      // Clamp pool size between 2 and 6 workers for optimal memory and CPU balance
      this.size = options.size || Math.max(2, Math.min(concurrency, 6));
      this.workers = [];
      this.idleWorkers = [];
      this.jobQueue = [];
      this.activeJobs = new Map();
      this.jobCounter = 0;
      this.blobUrl = null;
      this.isInitialized = false;

      this._init();
    }

    _init() {
      try {
        const blob = new Blob([WORKER_SCRIPT], { type: 'application/javascript' });
        this.blobUrl = URL.createObjectURL(blob);

        for (let i = 0; i < this.size; i++) {
          const worker = new Worker(this.blobUrl);
          worker.workerId = i;
          worker.onmessage = this._handleMessage.bind(this, worker);
          worker.onerror = this._handleError.bind(this, worker);
          this.workers.push(worker);
          this.idleWorkers.push(worker);
        }

        this.isInitialized = true;
        console.log(`[CollabNET] Web Worker Pool initialized with ${this.size} worker threads.`);
      } catch (e) {
        console.warn('[CollabNET] Web Worker initialization failed; falling back to main thread.', e);
        this.isInitialized = false;
      }
    }

    _handleMessage(worker, event) {
      const { jobId, success, result, error, elapsedMs } = event.data;
      const job = this.activeJobs.get(jobId);

      this.idleWorkers.push(worker);
      this.activeJobs.delete(jobId);
      this._drainQueue();

      if (job) {
        if (success) {
          job.resolve({ result, elapsedMs, workerId: worker.workerId });
        } else {
          job.reject(new Error(error || 'Worker execution failed'));
        }
      }
    }

    _handleError(worker, error) {
      console.error(`[CollabNET Worker #${worker.workerId} Error]`, error);
    }

    _drainQueue() {
      while (this.idleWorkers.length > 0 && this.jobQueue.length > 0) {
        const worker = this.idleWorkers.shift();
        const nextJob = this.jobQueue.shift();
        this.activeJobs.set(nextJob.jobId, nextJob);
        worker.postMessage({
          jobId: nextJob.jobId,
          task: nextJob.task,
          payload: nextJob.payload
        });
      }
    }

    /**
     * Dispatch a single job to the next available worker
     */
    dispatch(task, payload) {
      if (!this.isInitialized) {
        return Promise.reject(new Error('Worker pool not available'));
      }

      const jobId = ++this.jobCounter;
      return new Promise((resolve, reject) => {
        this.jobQueue.push({ jobId, task, payload, resolve, reject });
        this._drainQueue();
      });
    }

    /**
     * Parallel Map-Reduce across all pool workers
     * Splits an array of items into equal chunks, processes them in parallel across workers,
     * and combines the results using a reducer function.
     */
    async mapReduce(items, task, options = {}, reducer) {
      if (!items || items.length === 0) return [];
      const numWorkers = Math.min(this.size, items.length);
      const chunkSize = Math.ceil(items.length / numWorkers);
      const promises = [];

      for (let i = 0; i < numWorkers; i++) {
        const chunk = items.slice(i * chunkSize, (i + 1) * chunkSize);
        if (chunk.length > 0) {
          promises.push(this.dispatch(task, Object.assign({}, options, { chunk })));
        }
      }

      const responses = await Promise.all(promises);
      const results = responses.map(r => r.result);

      if (typeof reducer === 'function') {
        return reducer(results);
      }
      return results;
    }

    /**
     * Parallel Tokenisation: splits an array of strings in parallel chunks
     */
    async parallelTokenize(items, targetDelim = ',') {
      const formattedItems = items.map((raw, idx) => ({ id: idx + 1, raw }));
      
      const reducer = (chunkResults) => {
        const mergedFrequencies = {};
        const mergedRecords = [];
        for (const res of chunkResults) {
          for (const [token, count] of Object.entries(res.frequencies)) {
            mergedFrequencies[token] = (mergedFrequencies[token] || 0) + count;
          }
          for (const rec of res.records) {
            mergedRecords.push(rec);
          }
        }
        return { frequencies: mergedFrequencies, records: mergedRecords };
      };

      return this.mapReduce(formattedItems, 'tokenize_chunk', { targetDelim }, reducer);
    }

    /**
     * Parallel DOI Fetcher: resolves a list of DOIs across worker pool threads
     */
    async parallelFetchDois(doiList, timeoutMs = 15000) {
      const numWorkers = Math.min(this.size, doiList.length);
      const chunkSize = Math.ceil(doiList.length / numWorkers);
      const promises = [];

      for (let i = 0; i < numWorkers; i++) {
        const dois = doiList.slice(i * chunkSize, (i + 1) * chunkSize);
        if (dois.length > 0) {
          promises.push(this.dispatch('fetch_doi_chunk', { dois, timeoutMs }));
        }
      }

      const responses = await Promise.all(promises);
      const merged = [];
      for (const r of responses) {
        if (r.result && r.result.fetched) {
          merged.push(...r.result.fetched);
        }
      }
      return merged;
    }

    /**
     * Power-On Self-Test (POST): Machine precision & thread warmup
     */
    async runPost(verbose = true) {
      const startTime = performance.now();
      const eps = Number.EPSILON;
      const expectedEps = 2.220446049250313e-16;
      const epsPass = Math.abs(eps - expectedEps) < 1e-12 &&
                      ((1.0 + (eps / 2.0)) === 1.0) &&
                      ((1.0 + eps) > 1.0);

      // Float64Array and subnormal arithmetic sanity
      const f64 = new Float64Array(2);
      f64[0] = 1.0;
      f64[1] = eps;
      const f64Pass = (f64[0] + f64[1]) > 1.0;
      const subnormalPass = Number.MIN_VALUE > 0 && isFinite(1 / Number.MAX_VALUE);

      // Concurrent thread dispatch & warmup
      const pings = [];
      const pingStart = performance.now();
      for (let i = 0; i < this.size; i++) {
        pings.push(this.dispatch('ping', { threadId: i }));
      }
      const pingResponses = await Promise.all(pings);
      const pingTotalMs = performance.now() - pingStart;
      const avgPingMs = pingTotalMs / Math.max(1, this.size);

      const allPassed = epsPass && f64Pass && subnormalPass && pingResponses.length === this.size;
      const totalElapsed = performance.now() - startTime;

      this.postDiagnostics = {
        passed: allPassed,
        elapsedMs: totalElapsed,
        machinePrecision: {
          epsilon: eps,
          compliant: epsPass,
          float64Compliant: f64Pass,
          subnormalCompliant: subnormalPass
        },
        workers: {
          size: this.size,
          concurrency: navigator.hardwareConcurrency || 4,
          avgRoundTripMs: avgPingMs,
          allResponsive: pingResponses.length === this.size
        },
        timestamp: Date.now()
      };

      if (verbose) {
        console.info(
          `[CollabNET Client POST] Machine Prec: eps=${eps.toExponential(4)} (${epsPass ? 'PASS' : 'FAIL'}), ` +
          `Workers: ${this.size} active (${avgPingMs.toFixed(2)}ms avg latency) -> ${allPassed ? 'OPERATIONAL' : 'DEGRADED'}`
        );
      }

      return this.postDiagnostics;
    }

    destroy() {
      for (const w of this.workers) {
        w.terminate();
      }
      if (this.blobUrl) {
        URL.revokeObjectURL(this.blobUrl);
      }
      this.workers = [];
      this.idleWorkers = [];
      this.activeJobs.clear();
      this.isInitialized = false;
    }
  }

  // Instantiate singleton instance
  window.collabNetPool = new WorkerPool();
  // Warm up workers and machine precision in the background
  window.collabNetPool.runPost().catch(err => {
    console.warn('[CollabNET Client POST] Warmup warning:', err);
  });

  // -------------------------------------------------------------------------
  // Shiny bridge: safe accessor that works in both native Shiny and shinylive
  // WASM where the Shiny object loads asynchronously inside an iframe.
  // -------------------------------------------------------------------------
  function _getShiny() {
    // In shinylive, Shiny lives on the iframe's window but is also
    // propagated to the parent. Try the direct reference first.
    if (typeof Shiny !== 'undefined' && typeof Shiny.setInputValue === 'function') {
      return Shiny;
    }
    if (window.Shiny && typeof window.Shiny.setInputValue === 'function') {
      return window.Shiny;
    }
    return null;
  }

  function _safeSetInputValue(name, value, opts) {
    var s = _getShiny();
    if (s) {
      s.setInputValue(name, value, opts || {});
    } else {
      console.warn('[CollabNET] Shiny.setInputValue not yet available; queuing "' + name + '".');
      // Retry once after a short delay (Shiny may still be initialising)
      setTimeout(function() {
        var s2 = _getShiny();
        if (s2) s2.setInputValue(name, value, opts || {});
      }, 2000);
    }
  }

  function _bindShinyHandlers() {
    var S = _getShiny();
    if (!S || typeof S.addCustomMessageHandler !== 'function') return false;

    // Guard against double-binding
    if (window._collabNetShinyBound) return true;
    window._collabNetShinyBound = true;

    // Receive task from Shiny R server
    S.addCustomMessageHandler('collabnet_pool_exec', async function(msg) {
      var taskId = msg.taskId, action = msg.action, data = msg.data, options = msg.options;
      try {
        var result;
        if (action === 'tokenize') {
          result = await window.collabNetPool.parallelTokenize(data, (options && options.delim) || ',');
        } else if (action === 'fetch_dois') {
          result = await window.collabNetPool.parallelFetchDois(data, (options && options.timeoutMs) || 15000);
        } else {
          result = await window.collabNetPool.dispatch(action, data);
        }

        _safeSetInputValue('collabnet_pool_result', {
          taskId: taskId,
          success: true,
          data: result
        }, { priority: 'event' });
      } catch (err) {
        _safeSetInputValue('collabnet_pool_result', {
          taskId: taskId,
          success: false,
          error: err.message
        }, { priority: 'event' });
      }
    });

    // Send worker pool readiness, core count, and client-side POST diagnostics
    window.collabNetPool.runPost().then(function(diag) {
      _safeSetInputValue('collabnet_pool_ready', {
        cores: navigator.hardwareConcurrency || 4,
        workers: window.collabNetPool.size,
        postDiagnostics: diag
      });
    }).catch(function() {
      _safeSetInputValue('collabnet_pool_ready', {
        cores: navigator.hardwareConcurrency || 4,
        workers: window.collabNetPool.size
      });
    });

    console.info('[CollabNET] Shiny message handlers bound successfully.');
    return true;
  }

  // Strategy 1: Try immediately (native Shiny, already loaded)
  if (!_bindShinyHandlers()) {
    // Strategy 2: Wait for DOMContentLoaded
    document.addEventListener('DOMContentLoaded', function() {
      if (!_bindShinyHandlers()) {
        // Strategy 3: Listen for Shiny's own connected event (shinylive)
        document.addEventListener('shiny:connected', function() {
          _bindShinyHandlers();
        });
        // Strategy 4: Polling fallback for edge cases where events are missed
        var _attempts = 0;
        var _pollId = setInterval(function() {
          _attempts++;
          if (_bindShinyHandlers() || _attempts > 30) {
            clearInterval(_pollId);
            if (_attempts > 30 && !_getShiny()) {
              console.warn('[CollabNET] Shiny object not found after 30 attempts; worker pool is standalone.');
            }
          }
        }, 1000);
      }
    });
  }

})(window);
