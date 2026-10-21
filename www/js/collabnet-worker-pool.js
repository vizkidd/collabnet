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

            for (let d = 0; d < dois.length; d++) {
              const rawDoi = dois[d].trim().replace(/^https?:\\/\\/(dx\\.)?doi\\.org\\//i, '');
              if (!rawDoi) continue;

              try {
                const controller = new AbortController();
                const timeoutId = setTimeout(() => controller.abort(), timeoutMs);
                const url = 'https://api.openalex.org/works/https://doi.org/' + encodeURIComponent(rawDoi);
                
                const resp = await fetch(url, { signal: controller.signal });
                clearTimeout(timeoutId);

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
                } else {
                  results.push({ doi: rawDoi, success: false, status: resp.status });
                }
              } catch (err) {
                results.push({ doi: rawDoi, success: false, error: err.message });
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

  // Shiny message bindings
  document.addEventListener('DOMContentLoaded', function() {
    if (window.Shiny && Shiny.addCustomMessageHandler) {
      // Receive task from Shiny R server
      Shiny.addCustomMessageHandler('collabnet_pool_exec', async function(msg) {
        const { taskId, action, data, options } = msg;
        try {
          let result;
          if (action === 'tokenize') {
            result = await window.collabNetPool.parallelTokenize(data, options?.delim || ',');
          } else if (action === 'fetch_dois') {
            result = await window.collabNetPool.parallelFetchDois(data, options?.timeoutMs || 15000);
          } else {
            result = await window.collabNetPool.dispatch(action, data);
          }

          Shiny.setInputValue('collabnet_pool_result', {
            taskId: taskId,
            success: true,
            data: result
          }, { priority: 'event' });
        } catch (err) {
          Shiny.setInputValue('collabnet_pool_result', {
            taskId: taskId,
            success: false,
            error: err.message
          }, { priority: 'event' });
        }
      });

      // Send worker pool readiness and core count to Shiny
      Shiny.setInputValue('collabnet_pool_ready', {
        cores: navigator.hardwareConcurrency || 4,
        workers: window.collabNetPool.size
      });
    }
  });

})(window);
