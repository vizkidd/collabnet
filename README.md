# CollabNET: Interactive Bibliometric Collaboration & Social Network Analysis

[![WebAssembly](https://img.shields.io/badge/Powered%20By-WebAssembly%20%7C%20Shinylive-blue.svg)](https://vizkidd.github.io/collabnet/)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)
[![Architecture](https://img.shields.io/badge/Worker%20Pool-JS%20Multi--threading-orange.svg)](#architecture)
[![MCP](https://img.shields.io/badge/MCP-Model%20Context%20Protocol-purple.svg)](mcp/)

**CollabNET** is a high-performance, client-side bibliometric and social network analysis platform compiled to WebAssembly (WASM via Shinylive). It empowers researchers, bibliometricians, and research institutions to analyze co-authorship networks, research impact, interdisciplinary intersections, and topological graph-theoretic properties directly inside the web browser—with zero server installation, absolute data privacy, and full offline operation.

Live application: **[https://vizkidd.github.io/collabnet/](https://vizkidd.github.io/collabnet/)**

---

## 🌟 Key Features

### 1. Zero-Backend Ingestion & Portability
- **Live Resolvers**: Instant asynchronous publication and metadata extraction via ORCiD (v3.0 REST API), Scopus Author ID / Document Search, OpenAlex, and Crossref.
- **Universal File Ingestion**: Intelligent column mapping wizard for Scopus CSV, Web of Science (WoS), PubMed, Lens.org, and custom CSV/Excel files.
- **Session Persistence (`.cnet`)**: Binary state saving and restoration preserving imported records, active filters, column mappings, and network layout.
- **Tabular Exports**: Publication tables exportable to CSV, Excel, PDF, and clipboard via DataTables Buttons.

### 2. Rigorous Graph Theory & Network Topologies
- **Newman Fractional Weighting**: Edge weights between co-authors scaled inversely by the number of authors per publication:
  $$w_{ij} = \sum_{p \in P_{ij}} \frac{1}{N_p - 1}$$
- **Centrality Suite**: Node-level Degree, Betweenness, Closeness, Eigenvector centrality, PageRank, and k-Core decomposition.
- **Hamiltonian Graph Diagnostics**: Automated validation of Ore's Theorem and Dirac's Theorem for Hamiltonian cycle existence in collaboration components:
  - **Ore's Condition**: $\deg(u) + \deg(v) \ge |V|$ for every pair of non-adjacent vertices.
  - **Dirac's Condition**: $\deg(v) \ge \frac{|V|}{2}$ for all vertices.
- **Cut-Vertex & Articulation Point Detection**: Identifies critical collaboration gatekeepers whose departure would partition the network.
- **Dynamic Ego Networks**: One-click egocentric neighborhood extraction for selected authors.

### 3. Boolean Relationship Gates
Combine multiple focal authors or lookup keywords using propositional logic gates:
- **OR (Co-patriot/Collaborator)**: Publications containing *any* selected keyword.
- **AND (Companion)**: Publications where *all* focal entities co-authored together.
- **XOR (Rival)**: Publications authored by *strictly one* focal author (zero overlap).
- **NOR (Ignore)**: Publications authored by collaborators *excluding* the focal authors.
- **NAND (Divide & Exclude)**: Exclude publications that feature all focal entities simultaneously.
- **FULL (Unfiltered)**: Full dataset preserved with target authors annotated.

### 4. Overlap & Interdisciplinary Analysis
- High-resolution Venn diagrams and UpSet-style intersection plots across keywords, research departments, or publication venues.
- Dynamic publication and citation contribution trends stratified by Journal Quartiles (Q1, Q2, Q3, Q4) and authorship position (First, Second, Corresponding, Co-Author).

### 5. High-Concurrency Web Worker Pool
- Multi-threaded JavaScript Worker Pool executes CPU-heavy tasks (tokenization, Newman fractional edge weighting, parallel DOI resolution) without blocking the R WebAssembly event loop.
- Exponential backoff retry and Crossref fallback protection against HTTP 429 rate limits.

---

## 🏗️ Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                       Browser Window                        │
├───────────────────────────────┬─────────────────────────────┤
│   Shinylive WebAssembly       │   JS Web Worker Pool        │
│   (R Runtime / webR)          │   (Multi-threaded)          │
│                               │                             │
│ • Shiny Reactive Engine       │ • Multi-delimiter parsing   │
│ • igraph graph theory metrics │ • Newman edge weighting     │
│ • visNetwork rendering        │ • Parallel DOI fetch        │
│ • DataTables UI tables        │ • 429 backoff & fallback    │
│                               │                             │
│         ▲                     │         ▲                   │
│         └────────── Shiny Bridge ───────┘                   │
│             (collabnet_pool_exec / IPC)                     │
└─────────────────────────────────────────────────────────────┘
```

CollabNET operates as a standalone Single Page Application (SPA). Shinylive loads an in-browser webR engine compiled with Emscripten. All data remains in memory on your device—no research data or API keys are ever transmitted to an external application server.

---

## 🚀 Quick Start

### Online (Recommended)
Navigate to [https://vizkidd.github.io/collabnet/](https://vizkidd.github.io/collabnet/) in any modern desktop browser (Chrome, Firefox, Safari, Edge).

### Local Offline Execution
1. Clone the repository:
   ```bash
   git clone https://github.com/vizkidd/collabnet.git
   cd collabnet
   Rscript -e "source('buildWASM.R')"
   ```
2. Serve the `wasm_build` folder with any local web server:
   ```bash
   python3 -m http.server 8000 --directory wasm_build
   ```
3. Open `http://localhost:8000` in your browser.

---

## 🤖 Model Context Protocol (MCP) Integration

CollabNET includes a Model Context Protocol (MCP) server located in `mcp/` allowing AI agents (such as Antigravity, Claude Desktop, and Cursor) to orchestrate and query bibliometric networks programmatically:

```json
{
  "mcpServers": {
    "collabnet": {
      "command": "node",
      "args": ["/path/to/collabnet/mcp/src/index.mjs"],
      "env": {
        "COLLABNET_URL": "https://vizkidd.github.io/collabnet/"
      }
    }
  }
}
```

---

## 📚 Documentation & Manual
For full mathematical formulations, tutorial walkthroughs, and step-by-step guidance, refer to the [MANUAL.md](MANUAL.md) or click the **Manual** button in the dashboard top navigation.

## 📄 License
This project is licensed under the MIT License. See [LICENSE](LICENSE) for details.
