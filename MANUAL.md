# CollabNET: Comprehensive User Manual & Reference Guide

Welcome to the **CollabNET User Manual**. This document details the end-to-end functionality of the platform, the underlying network science algorithms, and practical workflows for academic research evaluation and bibliometric analysis.

---

## 📑 Table of Contents
1. [Dashboard Layout Overview](#1-dashboard-layout-overview)
2. [Data Ingestion & Live Resolvers](#2-data-ingestion--live-resolvers)
3. [Lookup Controls & Relationship Logic](#3-lookup-controls--relationship-logic)
4. [Interactive Collaboration Network Graph](#4-interactive-collaboration-network-graph)
5. [Graph Theory & Centrality Metrics](#5-graph-theory--centrality-metrics)
6. [Hamiltonian Diagnostics & Topology](#6-hamiltonian-diagnostics--topology)
7. [Overlap Analysis (Venn & UpSet)](#7-overlap-analysis-venn--upset)
8. [Publication & Citation Trends](#8-publication--citation-trends)
9. [Session Portability (`.cnet`) & Data Export](#9-session-portability-cnet--data-export)
10. [Troubleshooting & FAQ](#10-troubleshooting--faq)

---

## 1. Dashboard Layout Overview

The CollabNET interface follows a **Control Sidebar (Left) vs. Analytical Output (Right)** visual paradigm:

```
┌────────────────────────────────────────────────────────────────────────┐
│ [Title] CollabNET Analysis Dashboard    [Upload] [Download] [Keys] [Theme] [Readme] [Manual] [Tutorial] │
├─────────────────────────┬──────────────────────────────────────────────┤
│ LEFT: CONTROLS SIDEBAR  │ RIGHT: ANALYTICAL WORKSPACE                  │
│                         │                                              │
│ • Data Ingestion Panel  │ • Tab 1: Collaboration Network (visNetwork)  │
│   (ORCID, Scopus, DOI)  │ • Tab 2: Publication & Citation Trends       │
│ • Lookup Controls       │ • Tab 3: Centrality Leaderboard & Rankings   │
│   (Keywords, Logic Gate)│ • Tab 4: Centrality Map (Scatter plot)       │
│ • Delimiters & Columns  │ • Tab 5: Overlap Analysis (Venn & UpSet)     │
│ • Year Slider & Filters │ • Tab 6: Hamiltonian Cycle Diagnostics       │
│ • Edge Weight Range     │ • Tab 7: Publication Data Table (DataTables) │
└─────────────────────────┴──────────────────────────────────────────────┘
```

- **Top Navigation Bar**: Houses global actions—file upload, `.cnet` session download, API key configuration, Light/Dark theme toggle, and interactive documentation modals.
- **Left Column (Width = 3 / 25%)**: Contains all interactive parameters, ingestion inputs, lookup filters, relationship logic gates, and timeline sliders.
- **Right Column (Width = 9 / 75%)**: Renders high-resolution interactive charts, network physics graphs, leaderboards, and tabular data.

---

## 2. Data Ingestion & Live Resolvers

CollabNET supports two complementary ingestion methods: **Live API Fetching** and **Local File Upload**.

### A. Live API Fetching
In the left sidebar under **Data Ingestion**:
1. **ORCiD**: Enter one or more 16-character ORCID identifiers (e.g. `0000-0002-8060-444X`, `0000-0003-2287-2195`), one per line. Checksum digits ending in `X` are fully supported.
2. **Scopus Author ID**: Enter author IDs (e.g. `57871875700`), one per line. If a Scopus API Key is configured, ORCIDs can be automatically mapped to Scopus Author IDs.
3. **DOIs**: Enter Digital Object Identifiers (e.g. `10.1016/j.materresbull.2025.113811`).
4. **Submit**: Click **Submit / Fetch Data**. An async pipeline resolves works, dedupes titles, extracts author lists, and retrieves citation counts.

### B. Local File Upload
Click the **Upload** button in the top navigation bar to upload CSV or Excel files from:
- **Scopus CSV**: Exported directly from Scopus search results.
- **Web of Science (WoS)**: Tab-delimited or Excel exports.
- **PubMed**: Summary or CSV exports.
- **Lens.org**: Scholarly works exports.
- **Saved Session (`.cnet`)**: Restores an entire prior working session with one click.

#### Column Mapping Wizard
When importing custom datasets, CollabNET presents a two-step wizard:
- **Step 1 (Column Selection & Renaming)**: Select which columns to include, set delimiters (e.g. `;` or `,` for multiple authors), and map headers to canonical fields (`Authors`, `Title`, `Year`, `Citations`, `Journal`, `DOI`, `ORCID`).
- **Step 2 (Merge Strategy)**: When existing data is present, choose between **New Dataset** (replaces existing), **Append Rows** (unions records), or **Merge/Join** (joins on primary keys like `DOI` or `Title`).

---

## 3. Lookup Controls & Relationship Logic

The **Lookup Controls** panel enables selective focal subnetwork extraction based on author names or topical keywords.

### In-Order Keyword Matching
Type author names or keywords into the **Lookup Keywords** textarea line-by-line (e.g.):
```
Demis Hassabis
John Jumper
David Baker
```
Matches are evaluated against the designated delimiter-split columns (defaulting to `Authors`).

### Relationship Logic Gates
When two or more lookup keywords are provided, CollabNET uses propositional logic gates to define the publication cohort:

| Gate | Name | Behavior | Use Case |
|---|---|---|---|
| **FULL** | Full Data (Unfiltered) | Retains 100% of the dataset; annotates target authors without dropping unmatched rows. | Benchmark analysis across entire department or cohort. |
| **OR** | Co-patriot / Collaborator | Includes publications that contain **at least one** focal author. | Aggregating complete collaborative reach of a team. |
| **AND** | Companion | Includes **only** publications where **all** focal authors co-authored together. | Pinpoint direct joint collaborations between key PIs. |
| **XOR** | Rival | Includes publications authored by **strictly one** focal author (zero overlap). | Compare independent research output outside joint work. |
| **NOR** | Ignore | Includes publications authored by network members **excluding** the focal authors. | Discover peripheral and junior collaboration lines. |
| **NAND** | Divide & Exclude | Includes all publications **except** those where all focal authors worked together. | Exclude dominant joint flagship projects to see branch work. |

---

## 4. Interactive Collaboration Network Graph

The collaboration network is rendered using physics-driven dynamic layout (`visNetwork`):
- **Nodes**: Represent individual authors or entities.
  - Node Size: Proportional to total publications or degree.
  - Node Color: Differentiates focal/target authors (highlighted) vs. co-authors.
- **Edges**: Represent co-authorship relationships.
  - Edge Thickness: Proportional to Newman fractional co-authorship weight.
- **Edge Weight Slider**: Adjust the slider in the left sidebar to filter out weak or single-instance co-authorships and isolate core collaborative clusters.
- **Cut-Vertex Highlighting**: Toggle button highlights articulation points in red.
- **Physics Toggle**: Lock or release force-directed stabilization for easier manual node rearrangement.

---

## 5. Graph Theory & Centrality Metrics

### Newman Fractional Weighting
In co-authorship networks, simple paper counts distort relationship strength: co-authoring a 2-author paper represents a stronger collaborative bond than co-authoring a 1,000-author hyperauthored paper. CollabNET implements Mark Newman's fractional weighting formula:

$$w_{ij} = \sum_{p \in P_{ij}} \frac{1}{N_p - 1}$$

where $P_{ij}$ is the set of papers co-authored by authors $i$ and $j$, and $N_p$ is the total author count of publication $p$.

### Centrality Measures
The **Centrality Leaderboard** tab computes standard topological metrics:
- **Degree Centrality**: Total number of unique direct collaborators:
  $$C_D(v) = \deg(v)$$
- **Weighted Degree (Strength)**: Sum of fractional edge weights incident to the node:
  $$S(v) = \sum_{u \in N(v)} w_{vu}$$
- **Betweenness Centrality**: Frequency with which an author lies on the shortest collaboration path between pairs of other researchers:
  $$C_B(v) = \sum_{s \neq v \neq t} \frac{\sigma_{st}(v)}{\sigma_{st}}$$
  High betweenness identifies interdisciplinary bridges and knowledge brokers.
- **Closeness Centrality**: Inverted sum of shortest distances to all other reachable authors in the component:
  $$C_C(v) = \frac{1}{\sum_{u \neq v} d(v, u)}$$
- **Eigenvector Centrality**: Author importance proportional to the sum of importance scores of their collaborators:
  $$\lambda x_v = \sum_{u \in N(v)} x_u$$
- **PageRank**: Probability of arriving at an author during a random walk through the co-authorship graph (damping factor $\alpha = 0.85$).
- **Coreness (k-Core)**: Maximal subnetwork where every author has degree at least $k$.

---

## 6. Hamiltonian Diagnostics & Topology

CollabNET features automated topological checks for Hamiltonian cycles in collaboration components:
- A **Hamiltonian Cycle** is a closed loop that visits every vertex in the graph exactly once. In collaboration networks, it signifies a complete chained cycle of co-authorship linking all researchers.
- **Dirac's Condition**:
  $$\min_{v \in V} \deg(v) \ge \frac{|V|}{2}$$
  If satisfied for a graph with $|V| \ge 3$, the network is guaranteed to be Hamiltonian.
- **Ore's Condition**:
  $$\deg(u) + \deg(v) \ge |V| \quad \forall (u, v) \notin E$$
  If satisfied for all pairs of non-adjacent vertices, the network is guaranteed to be Hamiltonian.

The **Hamiltonian Cycle Diagnostics** tab summarizes component sizes, maximum/minimum degree, articulation points, and Ore/Dirac verification ratios.

---

## 7. Overlap Analysis (Venn & UpSet)

- **Venn Diagrams**: Select up to 4 categorical columns or target keywords to plot set overlap of shared publications or citations.
- **UpSet Plots**: Evaluates higher-order multi-set intersections for larger combinations of research departments or keywords without the spatial limitation of 3-circle Venn diagrams.

---

## 8. Publication & Citation Trends

- **Journal Quartiles (Q1–Q4)**: Evaluates publication volume and citations across JCR Journal Impact Factor Quartiles.
- **Authorship Position Distribution**: Visualizes contribution proportions across First Author, Second Author, Corresponding Author, and Co-Author roles.
- **Citation Distribution (Violin & Scatter Plots)**: Visualizes log-transformed citation spreads with percentile markers ($25^{th}$, Median, Mean, $75^{th}$).

---

## 9. Session Portability (`.cnet`) & Data Export

- **Save Session (`.cnet`)**: Click **Download** in the top navigation. CollabNET serializes all active datasets, extracted metadata, custom column selections, and filter states into a compressed `.cnet` binary file.
- **Restore Session**: Upload a `.cnet` file via the **Upload** button to resume work seamlessly.
- **Data Table Exports**: The **Publication Data** tab provides instant export buttons (`Copy`, `CSV`, `Excel`, `PDF`, `Print`) with human-readable column titles.

---

## 10. Troubleshooting & FAQ

- **ORCID checksum error**: Ensure the ORCID has 16 characters in four 4-digit groups (e.g. `0000-0002-8060-444X`). Checksum digits `X` or `x` are fully supported.
- **HTTP 429 Rate Limits**: CollabNET implements automatic exponential backoff retry and automatically fails over to the Crossref metadata resolver if OpenAlex rate-limits.
- **Network not updating**: Ensure the **Enable Collaboration Network** checkbox is checked in the tab header.
- **Plots hidden**: Verify the **Enable Publication & Citation Trends** checkbox is checked.
