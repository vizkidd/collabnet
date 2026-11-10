# deploy_static (Phase 9 E1, updated E3)

Static pages copied by `buildWASM.R` into the deploy dir (`wasm_build/`, published to GitHub Pages), so they resolve at
`https://vizkidd.github.io/collabnet/donation-policy/` (and `/refund-policy/`, a redirect stub), `/success/`, `/failure/`.

- `donation-policy/`: Buy Me a Coffee (BMC) donation policy. A **draft for owner approval** (noted in an HTML comment only).
- `success/`, `failure/` (E3): the old payment-return flow (pre-E3 provider) is **retired**. These are now static "this page is no longer used" notices
  (no script, `noindex`), kept so old links do not 404. They are **not** webhook endpoints: GitHub Pages answers POST with
  **405**, and BMC auto-disables a webhook after 10 consecutive failures, so the four BMC webhook URLs must point at the
  Cloudflare Worker/Pages deploy (`/collabnet/success`, `/collabnet/failure`, `/collabnet/monthlysuccess`,
  `/collabnet/monthlyfailure`; see DEPLOY_CLOUDFLARE.md). On Cloudflare the Worker answers those paths before static assets.
- On GitHub Pages there is no verify endpoint, so the export dialog shows the "I've donated" honour button
  (`collabnet.gate_honour_fallback = "auto"`); on Cloudflare it shows the one-time code and verifies the donation instead.
- The Shinylive service worker only handles `app_*` requests (see `sw_patch.py`), so these pages are served normally.
