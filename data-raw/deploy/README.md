# Deployment: pkgdown site + shinylive Palette Lab

The package website and the in-browser **Palette Lab** are published together to
GitHub Pages by `.github/workflows/pkgdown.yaml`:

- pkgdown site  → `https://viniciusoike.github.io/ekioplot/`
- Palette Lab   → `https://viniciusoike.github.io/ekioplot/palette-lab/`

The app runs entirely in the browser via WebAssembly (webR) — there is no Shiny
server to host or pay for.

## How `ekioplot` reaches the browser build

shinylive auto-fetches WebAssembly binaries for CRAN packages from
`https://repo.r-wasm.org`. `ekioplot` is not on CRAN, so it is served from
**r-universe**, which auto-builds a WASM binary for every package it tracks.
shinylive resolves it from the `Repository:` field that an r-universe install
writes into the package `DESCRIPTION`.

## One-time setup (do this once)

1. Create a public repo named **`universe`** under the `viniciusoike` account:
   `https://github.com/viniciusoike/universe`.

2. Add a `packages.json` at its root with the contents of
   [`r-universe-packages.json`](./r-universe-packages.json).

   > **Important — list *every* package in the universe, not just `ekioplot`.**
   > When this `universe` registry repo does not exist, r-universe
   > auto-discovers and builds *all* R-package repos in the account
   > (currently `realestatebr`, `trendseries`, `metrosp`). As soon as an
   > explicit registry repo exists, r-universe switches to using it verbatim
   > and **drops anything not listed**. So `packages.json` must enumerate the
   > full set:

   ```json
   [
     { "package": "realestatebr", "url": "https://github.com/viniciusoike/realestatebr" },
     { "package": "trendseries",  "url": "https://github.com/viniciusoike/trendseries" },
     { "package": "metrosp",      "url": "https://github.com/viniciusoike/metrosp" },
     { "package": "ekioplot",     "url": "https://github.com/viniciusoike/ekioplot" }
   ]
   ```

3. r-universe will pick it up within minutes and start building
   `https://viniciusoike.r-universe.dev`, including the WASM binary. Confirm at
   `https://viniciusoike.r-universe.dev/ekioplot` and check the **wasm** build badge.

   This also gives users a binary install path for the non-CRAN package:

   ```r
   install.packages("ekioplot", repos = "https://viniciusoike.r-universe.dev")
   ```

4. Enable GitHub Pages for this repo from the **`gh-pages`** branch
   (Settings → Pages → Source: Deploy from a branch → `gh-pages` / root).
   The workflow creates and pushes that branch on the first run.

After that, every push to `main`/`master` rebuilds and redeploys both the site
and the app.

## Notes

- The deploy uses the latest *published* r-universe build of `ekioplot` for the
  app, which may lag the newest commit by a few minutes. The app only calls
  stable exported functions, so this is harmless. If the app ever needs a
  brand-new function, wait for r-universe to rebuild before relying on it in the
  hosted app (local `run_palette_lab()` always uses your installed version).
- To preview the app build locally (requires the Emscripten/webR toolchain):
  `shinylive::export("inst/shiny-app", "_site")` then
  `httpuv::runStaticServer("_site")`.
