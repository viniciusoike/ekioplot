# Changelog

## ekioplot 0.7.0

This release makes `inst/ekio-palettes.yaml` the single source of truth
for EKIO brand color, aligns scale position with shade number, and moves
gt table styling into a companion package. It contains several breaking
changes; see below before upgrading.

### Breaking changes

- The exported color vectors `ekio_blue`, `ekio_gray`, `ekio_teal`,
  `ekio_orange`, and `ekio_accent` have been removed. All brand color is
  now reached through
  [`ekio_pal()`](https://viniciusoike.github.io/ekioplot/reference/ekio_pal.md).
  `ekio_accent` was byte-identical to `ekio_pal("full")`; the four
  scales are now `ekio_pal("blue")` and friends.

- Brand scales are **nine steps named `"100"` to `"900"`** instead of
  ten named `"50"` to `"900"`. Position and shade are now aligned by
  construction, so `ekio_pal("blue")[7]` and `ekio_pal("blue")["700"]`
  return the same color. Code that indexed the old ramps positionally
  will need updating.

- The `"50"` shades are gone. They were near-duplicates of `"100"` — a
  luminance gap of 1, 2, and -1 for gray, teal, and orange against a
  median interior gap of 24 — and orange’s inverted the ramp.
  [`theme_ekio()`](https://viniciusoike.github.io/ekioplot/reference/theme_ekio.md)’s
  background is now `gray.100`, a change of two luminance points.

- `theme_ekio_map()` is removed. It only blanked the axes and moved the
  legend; most of what a map needs (`coord_sf(expand = FALSE)`, colorbar
  sizing) cannot be expressed in a theme object.

- `gt_theme_ekio()` has moved to the companion package
  [ekiotable](https://github.com/viniciusoike/ekiotable). It reads brand
  tokens from this package via
  [`ekio_pal()`](https://viniciusoike.github.io/ekioplot/reference/ekio_pal.md)
  and `ekio_font()`, so tables and charts stay in sync without
  duplicating color definitions. `gt` is no longer a dependency of
  ekioplot.

- For sequential and diverging palettes, `n` now interpolates across the
  whole ramp instead of returning the `n` lightest colors.
  `ekio_pal("blue", n = 3)` gives a light/mid/dark triple. Categorical,
  small-group, and scientific palettes still take the first `n`.

- `teal["50"]` was `#F0FFF4` — identical to green’s lightest tint, not a
  teal — and is removed. `blue["800"]` changes from `#1B3A4B` to
  `#152A44`; the old value sat 3 luminance points from `blue.700` and
  was hue-shifted toward teal.

- The `purple`, `red`, `green`, and `amber` scales drop their lightest
  tint and gain a `"900"`, so all eight scales now have identical
  structure.

- All three diverging palettes change slightly. Their neutral pivot is
  now guaranteed to be the lightest color in the palette, so the visual
  center of the scale lands on the data’s zero:

  - `teal_orange` previously peaked at `teal.100` (luminance 247) rather
    than at its pivot (241), putting the lightest point one slot off
    center. Its cool arm now stops at `teal.200`, and the shared warm
    pivot is lighter.
  - `blue_red`’s warm arm was irregular
    (`red.100, .300, .500, .600, .800` — a 100-shade step among 200s),
    which left `red.500` and `red.600` only 19 luminance points apart
    where their neighbours were 61 and 54, wasting a class. It now
    mirrors the cool arm as `red.100, .300, .500, .700, .900`. This also
    narrows the gap between the two extreme ends from 39 luminance
    points to 26, so equally extreme values read with more equal weight.
  - `blue_orange` keeps its arms and gains the lighter pivot, which
    removes a 4-point step between the pivot and `orange.200`.

### New features

- [`ekio_contrast()`](https://viniciusoike.github.io/ekioplot/reference/ekio_contrast.md)
  computes the WCAG 2.1 contrast ratio between two colors.

- [`ekio_text_on()`](https://viniciusoike.github.io/ekioplot/reference/ekio_text_on.md)
  picks the more readable text color (black or white) for a given
  background, for labels on colored fills.

- All five recipe functions gain `title`, `subtitle`, and `caption`
  arguments. They previously had no way to set plot text: a `title`
  passed by name fell through `...` into the geom, where ggplot2 ignored
  it with a warning — or, for
  [`ekio_barplot()`](https://viniciusoike.github.io/ekioplot/reference/ekio_barplot.md),
  failed outright.

- New `ekio_font()` exposes the platform-appropriate EKIO font family.
  It is the canonical accessor for EKIO brand type, so packages styling
  other output do not repeat the platform logic.

- Color is defined once in `inst/ekio-palettes.yaml` and compiled into
  the package by `data-raw/palettes.R`. The YAML ships with the package,
  so downstream projects can read the canonical tokens via
  `system.file("ekio-palettes.yaml", package = "ekioplot")`.

- Palettes are defined by `scale.shade` token reference rather than
  literal hex, so a palette can no longer drift out of sync with the
  scale it was built from. The build script fails if a token does not
  resolve, if a scale stops darkening monotonically, or if a diverging
  palette’s pivot is not its lightest color. A test re-resolves the YAML
  and compares it against the built data, so editing one without
  rebuilding the other fails loudly.

### Documentation

- New pkgdown article “Color accessibility” showing which EKIO colors
  work with black vs. white text, with WCAG AA/AAA compliance for the
  accent colors in `ekio_pal("full")`.

### Bug fixes

- `ekio_pal(pal, n = length(pal))` routed through `colorRampPalette`,
  which returns an unnamed vector, so `ekio_pal("blue", n = 9)["700"]`
  gave `NA` while `ekio_pal("blue")["700"]` worked. An `n` matching the
  palette length is now a no-op.

## ekioplot 0.5.1

### New features

- [`list_ekio_palettes()`](https://viniciusoike.github.io/ekioplot/reference/list_ekio_palettes.md)
  gains a `verbose` argument that prints a formatted summary of the
  selected palette type(s).

- [`ekio_pal()`](https://viniciusoike.github.io/ekioplot/reference/ekio_pal.md)
  now returns an object of class `ekio_palette`, a character vector with
  a custom print method that automatically displays a color swatch in
  interactive sessions. This replaces the need for a separate
  `show_ekio_palette()` function. Use
  [`as.character()`](https://rdrr.io/r/base/character.html) to strip the
  class when only hex codes are needed.

### Deprecations

- [`show_all_ekio_palettes()`](https://viniciusoike.github.io/ekioplot/reference/show_all_ekio_palettes.md)
  is deprecated in favor of `list_ekio_palettes(verbose = TRUE)`. It
  still works but warns once per session, and is no longer listed on the
  pkgdown reference index.

- `show_ekio_palette()` is deprecated in favor of
  [`ekio_pal()`](https://viniciusoike.github.io/ekioplot/reference/ekio_pal.md)
  (which auto-displays a swatch on print). It still works but warns once
  per session.

## ekioplot 0.5.0

### Breaking changes

- `gt` moved from `Imports` to `Suggests`. It is only needed by
  `gt_theme_ekio()`, which now prompts to install it on first use via
  [`rlang::check_installed()`](https://rlang.r-lib.org/reference/is_installed.html).
  This drops 43 transitive dependencies (including `V8`, `Rcpp`, `curl`,
  `bslib`, `htmlwidgets`, `reactable`, `knitr`, and `rmarkdown`) from a
  default install, leaving core plotting dependent only on `ggplot2`’s
  tree. Users who style `gt` tables should add `gt` to their own
  dependencies.

- [`ekio_scatterplot()`](https://viniciusoike.github.io/ekioplot/reference/ekio_scatterplot.md)
  no longer forces a y = 0 baseline by default (`add_zero = FALSE`).
  Scatter plots whose y values are far from zero were distorted by the
  forced baseline. Pass `add_zero = TRUE` to restore the old behavior.

### Bug fixes

- [`ekio_histogram()`](https://viniciusoike.github.io/ekioplot/reference/ekio_histogram.md)
  now handles transformed x expressions such as `log(mpg)`; previously
  the bin calculation errored on anything other than a bare column name.

### Documentation

- Replaced a dead Imazon reference URL in the `ips_brasil` dataset
  documentation.

## ekioplot 0.4.0

### Breaking changes

- Removed `run_palette_lab()` and the bundled Shiny app. The Palette Lab
  now lives in its own repository
  (<https://github.com/viniciusoike/ekioplot-palette-lab>) and runs
  entirely in the browser at
  <https://viniciusoike.github.io/ekioplot-palette-lab/>. This drops 8
  app-only `Suggests` (`bslib`, `colorspace`, `colourpicker`, `forcats`,
  `ggbump`, `patchwork`, `shiny`, `stringr`), leaving `ekioplot` a lean
  visualization package.

## ekioplot 0.3.1

### Documentation

- Rewrote `README.md` to match the current API (the previous version
  referenced removed font helpers, theme style names, and palette names
  that no longer exist).

### Infrastructure

- Added an `R-CMD-check` GitHub Actions workflow covering Windows,
  macOS, and Linux (release, devel, and oldrel).
- Removed stale repository artifacts and scratch files.

## ekioplot 0.3.0

### Breaking changes

- Complete API overhaul. Palette system, themes, and scales have been
  redesigned with a cleaner, more consistent interface.

### Color system

- Unified palette access through
  [`ekio_pal()`](https://viniciusoike.github.io/ekioplot/reference/ekio_pal.md)
  — all palette types (categorical, small-group, scientific, sequential,
  diverging) are accessible from a single function.
- Added 8 sequential palettes (blue, teal, gray, orange, purple, red,
  green, amber) and 3 diverging palettes (blue_orange, blue_red,
  teal_orange).
- Added curated small-group variants: `duo_warm`, `duo_cool`,
  `trio_bold`, `trio_cool`, `quad_earth`, `quad_vivid`.
- Added scientific palettes: `okabe_ito`, `viridis`, `inferno`,
  `plasma`.
- [`list_ekio_palettes()`](https://viniciusoike.github.io/ekioplot/reference/list_ekio_palettes.md)
  now validates the `type` argument and errors on invalid types.
- `ekio_orange` now has 10 shades (50-900), matching the other color
  scales.

### Themes

- [`theme_ekio()`](https://viniciusoike.github.io/ekioplot/reference/theme_ekio.md)
  rebuilt using ggplot2’s modular `theme_sub_*()` helpers (requires
  ggplot2 \>= 3.5.0).
- Added `theme_ekio_map()` for choropleth and spatial visualizations.

### Scales

- Added continuous scale functions:
  [`scale_color_ekio_c()`](https://viniciusoike.github.io/ekioplot/reference/scale_color_ekio_c.md)
  and
  [`scale_fill_ekio_c()`](https://viniciusoike.github.io/ekioplot/reference/scale_color_ekio_c.md)
  for sequential and diverging palettes.
- British spelling aliases
  ([`scale_colour_ekio_c()`](https://viniciusoike.github.io/ekioplot/reference/scale_color_ekio_c.md),
  [`scale_colour_ekio_d()`](https://viniciusoike.github.io/ekioplot/reference/scale_color_ekio_d.md))
  included.

### Recipe functions

- Added high-level chart builders:
  [`ekio_histogram()`](https://viniciusoike.github.io/ekioplot/reference/ekio_histogram.md),
  [`ekio_lineplot()`](https://viniciusoike.github.io/ekioplot/reference/ekio_lineplot.md),
  [`ekio_scatterplot()`](https://viniciusoike.github.io/ekioplot/reference/ekio_scatterplot.md),
  [`ekio_barplot()`](https://viniciusoike.github.io/ekioplot/reference/ekio_barplot.md),
  [`ekio_areaplot()`](https://viniciusoike.github.io/ekioplot/reference/ekio_areaplot.md).
- Smart aesthetic detection automatically distinguishes static colors
  from variable mappings and applies appropriate scales.

### Tables

- Added `gt_theme_ekio()` for professional gt table styling with EKIO
  branding.
- Fixed `gt_theme_ekio()` crashing on tables without summary rows or row
  groups.

### Datasets

- Added `fuels` dataset (global fuel consumption time series).
- Added `brazil_agriculture`, `brazil_agriculture_states`, `brazil_gdp`,
  and `brazil_population` datasets.
- Added `ips_brasil` dataset (Social Progress Index for Brazilian
  municipalities).

### Other

- Removed `dplyr` dependency.
- Removed `scales` from Imports (not used directly).
- Removed dead code and obsolete prototype files.
- Added Shiny palette explorer app (`inst/shiny-app/`).
- Added test coverage for recipe functions and gt theme.
