# Changelog

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
- Added
  [`theme_ekio_map()`](https://viniciusoike.github.io/ekioplot/reference/theme_ekio_map.md)
  for choropleth and spatial visualizations.

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

- Added
  [`gt_theme_ekio()`](https://viniciusoike.github.io/ekioplot/reference/gt_theme_ekio.md)
  for professional gt table styling with EKIO branding.
- Fixed
  [`gt_theme_ekio()`](https://viniciusoike.github.io/ekioplot/reference/gt_theme_ekio.md)
  crashing on tables without summary rows or row groups.

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
