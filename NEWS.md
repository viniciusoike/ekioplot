# ekioplot 0.7.1

A maintenance release: bug fixes and cleanup, no API changes.

## Bug fixes

* Recipe functions no longer error on a continuous color or fill mapping.
  `palette` defaulted to `"contrast"` for every mapping, but `"contrast"` is
  categorical and `scale_*_ekio_c()` rejects it, so the documented continuous
  path failed in all five recipes. The default is now `"blue"` when the
  mapping is continuous and `"contrast"` when it is discrete.

* `ekio_barplot()` applied `scale_fill_ekio_d()` regardless of the mapping,
  so a continuous `fill` failed with "Continuous value supplied to a discrete
  scale". It now branches on the detected type like the other four recipes.

* `theme_ekio()` passed `paper = colors$off_white`, a typo for `offwhite`
  that resolved to `NULL` and silently dropped the paper color. Paper is now
  the `gray.100` brand token, matching the plot and panel backgrounds.

* `theme_ekio()` validates `grid` with `match.arg()`. An unrecognized value
  such as `grid = "bogus"` previously returned a theme with no grid lines
  instead of erroring, matching how `ticks` already behaved.

* `ekio_lineplot()`, `ekio_scatterplot()`, and `ekio_barplot()` check that
  `data` is a data frame, as `ekio_histogram()` and `ekio_areaplot()` did.

* `ekio_barplot()` draws the zero baseline on the correct axis when
  `horizontal = TRUE`.

* Recipe functions now show the axis ticks appropriate to each chart while
  keeping the requested grid lines independent.

* `theme_ekio()` no longer restores major grid lines that were not requested
  through the `grid` argument.

* `%||%` is imported from rlang. It was previously resolved from base R,
  which only gained the operator in 4.4.0, so `scale_color_ekio_c()` and
  `list_ekio_palettes(verbose = TRUE)` failed on the R 4.1–4.3 that
  `Depends` claimed to support.

## Dependencies

* `ggplot2` minimum is now 4.0.0. `theme_ekio()` passes `paper` to
  `theme_minimal()`, which 3.5.x does not accept, so the declared
  `>= 3.5.0` never actually worked.

## Documentation

* Removed references to `ekio_font()` from `NEWS.md` and the getting-started
  vignette. The function was announced in 0.7.0 but never shipped; brand
  tokens reach downstream packages through `ekio_pal()`.

* Dropped the 0.7.0 note announcing the "Color accessibility" article, which
  was removed before release.

* Corrected the `ips_brasil` `@format` variable count from 8 to 9.

## Internal

* Removed the blanket `@import ggplot2` in favor of `ggplot2::` prefixes,
  with `@importFrom` retained for `theme.R`, the one file with many calls.

* `print.ekio_palette()` picks swatch label colors with `ekio_text_on()`
  instead of a private luminance approximation, removing the third copy of
  luminance math in the package.

* Dropped an unused `param_name` argument from `.detect_aesthetic_type()`,
  an unused `rlang::as_name` import, dead assignments in the grid theme
  branches, and a stale `globalVariables()` declaration.

# ekioplot 0.7.0

This release makes `inst/ekio-palettes.yaml` the single source of truth for
EKIO brand color, aligns scale position with shade number, and moves gt table
styling into a companion package. It contains several breaking changes; see
below before upgrading.

## Breaking changes

* The exported color vectors `ekio_blue`, `ekio_gray`, `ekio_teal`,
  `ekio_orange`, and `ekio_accent` have been removed. All brand color is now
  reached through `ekio_pal()`. `ekio_accent` was byte-identical to
  `ekio_pal("full")`; the four scales are now `ekio_pal("blue")` and friends.

* Brand scales are **nine steps named `"100"` to `"900"`** instead of ten
  named `"50"` to `"900"`. Position and shade are now aligned by
  construction, so `ekio_pal("blue")[7]` and `ekio_pal("blue")["700"]` return
  the same color. Code that indexed the old ramps positionally will need
  updating.

* The `"50"` shades are gone. They were near-duplicates of `"100"` — a
  luminance gap of 1, 2, and -1 for gray, teal, and orange against a median
  interior gap of 24 — and orange's inverted the ramp. `theme_ekio()`'s
  background is now `gray.100`, a change of two luminance points.

* `theme_ekio_map()` is removed. It only blanked the axes and moved the
  legend; most of what a map needs (`coord_sf(expand = FALSE)`, colorbar
  sizing) cannot be expressed in a theme object.

* `gt_theme_ekio()` has moved to the companion package
  [ekiotable](https://github.com/viniciusoike/ekiotable). It reads brand
  tokens from this package via `ekio_pal()`, so tables and charts stay in
  sync without duplicating color definitions. `gt` is no longer a dependency
  of ekioplot.

* For sequential and diverging palettes, `n` now interpolates across the
  whole ramp instead of returning the `n` lightest colors.
  `ekio_pal("blue", n = 3)` gives a light/mid/dark triple. Categorical,
  small-group, and scientific palettes still take the first `n`.

* `teal["50"]` was `#F0FFF4` — identical to green's lightest tint, not a
  teal — and is removed. `blue["800"]` changes from `#1B3A4B` to `#152A44`;
  the old value sat 3 luminance points from `blue.700` and was hue-shifted
  toward teal.

* The `purple`, `red`, `green`, and `amber` scales drop their lightest tint
  and gain a `"900"`, so all eight scales now have identical structure.

* All three diverging palettes change slightly. Their neutral pivot is now
  guaranteed to be the lightest color in the palette, so the visual center of
  the scale lands on the data's zero:

  * `teal_orange` previously peaked at `teal.100` (luminance 247) rather than
    at its pivot (241), putting the lightest point one slot off center. Its
    cool arm now stops at `teal.200`, and the shared warm pivot is lighter.
  * `blue_red`'s warm arm was irregular (`red.100, .300, .500, .600, .800` —
    a 100-shade step among 200s), which left `red.500` and `red.600` only 19
    luminance points apart where their neighbours were 61 and 54, wasting a
    class. It now mirrors the cool arm as `red.100, .300, .500, .700, .900`.
    This also narrows the gap between the two extreme ends from 39 luminance
    points to 26, so equally extreme values read with more equal weight.
  * `blue_orange` keeps its arms and gains the lighter pivot, which removes a
    4-point step between the pivot and `orange.200`.

## New features

* `ekio_contrast()` computes the WCAG 2.1 contrast ratio between two colors.

* `ekio_text_on()` picks the more readable text color (black or white) for a
  given background, for labels on colored fills.

* All five recipe functions gain `title`, `subtitle`, and `caption`
  arguments. They previously had no way to set plot text: a `title` passed
  by name fell through `...` into the geom, where ggplot2 ignored it with a
  warning — or, for `ekio_barplot()`, failed outright.

* Color is defined once in `inst/ekio-palettes.yaml` and compiled into the
  package by `data-raw/palettes.R`. The YAML ships with the package, so
  downstream projects can read the canonical tokens via
  `system.file("ekio-palettes.yaml", package = "ekioplot")`.

* Palettes are defined by `scale.shade` token reference rather than literal
  hex, so a palette can no longer drift out of sync with the scale it was
  built from. The build script fails if a token does not resolve, if a scale
  stops darkening monotonically, or if a diverging palette's pivot is not its
  lightest color. A test re-resolves the YAML and compares it against the
  built data, so editing one without rebuilding the other fails loudly.

## Bug fixes

* `ekio_pal(pal, n = length(pal))` routed through `colorRampPalette`, which
  returns an unnamed vector, so `ekio_pal("blue", n = 9)["700"]` gave `NA`
  while `ekio_pal("blue")["700"]` worked. An `n` matching the palette length
  is now a no-op.

# ekioplot 0.5.1

## New features

* `list_ekio_palettes()` gains a `verbose` argument that prints a formatted
  summary of the selected palette type(s).

* `ekio_pal()` now returns an object of class `ekio_palette`, a character
  vector with a custom print method that automatically displays a color
  swatch in interactive sessions. This replaces the need for a separate
  `show_ekio_palette()` function. Use `as.character()` to strip the class
  when only hex codes are needed.

## Deprecations

* `show_all_ekio_palettes()` is deprecated in favor of
  `list_ekio_palettes(verbose = TRUE)`. It still works but warns once per
  session, and is no longer listed on the pkgdown reference index.

* `show_ekio_palette()` is deprecated in favor of
  `ekio_pal()` (which auto-displays a swatch on print). It still works
  but warns once per session.

# ekioplot 0.5.0

## Breaking changes

* `gt` moved from `Imports` to `Suggests`. It is only needed by
  `gt_theme_ekio()`, which now prompts to install it on first use via
  `rlang::check_installed()`. This drops 43 transitive dependencies (including
  `V8`, `Rcpp`, `curl`, `bslib`, `htmlwidgets`, `reactable`, `knitr`, and
  `rmarkdown`) from a default install, leaving core plotting dependent only on
  `ggplot2`'s tree. Users who style `gt` tables should add `gt` to their own
  dependencies.

* `ekio_scatterplot()` no longer forces a y = 0 baseline by default
  (`add_zero = FALSE`). Scatter plots whose y values are far from zero were
  distorted by the forced baseline. Pass `add_zero = TRUE` to restore the old
  behavior.

## Bug fixes

* `ekio_histogram()` now handles transformed x expressions such as `log(mpg)`;
  previously the bin calculation errored on anything other than a bare column
  name.

## Documentation

* Replaced a dead Imazon reference URL in the `ips_brasil` dataset
  documentation.

# ekioplot 0.4.0

## Breaking changes

* Removed `run_palette_lab()` and the bundled Shiny app. The Palette Lab now
  lives in its own repository
  (<https://github.com/viniciusoike/ekioplot-palette-lab>) and runs entirely in
  the browser at <https://viniciusoike.github.io/ekioplot-palette-lab/>. This
  drops 8 app-only `Suggests` (`bslib`, `colorspace`, `colourpicker`,
  `forcats`, `ggbump`, `patchwork`, `shiny`, `stringr`), leaving `ekioplot` a
  lean visualization package.

# ekioplot 0.3.1

## Documentation

* Rewrote `README.md` to match the current API (the previous version
  referenced removed font helpers, theme style names, and palette names
  that no longer exist).

## Infrastructure

* Added an `R-CMD-check` GitHub Actions workflow covering Windows, macOS,
  and Linux (release, devel, and oldrel).
* Removed stale repository artifacts and scratch files.

# ekioplot 0.3.0

## Breaking changes

* Complete API overhaul. Palette system, themes, and scales have been
  redesigned with a cleaner, more consistent interface.

## Color system

* Unified palette access through `ekio_pal()` — all palette types
  (categorical, small-group, scientific, sequential, diverging) are
  accessible from a single function.
* Added 8 sequential palettes (blue, teal, gray, orange, purple, red,
  green, amber) and 3 diverging palettes (blue_orange, blue_red,
  teal_orange).
* Added curated small-group variants: `duo_warm`, `duo_cool`,
  `trio_bold`, `trio_cool`, `quad_earth`, `quad_vivid`.
* Added scientific palettes: `okabe_ito`, `viridis`, `inferno`, `plasma`.
* `list_ekio_palettes()` now validates the `type` argument and errors on
  invalid types.
* `ekio_orange` now has 10 shades (50-900), matching the other color
  scales.

## Themes

* `theme_ekio()` rebuilt using ggplot2's modular `theme_sub_*()` helpers
  (requires ggplot2 >= 3.5.0).
* Added `theme_ekio_map()` for choropleth and spatial visualizations.

## Scales

* Added continuous scale functions: `scale_color_ekio_c()` and
  `scale_fill_ekio_c()` for sequential and diverging palettes.
* British spelling aliases (`scale_colour_ekio_c()`,
  `scale_colour_ekio_d()`) included.

## Recipe functions

* Added high-level chart builders: `ekio_histogram()`,
  `ekio_lineplot()`, `ekio_scatterplot()`, `ekio_barplot()`,
  `ekio_areaplot()`.
* Smart aesthetic detection automatically distinguishes static colors
  from variable mappings and applies appropriate scales.

## Tables

* Added `gt_theme_ekio()` for professional gt table styling with EKIO
  branding.
* Fixed `gt_theme_ekio()` crashing on tables without summary rows or
  row groups.

## Datasets

* Added `fuels` dataset (global fuel consumption time series).
* Added `brazil_agriculture`, `brazil_agriculture_states`,
  `brazil_gdp`, and `brazil_population` datasets.
* Added `ips_brasil` dataset (Social Progress Index for Brazilian
  municipalities).

## Other

* Removed `dplyr` dependency.
* Removed `scales` from Imports (not used directly).
* Removed dead code and obsolete prototype files.
* Added Shiny palette explorer app (`inst/shiny-app/`).
* Added test coverage for recipe functions and gt theme.
