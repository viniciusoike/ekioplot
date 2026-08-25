# ekioplot 1.1.0

## Breaking changes

* Removed the `ips_brasil` dataset because its source does not state terms
  permitting redistribution.

* Removed the `fuels` dataset because the upstream data combine sources with
  redistribution terms that are not sufficiently clear for bundling on CRAN.
  Area-plot examples now use `ggplot2::economics_long` instead.

* Every brand scale is regenerated from one OKLCH specification, so every hex
  code changes except `blue.700`, `blue.800` and `blue.900`. All seven scales
  now share one lightness spine and one chroma arc, which makes a shade number
  mean the same visual weight in every family: `blue.500` and `orange.500` are
  interchangeable in weight. The specification lives in
  `data-raw/build-ramps.R` and `data-raw/palettes.R` checks the shipped hex
  against it.

* Removed the `purple` and `amber` scales and added `stone`, a warm neutral.
  The seven scales are now `blue`, `gray`, `stone`, `teal`, `green`, `orange`
  and `red`.

* `gold` replaces `amber` as an accent rather than a scale: three colors named
  `"light"`, `"mid"` and `"deep"`, reached as `ekio_pal("gold")["mid"]`. A
  nine-step gold ramp turns brown at the dark end, so only the top of it was
  ever gold. The three sit on the same lightness rungs as scale shades 300,
  400 and 500, and `deep` clears WCAG AA on the off-white surface. There is no
  `gold` sequential palette.

* `gray` is a true cool neutral rather than a blue-tinted one, and its pale
  steps moved down the ramp. `gray.500` now clears WCAG AA on the off-white
  surface at 4.75:1, where it previously sat at 2.1:1 and failed. This is the
  color `theme_ekio()` uses for muted text.

* The previous `cool`, `minimal`, `contrast`, `muted`, `muted_warm`, `binary`
  and `political` categorical palettes have been removed, along with the
  `highlight` and `small_group` palette groups. `full` remains available.

* Diverging palettes return nine colors rather than eleven. Both arms step
  through the even shades, so the halves carry equal weight rung for rung.

* `ekio_areaplot()`, `ekio_barplot()` and `ekio_histogram()` no longer accept
  `add_zero`; `ekio_lineplot()` and `ekio_scatterplot()` retain the argument.

* Removed the `verbose` argument from `list_ekio_palettes()`.

* `theme_ekio()` draws grid lines in `gray.200` rather than `gray.300`,
  following the ramp's new spacing.

## New features

* `ekio_pal()` gains the fixed `cool3`, `cool4` and `full_muted` categorical
  palettes. The new `accent_blue` and `accent_orange` palettes return four
  colors by default and accept `n` from 2 to 6, keeping the accent first and
  adding receding grays as needed.

* Added `basic.pivot`, the neutral shared by the diverging palettes.

* A palette member can now point at any named token group, not just a scale
  shade: `gold.light` and `basic.pivot` resolve by the same rule as
  `blue.700`.

* `list_ekio_palettes()` accepts `type = "accent"`.

## Improvements

* `ekio_pal()` and `list_ekio_palettes()` now validate scalar arguments and
  return clear errors for missing, fractional, or vector inputs.

## Color provenance

* No shipped brand color derives from Chakra UI any more. The matplotlib and
  Okabe & Ito notices remain for the `scientific` palettes, while bundled IBGE
  datasets are documented separately in `inst/COPYRIGHTS`.

# ekioplot 0.8.1

* Credited the third-party sources of the color scales. The `gray`, `teal`,
  `orange`, `red`, `green`, `purple` and `amber` scales derive from Chakra UI
  v2's default theme colors (MIT); the scientific palettes come from
  matplotlib and from Okabe & Ito. Notices are in `inst/COPYRIGHTS`, reachable
  as `system.file("COPYRIGHTS", package = "ekioplot")`. No color values
  changed.

# ekioplot 0.8.0

## Breaking changes

* `show_all_ekio_palettes()` is removed. It was deprecated in 0.5.1; use
  `list_ekio_palettes()` to inspect available palette names instead.

* `theme_ekio()` draws on off-white (`#FEFEFE`) rather than the `gray.100`
  brand tint (`#F7FAFC`). Every plot changes appearance. Pass
  `theme_ekio(background = "gray")` to keep the old surface.

* The recipes are now opinionated about continuous color and fill mappings.
  `ekio_histogram()`, `ekio_lineplot()`, `ekio_barplot()` and
  `ekio_areaplot()` error and point at the fix — bin the variable or wrap it
  in `factor()`. `ekio_scatterplot()`, where a ramp does read well, warns and
  keeps the continuous scale. 0.7.1 made all five support continuous mappings
  uniformly, which removed an inconsistency but was never the right default
  for a binned, bar or band chart.

* A color or fill mapping that cannot be evaluated against `data` now errors
  in the recipe, naming the argument and expression. It was previously
  swallowed and treated as discrete, so a typo'd column surfaced as ggplot2's
  "object not found" from inside the build.

## New features

* `theme_ekio()` gains a `background` argument: `"offwhite"` (default),
  `"white"`, `"gray"` (the `gray.100` brand tint) or `"transparent"`. It sets
  the paper, plot and panel surfaces together, so the three can no longer
  drift apart.

* A `basic` brand token group — `white`, `offwhite`, `black` — supplies plot
  surfaces and inverted text. These are tokens, not a palette: `ekio_pal()`
  and `list_ekio_palettes()` do not offer them, since white-on-offwhite is
  not something data maps onto.

## Improvements

* Arguments passed through `...` now override the recipe's own geom defaults
  instead of failing with a duplicate-argument error, so
  `ekio_histogram(df, x, fill = g, alpha = 0.3)` works.

* `ekio_areaplot()` honors `position` when there is no fill mapping. It was
  previously applied only to the grouped case, so `position = "fill"` was
  silently ignored on a single series.

## Internal

* The five recipes share one internal layer builder (`.recipe_layer()`)
  instead of each reimplementing the missing / static-color / variable-mapping
  branch tree. Both bugs fixed in 0.7.1 came from that duplication.

* The internal token accessor `.ekio()` resolves palettes as well as scales,
  and accepts a position or a name where the colors carry one:
  `.ekio("basic", "white")`, `.ekio("blue", 7)`. It remains the only way
  package code reaches a brand color.

* `print.ekio_palette()` is split from the swatch builder it calls, so the
  plot can be tested without opening a graphics device.

* R-CMD-check runs on R 4.1 through 4.4 as well as release and devel. The
  matrix stopped at oldrel-1, so the `R (>= 4.1.0)` floor in `DESCRIPTION`
  was never exercised — which is how the `%||%` bug fixed in 0.7.1 went
  unnoticed.

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
  which only gained the operator in 4.4.0, so `scale_color_ekio_c()` failed
  on the R 4.1–4.3 that `Depends` claimed to support.

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

* `gt_theme_ekio()` is no longer part of `ekioplot`; `gt` is no longer a
  dependency of the package.

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

* `ekio_pal()` now returns an object of class `ekio_palette`, a character
  vector with a custom print method that automatically displays a color
  swatch in interactive sessions. This replaces the need for a separate
  `show_ekio_palette()` function. Use `as.character()` to strip the class
  when only hex codes are needed.

## Deprecations

* `show_all_ekio_palettes()` is deprecated in favor of
  `list_ekio_palettes()`. It still works but warns once per session, and is
  no longer listed on the pkgdown reference index.

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

* Removed the bundled Shiny app, dropping 8 app-only `Suggests` (`bslib`,
  `colorspace`, `colourpicker`, `forcats`, `ggbump`, `patchwork`, `shiny`,
  `stringr`) and leaving `ekioplot` a lean visualization package.

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
