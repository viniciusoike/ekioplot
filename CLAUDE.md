# CLAUDE.md - Project Context

## Project: ekioplot
R package implementing EKIO's visual identity system for ggplot2. Table styling lives in the companion `ekiotable` package.

## Description
A comprehensive R package that provides EKIO-branded themes, color palettes, scale functions and high-level "recipe" chart functions for professional data visualizations. Follows EKIO design principles of clarity, purposeful color usage, and professional presentation standards.

## Brand tokens (canonical)

This package is EKIO's single source of truth for brand color and type. Downstream projects (the `ekio` workspace, `ekio-site`) mirror these — update here first.

- **Colors**: defined in `inst/ekio-palettes.yaml` (source of truth), compiled to `R/sysdata.rda` by `data-raw/palettes.R`. Seven nine-step scales — `blue`, `gray`, `stone`, `teal`, `green`, `orange`, `red` — named `100`–`900`, light to dark; position `i` == shade `i * 100`. Primary `blue.700` = `#1E3A5F`, ink `gray.900` = `#191A1C`; accents `orange.400` = `#D3742A`, `gold.light` = `#D5AA48`, `teal.600` = `#006261`. Palettes are defined by token reference, not literal hex.
- **Gold is an accent, not a scale**: three named tokens (`light` `#D5AA48`, `mid` `#B88715`, `deep` `#966800`) on spine rungs 300/400/500, because a nine-step gold ramp turns brown at the dark end. Reached as `ekio_pal("gold")["mid"]` or `.ekio("gold", "mid")`. `deep` carries the text-safe promise that shade 500 carries for the scales. No `gold` sequential palette exists.
- **Token references**: a palette member is a literal hex, a `scale.shade` (`blue.700`), or a `group.name` pointing at a named token group (`gold.light`, `basic.pivot`). Any palette written as a YAML mapping rather than a sequence becomes such a group. Scales win on name collision.
- **Generated, not hand-picked**: the scales come from one OKLCH spec in `data-raw/build-ramps.R` — a shared lightness spine (pinned so `blue.700` is the brand navy), a shared chroma arc, and a hue path plus chroma budget per family. `data-raw/palettes.R` checks the YAML hex against the spec and refuses to build on drift. To change a color, edit the spec, rerun `Rscript data-raw/build-ramps.R`, paste the block into the YAML, then rerun `data-raw/palettes.R`. Never hand-edit a scale.
- **What the spine buys**: shade number means one visual weight in every family, so `blue.500` and `orange.500` are interchangeable. Shade `500` clears WCAG AA on the off-white surface in all seven scales. Both properties are asserted at build time and in `tests/testthat/test-palette-data.R`.
- **Categorical palettes vary shade on purpose**: equal lightness balances hues but makes them hard to tell apart, so each series in `contrast` sits on its own rung. `contrast` (5) survives grayscale and deuteranopia; `full` (8) separates by hue past five categories and does not.
- **Surfaces**: the `basic` token group, not a scale — `offwhite` = `#FEFEFE` (the `theme_ekio()` default background), `white` = `#FFFFFF`, `pivot` = `#F5F3EF` (the diverging midpoint), `black` = `#000000`. The `gray.100` tint (`#F2F3F5`) is reachable as `theme_ekio(background = "gray")`. White, offwhite and black are the only hand-set hex in the package; the pivot is generated with the scales. `basic` is internal to `.ekio()` and deliberately absent from `ekio_pal()` and `list_ekio_palettes()`; the `accent` group, by contrast, is user-facing
- **Type — web (ekio-site)**: Lora (serif display/headings), Lato (body), Fira Code (mono).

## Package Architecture

### Source Files (R/)
- **colors.R** — `ekio_pal()` palette accessor (auto-displays swatch via S3 print method; `.palette_plot()` builds it), internal `.ekio(group, n)` token accessor used by themes and recipes, and `list_ekio_palettes()`. Contains no hex codes — all color comes from `R/sysdata.rda`

- **scales.R** — Discrete (`scale_color_ekio_d`, `scale_fill_ekio_d`) and continuous (`scale_color_ekio_c`, `scale_fill_ekio_c`) ggplot2 scale functions with British spelling aliases
- **theme.R** — `theme_ekio()` (modular, uses `theme_sub_*` helpers) and internal `detect_font()`, which resolves a requested font family against installed fonts with a fallback chain
- **recipes.R** — High-level chart builders (`ekio_histogram`, `ekio_lineplot`, `ekio_scatterplot`, `ekio_barplot`, `ekio_areaplot`) with smart aesthetic detection (static color vs. variable mapping)
- **accessibility.R** — WCAG contrast helpers: `ekio_contrast()` (contrast ratio) and `ekio_text_on()` (black/white text picker for colored fills). `print.ekio_palette()` uses `ekio_text_on()` for swatch labels — keep luminance math here only
- **data.R** — Documentation for 6 bundled datasets (Brazilian socioeconomic/agriculture data, global fuels)
- **utils.R** — Package-level `@importFrom` tags only

### Key Design Decisions
- **Unified palette access**: All palette types (accent, categorical, highlight, small-group, scientific, sequential, diverging) go through `ekio_pal()`. Sequential/diverging palettes are also usable in continuous scales
- **Smart aesthetic detection**: Recipe functions use `rlang::enquo()` + internal `.detect_aesthetic_type()` to distinguish between missing args, static color strings, and variable mappings — auto-selecting appropriate scales
- **One recipe layer builder**: recipes do not branch on the aesthetic type themselves. `.resolve_aes()` detects the type, applies the recipe's continuous policy and settles the palette; `.recipe_layer()` builds the `ggplot() + geom_*()` call and attaches the scale. Each recipe only supplies its base aesthetics, geom and geom arguments. Adding a recipe means calling these, not copying a branch tree
- **Continuous policy per chart**: `.resolve_aes(continuous = )` is `"reject"` for histogram, line, bar and area (a continuous color/fill errors, pointing at `factor()` or binning) and `"warn"` for scatter. Change the policy at the call site, never with an ad-hoc `if`
- **Modular themes**: `theme_ekio()` uses ggplot2's `theme_sub_*()` helpers and the `paper` argument to `theme_minimal()` (requires ggplot2 >= 4.0.0)
- **Namespace style**: no blanket `@import ggplot2`. Use `ggplot2::` prefixes; `@importFrom` is for files with many calls, like `theme.R`
- **Conditional grids**: `theme_ekio()` builds `grid_x` and `grid_y` only for the requested axes and adds them when they exist
- **Independent ticks**: `theme_ekio(ticks = "x" | "y" | "xy" | "none")` adds requested axis ticks and lines independently of major grids, allowing `grid = "none"` with visible axes
- **Color references**: Never hardcode hex. Inside the package use `.ekio("blue", 700)`; from user code use `ekio_pal()`. To change a color, edit the spec in `data-raw/build-ramps.R`, regenerate the YAML `scales:` block, then rerun `data-raw/palettes.R`

### Dependencies
- **Imports**: cli, ggplot2 (>= 4.0.0), grDevices, rlang, systemfonts
- **Suggests**: dplyr, knitr, rmarkdown, testthat (>= 3.0.0), tibble, yaml

## Coding Conventions
- **Documentation**: All functions use roxygen2 comments with `#'`
- **Naming**: snake_case for functions and variables (e.g., `theme_ekio`, `ekio_pal`)
- **Function structure**: Export functions with `@export`, include examples in `@examples`. Use `@examplesIf rlang::is_interactive()` for examples that produce plots
- **Testing**: Uses testthat framework in `tests/testthat/`. Tests exist for accessibility, colors, palette data, recipes, scales, and themes
- **Code style**: Standard R conventions with 2-space indentation, meaningful parameter names
    - Use native R pipe
    - Don't make pipe chains too long (max 5-6 functions, max 15-20 lines)
    - Follow tidyverse guide style
    - Avoid cat, always prefer cli. Don't be too verbose, always be sparing with messages and write concise messages

## Comment style
- Be concise
- Use `# ---- Section Name ----` to separate sections

## General observations
- After making changes, run `check()` and fix only errors and warnings. Ignore notes
- The Palette Lab Shiny app lives in its own repository (`viniciusoike/ekioplot-palette-lab`), not in this package. It depends on `ekioplot`'s stable exported functions
