# CLAUDE.md - Project Context

## Project: ekioplot
R package implementing EKIO's visual identity system for ggplot2. Table styling lives in the companion `ekiotable` package.

## Description
A comprehensive R package that provides EKIO-branded themes, color palettes, scale functions and high-level "recipe" chart functions for professional data visualizations. Follows EKIO design principles of clarity, purposeful color usage, and professional presentation standards.

## Brand tokens (canonical)

This package is EKIO's single source of truth for brand color and type. Downstream projects (the `ekio` workspace, `ekio-site`) mirror these — update here first.

- **Colors**: defined in `inst/ekio-palettes.yaml` (source of truth), compiled to `R/sysdata.rda` by `data-raw/palettes.R`. Eight nine-step scales named `100`–`900`, light to dark; position `i` == shade `i * 100`. Primary `blue.700` = `#1E3A5F`, ink `gray.900` = `#1A202C`; accents `orange.600` = `#DD6B20`, `teal.700` = `#2C7A7B`. Palettes are defined by `scale.shade` token reference, not literal hex.
- **Surfaces**: the `basic` token group, not a scale — `offwhite` = `#FEFEFE` (the `theme_ekio()` default background), `white` = `#FFFFFF`, `black` = `#000000`. The `gray.100` tint (`#F7FAFC`) was the default background through 0.7.1 and is still reachable as `theme_ekio(background = "gray")`. `basic` is internal to `.ekio()`; it is deliberately absent from `ekio_pal()` and `list_ekio_palettes()`
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
- **Unified palette access**: All palette types (categorical, small-group, scientific, sequential, diverging) go through `ekio_pal()`. Sequential/diverging palettes are also usable in continuous scales
- **Smart aesthetic detection**: Recipe functions use `rlang::enquo()` + internal `.detect_aesthetic_type()` to distinguish between missing args, static color strings, and variable mappings — auto-selecting appropriate scales
- **One recipe layer builder**: recipes do not branch on the aesthetic type themselves. `.resolve_aes()` detects the type, applies the recipe's continuous policy and settles the palette; `.recipe_layer()` builds the `ggplot() + geom_*()` call and attaches the scale. Each recipe only supplies its base aesthetics, geom and geom arguments. Adding a recipe means calling these, not copying a branch tree
- **Continuous policy per chart**: `.resolve_aes(continuous = )` is `"reject"` for histogram, line, bar and area (a continuous color/fill errors, pointing at `factor()` or binning) and `"warn"` for scatter. Change the policy at the call site, never with an ad-hoc `if`
- **Modular themes**: `theme_ekio()` uses ggplot2's `theme_sub_*()` helpers and the `paper` argument to `theme_minimal()` (requires ggplot2 >= 4.0.0)
- **Namespace style**: no blanket `@import ggplot2`. Use `ggplot2::` prefixes; `@importFrom` is for files with many calls, like `theme.R`
- **Conditional grids**: `theme_ekio()` builds `grid_x` and `grid_y` only for the requested axes and adds them when they exist
- **Independent ticks**: `theme_ekio(ticks = "x" | "y" | "xy" | "none")` adds requested axis ticks and lines independently of major grids, allowing `grid = "none"` with visible axes
- **Color references**: Never hardcode hex. Inside the package use `.ekio("blue", 700)`; from user code use `ekio_pal()`. To change a color, edit `inst/ekio-palettes.yaml` and rerun `data-raw/palettes.R`

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
