# CLAUDE.md - Project Context

## Project: ekioplot
R package implementing EKIO's visual identity system for ggplot2 and gt tables.

## Description
A comprehensive R package that provides EKIO-branded themes, color palettes, scale functions, high-level "recipe" chart functions, and gt table theming for professional data visualizations. Follows EKIO design principles of clarity, purposeful color usage, and professional presentation standards.

## Brand tokens (canonical)

This package is EKIO's single source of truth for brand color and type. Downstream projects (the `ekio` workspace, `ekio-site`) mirror these — update here first.

- **Colors**: defined in `inst/ekio-palettes.yaml` (source of truth), compiled to `R/sysdata.rda` by `data-raw/palettes.R`. Eight nine-step scales named `100`–`900`, light to dark; position `i` == shade `i * 100`. Primary `blue.700` = `#1E3A5F`, ink `gray.900` = `#1A202C`, background `gray.100` = `#F7FAFC`; accents `orange.600` = `#DD6B20`, `teal.700` = `#2C7A7B`. Palettes are defined by `scale.shade` token reference, not literal hex.
- **Type — charts & gt tables**: Helvetica Neue (macOS) / Arial (Windows) via `.get_ekio_font()`; sans only, no serif in ggplot2/gt output.
- **Type — web (ekio-site)**: Lora (serif display/headings), Lato (body), Fira Code (mono). Lora is a deliberate web-only editorial display font; it is intentionally *not* used in chart output, which stays on the sans stack above.

## Package Architecture

### Source Files (R/)
- **colors.R** — `ekio_pal()` palette accessor (auto-displays swatch via S3 print method), internal `.ekio(scale, shade)` token accessor used by themes/recipes/gt, `list_ekio_palettes()`, `show_ekio_palette()` (deprecated, use `ekio_pal()`), `show_all_ekio_palettes()` (deprecated, use `list_ekio_palettes(verbose = TRUE)`). Contains no hex codes — all color comes from `R/sysdata.rda`

- **scales.R** — Discrete (`scale_color_ekio_d`, `scale_fill_ekio_d`) and continuous (`scale_color_ekio_c`, `scale_fill_ekio_c`) ggplot2 scale functions with British spelling aliases
- **theme.R** — `theme_ekio()` (modular, uses `theme_sub_*` helpers). Platform-aware font selection via `.get_ekio_font()`
- **recipes.R** — High-level chart builders (`ekio_histogram`, `ekio_lineplot`, `ekio_scatterplot`, `ekio_barplot`) with smart aesthetic detection (static color vs. variable mapping)
- **gt_theme.R** — `gt_theme_ekio()` for professional gt table styling
- **data.R** — Documentation for 6 bundled datasets (Brazilian socioeconomic/agriculture data, global fuels)
- **utils.R** — Package-level imports and `globalVariables` suppression

### Key Design Decisions
- **Unified palette access**: All palette types (categorical, small-group, scientific, sequential, diverging) go through `ekio_pal()`. Sequential/diverging palettes are also usable in continuous scales
- **Smart aesthetic detection**: Recipe functions use `rlang::enquo()` + internal `.detect_aesthetic_type()` to distinguish between missing args, static color strings, and variable mappings — auto-selecting appropriate scales
- **Modular themes**: `theme_ekio()` uses ggplot2's `theme_sub_*()` helpers (requires ggplot2 >= 3.5.0)
- **Color references**: Never hardcode hex. Inside the package use `.ekio("blue", 700)`; from user code use `ekio_pal()`. To change a color, edit `inst/ekio-palettes.yaml` and rerun `data-raw/palettes.R`

### Dependencies
- **Imports**: cli, ggplot2 (>= 3.5.0), grDevices, gt, rlang
- **Suggests**: dplyr, knitr, rmarkdown, testthat (>= 3.0.0), tibble

## Coding Conventions
- **Documentation**: All functions use roxygen2 comments with `#'`
- **Naming**: snake_case for functions and variables (e.g., `theme_ekio`, `ekio_pal`)
- **Function structure**: Export functions with `@export`, include examples in `@examples`. Use `@examplesIf rlang::is_interactive()` for examples that produce plots
- **Testing**: Uses testthat framework in `tests/testthat/`. Tests exist for colors, scales, and themes. No tests yet for recipes or gt_theme
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
