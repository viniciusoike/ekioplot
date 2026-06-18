# CLAUDE.md - Project Context

## Project: ekioplot
R package implementing EKIO's visual identity system for ggplot2 and gt tables.

## Description
A comprehensive R package that provides EKIO-branded themes, color palettes, scale functions, high-level "recipe" chart functions, and gt table theming for professional data visualizations. Follows EKIO design principles of clarity, purposeful color usage, and professional presentation standards.

## Package Architecture

### Source Files (R/)
- **colors.R** — Color scale vectors (`ekio_blue`, `ekio_gray`, `ekio_teal`, `ekio_orange`, `ekio_accent`), internal sequential/diverging palette lists, `ekio_pal()` palette accessor, `list_ekio_palettes()`, `show_ekio_palette()`, `show_all_ekio_palettes()`
- **scales.R** — Discrete (`scale_color_ekio_d`, `scale_fill_ekio_d`) and continuous (`scale_color_ekio_c`, `scale_fill_ekio_c`) ggplot2 scale functions with British spelling aliases
- **theme.R** — `theme_ekio()` (modular, uses `theme_sub_*` helpers) and `theme_ekio_map()` for spatial maps. Platform-aware font selection via `.get_ekio_font()`
- **recipes.R** — High-level chart builders (`ekio_histogram`, `ekio_lineplot`, `ekio_scatterplot`, `ekio_barplot`) with smart aesthetic detection (static color vs. variable mapping)
- **gt_theme.R** — `gt_theme_ekio()` for professional gt table styling
- **data.R** — Documentation for 6 bundled datasets (Brazilian socioeconomic/agriculture data, global fuels)
- **utils.R** — Package-level imports and `globalVariables` suppression

### Key Design Decisions
- **Unified palette access**: All palette types (categorical, small-group, scientific, sequential, diverging) go through `ekio_pal()`. Sequential/diverging palettes are also usable in continuous scales
- **Smart aesthetic detection**: Recipe functions use `rlang::enquo()` + internal `.detect_aesthetic_type()` to distinguish between missing args, static color strings, and variable mappings — auto-selecting appropriate scales
- **Modular themes**: `theme_ekio()` uses ggplot2's `theme_sub_*()` helpers (requires ggplot2 >= 3.5.0)
- **Color references**: Theme and gt functions reference exported color vectors (`ekio_blue["700"]`) instead of hardcoded hex values

### Dependencies
- **Imports**: cli, ggplot2 (>= 3.5.0), grDevices, gt, rlang, scales
- **Suggests**: shiny, colourpicker, testthat (>= 3.0.0)

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
- The Shiny app in `inst/shiny-app/` is a palette explorer / visual testing tool
