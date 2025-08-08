# CLAUDE.md - Project Context

## Project: ekioplot
R package implementing EKIO's visual identity system for ggplot2

## Description
A comprehensive R package that provides EKIO-branded themes, color palettes, and font management for creating professional data visualizations following EKIO design principles of clarity, purposeful color usage, and professional presentation standards.

## Coding Conventions
- **Documentation**: All functions use roxygen2 comments with `#'` 
- **Naming**: Snake_case for functions and variables (e.g., `theme_ekio`, `ekio_colors`)
- **Function structure**: Export functions with `@export`, include examples in `@examples`
- **Dependencies**: Core dependencies in DESCRIPTION (ggplot2, scales, fonts packages)
- **Testing**: Uses testthat framework in `tests/testthat/` directory
- **Code style**: Standard R conventions with 2-space indentation, meaningful parameter names