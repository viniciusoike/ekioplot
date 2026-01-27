# EKIO Package Update Instructions for Claude Code

> **OUTDATED**: This document has been superseded by the implementation plan at
> `~/.claude/plans/drifting-coalescing-bear.md`. The new plan incorporates additional
> decisions: separate file for external palettes (viridis, okabe_ito), aesthetic
> detection feature, simplified fonts, GT themes kept, and visual testing via Quarto.
> This file is kept for reference only.

---

## Overview

This document provides comprehensive instructions for updating the `ekioplot` R package to implement EKIO Design System v2.0. The update consolidates and simplifies the color system, improves the theme implementation, and ensures consistency with professional consulting deliverables.

---

## Repository Information

- **Repository**: https://github.com/viniciusoike/ekioplot
- **Branch**: master
- **Package Type**: R package for ggplot2 visualization

---

## Phase 1: Color System Update

### 1.1 Create/Update `R/colors.R`

Replace or update the color definitions with the new consolidated system:

```r
# =============================================================================
# EKIO Design System v2.0 - Color Definitions
# =============================================================================

#' EKIO Primary Blue Scale
#' @description Brand identity color scale from light (#EEF5FA) to dark (#0D1B2A)
#' @export
ekio_blue <- c(
  "900" = "#0D1B2A",
  "800" = "#1B3A4B",

"700" = "#1E3A5F",
  "600" = "#2B4C7E",
  "500" = "#3A6EA5",
  "400" = "#4A90C2",
  "300" = "#7EB6D8",
  "200" = "#A8D0E8",
  "100" = "#D4E8F5",
  "50"  = "#EEF5FA"
)

#' EKIO Neutral Gray Scale
#' @description Neutral grays for text, backgrounds, and grids
#' @export
ekio_gray <- c(
  "900" = "#1A202C",
  "800" = "#2D3748",
  "700" = "#4A5568",
  "600" = "#718096",
  "500" = "#A0AEC0",
  "400" = "#CBD5E0",
  "300" = "#E2E8F0",
  "200" = "#EDF2F7",
  "100" = "#F7FAFC",
  "50"  = "#FAFBFC"
)

#' EKIO Teal Scale
#' @description Secondary brand color for complementary use
#' @export
ekio_teal <- c(
  "900" = "#234E52",
  "800" = "#285E61",
  "700" = "#2C7A7B",
  "600" = "#319795",
  "500" = "#38B2AC",
  "400" = "#4FD1C5",
  "300" = "#81E6D9",
  "200" = "#B2F5EA",
  "100" = "#E6FFFA",
  "50"  = "#F0FFF4"
)

#' EKIO Orange Scale
#' @description Accent color for highlights and contrast
#' @export
ekio_orange <- c(
  "900" = "#7B341E",
  "800" = "#9C4221",
  "700" = "#C05621",
  "600" = "#DD6B20",
  "500" = "#ED8936",
  "400" = "#F6AD55",
  "300" = "#FBD38D",
  "200" = "#FEEBC8",
  "100" = "#FFFAF0"
)

#' EKIO Named Accent Colors
#' @description Individual accent colors for quick access
#' @export
ekio_accent <- c(
  blue   = "#1E3A5F",
  orange = "#DD6B20",
  teal   = "#2C7A7B",
  amber  = "#D69E2E",
  purple = "#805AD5",
  red    = "#C53030",
  green  = "#38A169",
  gray   = "#718096"
)
```

### 1.2 Create/Update Qualitative Palette Function

```r
#' Get EKIO Qualitative Palette
#'
#' @param palette Character. Name of the palette. Options: "cool", "minimal",
#'   "contrast", "full", "muted", "binary", "political"
#' @param n Integer. Number of colors to return. If NULL, returns all colors.
#' @param reverse Logical. If TRUE, reverses the palette order.
#'
#' @return A character vector of hex color codes
#' @export
#'
#' @examples
#' ekio_pal("cool")
#' ekio_pal("contrast", n = 4)
#' ekio_pal("binary", reverse = TRUE)
ekio_pal <- function(palette = "contrast", n = NULL, reverse = FALSE) {

  palettes <- list(
    # Cool harmony: analogous blue-teal (2-3 categories)
    cool = c("#1E3A5F", "#4A90C2", "#2C7A7B"),

    # Minimal: blue + grays (subtle differentiation)
    minimal = c("#1E3A5F", "#4A5568", "#A0AEC0"),

    # Contrast: maximum distinguishability (4-6 categories)
    contrast = c("#1E3A5F", "#DD6B20", "#2C7A7B", "#D69E2E", "#805AD5", "#C53030"),

    # Full: extended palette (up to 8 categories)
    full = c("#1E3A5F", "#DD6B20", "#2C7A7B", "#D69E2E", "#805AD5", "#C53030", "#38A169", "#718096"),

    # Muted: subtle backgrounds/fills
    muted = c("#4A5568", "#718096", "#A0AEC0", "#CBD5E0", "#E2E8F0"),

    # Binary: two-group comparison (blue vs orange)
    binary = c("#1E3A5F", "#DD6B20"),

    # Political: blue vs red
    political = c("#1E3A5F", "#C53030")
  )

  pal <- palettes[[palette]]

  if (is.null(pal)) {
    stop(
      "Palette '", palette, "' not found. Available palettes: ",
      paste(names(palettes), collapse = ", ")
    )
  }

  if (reverse) pal <- rev(pal)
  if (!is.null(n)) pal <- pal[1:min(n, length(pal))]

  pal
}

#' List Available EKIO Palettes
#'
#' @param type Character. Type of palettes to list: "qualitative", "sequential",
#'   "diverging", or "all"
#'
#' @return A character vector of palette names
#' @export
list_ekio_palettes <- function(type = "all") {
  qual <- c("cool", "minimal", "contrast", "full", "muted", "binary", "political")
  seq <- c("blue", "teal", "gray", "orange")
  div <- c("blue_orange", "blue_red", "teal_amber")

  switch(type,
    qualitative = qual,
    sequential = seq,
    diverging = div,
    all = list(qualitative = qual, sequential = seq, diverging = div)
  )
}
```

### 1.3 Create Sequential Palette Function

```r
#' Get EKIO Sequential Palette
#'
#' @param palette Character. Name of the sequential palette: "blue", "teal",
#'   "gray", or "orange"
#' @param n Integer. Number of colors to generate (interpolated if needed)
#' @param reverse Logical. If TRUE, reverses light-to-dark order
#'
#' @return A character vector of hex color codes
#' @export
#'
#' @examples
#' ekio_seq_pal("blue")
#' ekio_seq_pal("blue", n = 5)
#' ekio_seq_pal("teal", reverse = TRUE)
ekio_seq_pal <- function(palette = "blue", n = 9, reverse = FALSE) {

  palettes <- list(
    blue = c(
      "#EEF5FA", "#D4E8F5", "#A8D0E8", "#7EB6D8", "#4A90C2",
      "#3A6EA5", "#2B4C7E", "#1E3A5F", "#0D1B2A"
    ),
    teal = c(
      "#E6FFFA", "#B2F5EA", "#81E6D9", "#4FD1C5", "#38B2AC",
      "#319795", "#2C7A7B", "#285E61", "#234E52"
    ),
    gray = c(
      "#FAFBFC", "#F7FAFC", "#EDF2F7", "#E2E8F0", "#CBD5E0",
      "#A0AEC0", "#718096", "#4A5568", "#2D3748", "#1A202C"
    ),
    orange = c(
      "#FFFAF0", "#FEEBC8", "#FBD38D", "#F6AD55", "#ED8936",
      "#DD6B20", "#C05621", "#9C4221", "#7B341E"
    )
  )

  pal <- palettes[[palette]]

  if (is.null(pal)) {
    stop(
      "Sequential palette '", palette, "' not found. Available: ",
      paste(names(palettes), collapse = ", ")
    )
  }

  if (reverse) pal <- rev(pal)

  # Interpolate if n differs from palette length
  if (n != length(pal)) {
    pal <- grDevices::colorRampPalette(pal)(n)
  }

  pal
}
```

### 1.4 Create Diverging Palette Function

```r
#' Get EKIO Diverging Palette
#'
#' @param palette Character. Name of the diverging palette: "blue_orange",
#'   "blue_red", or "teal_amber"
#' @param n Integer. Number of colors (should be odd for clear midpoint)
#' @param reverse Logical. If TRUE, reverses the palette
#'
#' @return A character vector of hex color codes
#' @export
#'
#' @examples
#' ekio_div_pal("blue_orange")
#' ekio_div_pal("blue_red", n = 11)
ekio_div_pal <- function(palette = "blue_orange", n = 7, reverse = FALSE) {

  palettes <- list(
    blue_orange = c(
      "#1E3A5F", "#3A6EA5", "#7EB6D8", "#F7FAFC",
      "#FEEBC8", "#ED8936", "#C05621"
    ),
    blue_red = c(
      "#1E3A5F", "#3A6EA5", "#7EB6D8", "#F7FAFC",
      "#FED7D7", "#E53E3E", "#C53030"
    ),
    teal_amber = c(
      "#234E52", "#2C7A7B", "#4FD1C5", "#F7FAFC",
      "#FAF089", "#D69E2E", "#975A16"
    )
  )

  pal <- palettes[[palette]]

  if (is.null(pal)) {
    stop(
      "Diverging palette '", palette, "' not found. Available: ",
      paste(names(palettes), collapse = ", ")
    )
  }

  if (reverse) pal <- rev(pal)

  if (n != length(pal)) {
    pal <- grDevices::colorRampPalette(pal)(n)
  }

  pal
}
```

---

## Phase 2: ggplot2 Scale Functions

### 2.1 Create/Update `R/scales.R`

```r
# =============================================================================
# EKIO ggplot2 Scale Functions
# =============================================================================

#' Discrete Color Scale for EKIO Palettes
#'
#' @param palette Character. Name of the qualitative palette
#' @param reverse Logical. Reverse the palette order
#' @param ... Additional arguments passed to discrete_scale
#'
#' @return A ggplot2 scale object
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_ekio_d("cool")
scale_color_ekio_d <- function(palette = "contrast", reverse = FALSE, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    scale_name = "ekio",
    palette = function(n) ekio_pal(palette, n, reverse),
    ...
  )
}

#' @rdname scale_color_ekio_d
#' @export
scale_colour_ekio_d <- scale_color_ekio_d

#' @rdname scale_color_ekio_d
#' @export
scale_fill_ekio_d <- function(palette = "contrast", reverse = FALSE, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    scale_name = "ekio",
    palette = function(n) ekio_pal(palette, n, reverse),
    ...
  )
}

#' Continuous Color Scale for EKIO Sequential Palettes
#'
#' @param palette Character. Name of the sequential palette
#' @param reverse Logical. Reverse the palette order
#' @param ... Additional arguments passed to scale_color_gradientn
#'
#' @return A ggplot2 scale object
#' @export
scale_color_ekio_c <- function(palette = "blue", reverse = FALSE, ...) {
  ggplot2::scale_color_gradientn(
    colours = ekio_seq_pal(palette, n = 9, reverse = reverse),
    ...
  )
}

#' @rdname scale_color_ekio_c
#' @export
scale_colour_ekio_c <- scale_color_ekio_c

#' @rdname scale_color_ekio_c
#' @export
scale_fill_ekio_c <- function(palette = "blue", reverse = FALSE, ...) {
  ggplot2::scale_fill_gradientn(
    colours = ekio_seq_pal(palette, n = 9, reverse = reverse),
    ...
  )
}

#' Diverging Color Scale for EKIO
#'
#' @param palette Character. Name of the diverging palette
#' @param midpoint Numeric. The midpoint value for the diverging scale
#' @param reverse Logical. Reverse the palette order
#' @param ... Additional arguments passed to scale_color_gradient2
#'
#' @return A ggplot2 scale object
#' @export
scale_color_ekio_div <- function(palette = "blue_orange", midpoint = 0,
                                  reverse = FALSE, ...) {
  colors <- ekio_div_pal(palette, n = 7, reverse = reverse)
  ggplot2::scale_color_gradientn(
    colours = colors,
    values = scales::rescale(c(-1, -0.5, -0.1, 0, 0.1, 0.5, 1)),
    ...
  )
}

#' @rdname scale_color_ekio_div
#' @export
scale_colour_ekio_div <- scale_color_ekio_div

#' @rdname scale_color_ekio_div
#' @export
scale_fill_ekio_div <- function(palette = "blue_orange", midpoint = 0,
                                 reverse = FALSE, ...) {
  colors <- ekio_div_pal(palette, n = 7, reverse = reverse)
  ggplot2::scale_fill_gradientn(
    colours = colors,
    values = scales::rescale(c(-1, -0.5, -0.1, 0, 0.1, 0.5, 1)),
    ...
  )
}
```

---

## Phase 3: Theme Implementation

### 3.1 Create/Update `R/theme.R`

```r
# =============================================================================
# EKIO ggplot2 Theme
# =============================================================================

#' EKIO Theme for ggplot2
#'
#' A professional, minimal theme optimized for consulting deliverables.
#' Features clear typography, subtle grids, and warm gray backgrounds.
#'
#' @param base_size Numeric. Base font size (default: 11)
#' @param base_family Character. Base font family
#' @param grid Character. Grid lines to display: "y" (default), "x", "xy", or "none"
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_ekio()
#'
#' # With x-y grid
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_ekio(grid = "xy")
theme_ekio <- function(base_size = 11, base_family = "", grid = "y") {

  # Color definitions
  colors <- list(
    text_dark   = "#1A202C",
    text_mid    = "#4A5568",
    text_light  = "#718096",
    text_muted  = "#A0AEC0",
    grid        = "#E2E8F0",
    background  = "#FAFBFC",
    panel       = "#FFFFFF"
  )

  # Grid element handling
  grid_y <- if (grid %in% c("y", "xy")) {
    ggplot2::element_line(color = colors$grid, linewidth = 0.4)
  } else {
    ggplot2::element_blank()
  }

  grid_x <- if (grid %in% c("x", "xy")) {
    ggplot2::element_line(color = colors$grid, linewidth = 0.4)
  } else {
    ggplot2::element_blank()
  }

  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Plot titles
      plot.title = ggplot2::element_text(
        size = ggplot2::rel(1.4),
        face = "bold",
        color = colors$text_dark,
        margin = ggplot2::margin(b = 8),
        hjust = 0
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(1.05),
        color = colors$text_light,
        margin = ggplot2::margin(b = 16),
        hjust = 0
      ),
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(0.8),
        color = colors$text_muted,
        margin = ggplot2::margin(t = 12),
        hjust = 0
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",

      # Axis titles
      axis.title = ggplot2::element_text(
        size = ggplot2::rel(0.95),
        color = colors$text_mid
      ),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),

      # Axis text
      axis.text = ggplot2::element_text(
        size = ggplot2::rel(0.85),
        color = colors$text_light
      ),

      # Grid
      panel.grid.major.y = grid_y,
      panel.grid.major.x = grid_x,
      panel.grid.minor = ggplot2::element_blank(),

      # Background
      panel.background = ggplot2::element_rect(fill = colors$panel, color = NA),
      plot.background = ggplot2::element_rect(fill = colors$background, color = NA),

      # Legend
      legend.position = "top",
      legend.justification = "left",
      legend.title = ggplot2::element_text(
        size = ggplot2::rel(0.9),
        color = colors$text_mid
      ),
      legend.text = ggplot2::element_text(
        size = ggplot2::rel(0.85),
        color = colors$text_light
      ),
      legend.key = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.margin = ggplot2::margin(b = 10),

      # Facets
      strip.text = ggplot2::element_text(
        size = ggplot2::rel(1),
        face = "bold",
        color = colors$text_dark,
        hjust = 0
      ),
      strip.background = ggplot2::element_blank(),

      # Margins
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    )
}

#' EKIO Map Theme
#'
#' A variant of theme_ekio optimized for choropleth maps and spatial visualizations.
#' Removes axis elements and positions legend on the right.
#'
#' @inheritParams theme_ekio
#'
#' @return A ggplot2 theme object
#' @export
theme_ekio_map <- function(base_size = 11, base_family = "") {
  theme_ekio(base_size = base_size, base_family = base_family, grid = "none") +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      legend.position = "right",
      legend.justification = "top"
    )
}

#' EKIO Presentation Theme
#'
#' A larger variant of theme_ekio optimized for presentations and slides.
#'
#' @inheritParams theme_ekio
#'
#' @return A ggplot2 theme object
#' @export
theme_ekio_presentation <- function(base_size = 16, base_family = "", grid = "y") {
  theme_ekio(base_size = base_size, base_family = base_family, grid = grid) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.6)),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(1.1)),
      plot.margin = ggplot2::margin(30, 30, 30, 30)
    )
}
```

---

## Phase 4: Utility Functions

### 4.1 Create `R/show_palette.R`

```r
#' Display EKIO Palette
#'
#' Visualizes a color palette as a horizontal bar chart.
#'
#' @param palette Character or vector. Either a palette name or a vector of colors
#' @param type Character. Type of palette: "qualitative", "sequential", or "diverging"
#' @param n Integer. Number of colors for sequential/diverging palettes
#' @param labels Logical. Show hex codes as labels
#'
#' @return A ggplot2 object (invisibly)
#' @export
#'
#' @examples
#' show_ekio_palette("contrast")
#' show_ekio_palette("blue", type = "sequential")
show_ekio_palette <- function(palette, type = "qualitative", n = 9, labels = TRUE) {

  # Get colors based on type
  colors <- switch(type,
    qualitative = ekio_pal(palette),
    sequential = ekio_seq_pal(palette, n = n),
    diverging = ekio_div_pal(palette, n = n),
    palette  # If vector of colors passed directly
  )

  # Create data frame
  df <- data.frame(
    x = seq_along(colors),
    color = colors,
    stringsAsFactors = FALSE
  )

  # Build plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = 1, fill = color)) +
    ggplot2::geom_tile(color = "white", linewidth = 2) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::labs(
      title = if (is.character(palette) && length(palette) == 1) {
        paste("EKIO Palette:", palette)
      } else {
        "EKIO Palette"
      }
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5, face = "bold", margin = ggplot2::margin(b = 10)
      ),
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    )

  if (labels) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = color),
      size = 3,
      color = ifelse(
        grDevices::col2rgb(colors)[1, ] * 0.299 +
        grDevices::col2rgb(colors)[2, ] * 0.587 +
        grDevices::col2rgb(colors)[3, ] * 0.114 > 150,
        "#1A202C", "#FFFFFF"
      )
    )
  }

  print(p)
  invisible(p)
}

#' Show All EKIO Palettes
#'
#' Displays a grid of all available palettes.
#'
#' @return A ggplot2 object (invisibly)
#' @export
show_all_ekio_palettes <- function() {
  message("Run show_ekio_palette() with each palette name to view individually:")
  message("\nQualitative: ", paste(list_ekio_palettes("qualitative"), collapse = ", "))
  message("Sequential: ", paste(list_ekio_palettes("sequential"), collapse = ", "))
  message("Diverging: ", paste(list_ekio_palettes("diverging"), collapse = ", "))
}
```

---

## Phase 5: Update Package Files

### 5.1 Update DESCRIPTION

Ensure DESCRIPTION includes:

```
Package: ekioplot
Title: EKIO Visual Identity System for R Data Visualization
Version: 2.0.0
Authors@R:
    person("Vinicius", "Oike", , "vinicius@ekio.io", role = c("aut", "cre"))
Description: Comprehensive implementation of EKIO's visual identity system for
    data visualization. Provides professional themes, color palettes, and scale
    functions for ggplot2. Includes qualitative, sequential, and diverging
    palettes optimized for consulting deliverables.
License: MIT + file LICENSE
Encoding: UTF-8
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.3.1
Imports:
    ggplot2 (>= 3.4.0),
    scales,
    grDevices
Suggests:
    testthat (>= 3.0.0),
    ggridges
Config/testthat/edition: 3
```

### 5.2 Update NAMESPACE

After running `devtools::document()`, ensure exports include:

```
# Colors
export(ekio_blue)
export(ekio_gray)
export(ekio_teal)
export(ekio_orange)
export(ekio_accent)

# Palette functions
export(ekio_pal)
export(ekio_seq_pal)
export(ekio_div_pal)
export(list_ekio_palettes)

# Scales
export(scale_color_ekio_d)
export(scale_colour_ekio_d)
export(scale_fill_ekio_d)
export(scale_color_ekio_c)
export(scale_colour_ekio_c)
export(scale_fill_ekio_c)
export(scale_color_ekio_div)
export(scale_colour_ekio_div)
export(scale_fill_ekio_div)

# Themes
export(theme_ekio)
export(theme_ekio_map)
export(theme_ekio_presentation)

# Utilities
export(show_ekio_palette)
export(show_all_ekio_palettes)
```

### 5.3 Update README.md

```markdown
# ekioplot <img src="man/figures/logo.png" align="right" height="139" />
 
[![R-CMD-check](https://github.com/viniciusoike/ekioplot/workflows/R-CMD-check/badge.svg)](https://github.com/viniciusoike/ekioplot/actions)

## Overview

`ekioplot` implements EKIO's visual identity system for R data visualization.
The package provides themes, color palettes, and ggplot2 scales for creating
professional charts following EKIO design principles.

## Installation

```r
# Install from GitHub
devtools::install_github("viniciusoike/ekioplot")
```

## Quick Start

```r
library(ekioplot)
library(ggplot2)

# Basic usage with theme and colors
ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point(size = 3) +
  scale_color_ekio_d("cool") +
  theme_ekio() +
  labs(title = "Vehicle Fuel Efficiency")
```

## Color Palettes

### Qualitative Palettes

| Palette | Colors | Use Case |
|---------|--------|----------|
| `cool` | 3 | Blue-teal harmony for 2-3 categories |
| `minimal` | 3 | Blue + grays for subtle differentiation |
| `contrast` | 6 | Maximum distinguishability |
| `full` | 8 | Extended palette for many categories |
| `binary` | 2 | Two-group comparisons |

```r
# Get palette colors
ekio_pal("contrast", n = 4)

# Apply to ggplot2
scale_color_ekio_d("contrast")
scale_fill_ekio_d("cool")
```

### Sequential Palettes

For ordered data and choropleths:

```r
ekio_seq_pal("blue", n = 9)
scale_fill_ekio_c("blue")
```

### Diverging Palettes

For data with meaningful center point:
  
```r
ekio_div_pal("blue_orange", n = 7)
scale_fill_ekio_div("blue_orange")
```

## Themes

```r
# Standard theme (horizontal grid)
theme_ekio()

# Both grids for scatter plots
theme_ekio(grid = "xy")

# No grid for maps
theme_ekio_map()

# Larger text for presentations
theme_ekio_presentation()
```

## Color Reference

```r
# Primary brand color
ekio_blue["700"]  # "#1E3A5F"

# Accent colors
ekio_accent["orange"]  # "#DD6B20"
ekio_accent["teal"]    # "#2C7A7B"
```

---

*EKIO Economic Consulting*
```

---

## Phase 6: Testing

### 6.1 Create/Update Tests in `tests/testthat/`

Create `tests/testthat/test-colors.R`:

```r
test_that("ekio_pal returns correct colors", {
  expect_length(ekio_pal("cool"), 3)
  expect_length(ekio_pal("contrast"), 6)
  expect_length(ekio_pal("binary"), 2)
  
  # Test n parameter
  expect_length(ekio_pal("contrast", n = 3), 3)
  
  # Test reverse
  expect_equal(ekio_pal("binary", reverse = TRUE), rev(ekio_pal("binary")))
})

test_that("ekio_seq_pal interpolates correctly", {
  expect_length(ekio_seq_pal("blue", n = 5), 5)
  expect_length(ekio_seq_pal("blue", n = 15), 15)
})

test_that("ekio_div_pal returns correct structure", {
  pal <- ekio_div_pal("blue_orange")
  expect_length(pal, 7)
  # Middle color should be light (neutral)
  expect_equal(pal[4], "#F7FAFC")
})

test_that("invalid palette names throw errors", {
  expect_error(ekio_pal("nonexistent"))
  expect_error(ekio_seq_pal("nonexistent"))
  expect_error(ekio_div_pal("nonexistent"))
})
```

Create `tests/testthat/test-theme.R`:

```r
test_that("theme_ekio returns ggplot2 theme", {
  expect_s3_class(theme_ekio(), "theme")
})

test_that("theme grid parameter works", {
  theme_y <- theme_ekio(grid = "y")
  theme_none <- theme_ekio(grid = "none")
  
  # Grid settings should differ
  expect_false(identical(theme_y, theme_none))
})

test_that("theme variants exist",
 {
  expect_s3_class(theme_ekio_map(), "theme")
  expect_s3_class(theme_ekio_presentation(), "theme")
})
```

---

## Phase 7: Documentation

### 7.1 Run Documentation Generation

```r
# In R console
devtools::document()
devtools::check()
```

### 7.2 Update CLAUDE.md

```markdown
# EKIO Plot Package - Claude Code Instructions

## Package Overview

`ekioplot` is the R implementation of EKIO's visual identity system for data
visualization. Version 2.0 consolidates the color system and simplifies the API.

## Key Files

- `R/colors.R` - Color definitions and palette functions
- `R/scales.R` - ggplot2 scale functions
- `R/theme.R` - Theme implementation
- `R/show_palette.R` - Utility functions

## Color System

### Primary Brand Color
- EKIO Blue 700: `#1E3A5F`

### Qualitative Palettes
- `cool`: 3 colors, blue-teal harmony
- `contrast`: 6 colors, maximum distinguishability
- `binary`: 2 colors, blue vs orange

### Sequential Palettes
- `blue`, `teal`, `gray`, `orange`
- 9 colors each, light to dark

### Diverging Palettes
- `blue_orange`, `blue_red`, `teal_amber`
- 7 colors, neutral midpoint

## Build Commands

```bash
# Document and check
Rscript -e "devtools::document(); devtools::check()"

# Install locally
Rscript -e "devtools::install()"

# Run tests
Rscript -e "devtools::test()"
```

## Style Guidelines

- Use roxygen2 for documentation
- Follow tidyverse style guide
- All exported functions must have examples
- British spelling alternatives for "colour" functions
```

---

## Summary of Changes

| Component | Action | Priority |
|-----------|--------|----------|
| `R/colors.R` | Replace with v2.0 color system | High |
| `R/scales.R` | Update scale functions | High |
| `R/theme.R` | Simplify to single theme + variants | High |
| `DESCRIPTION` | Update version to 2.0.0 | Medium |
| `README.md` | Rewrite with new API | Medium |
| `tests/` | Add/update tests | Medium |
| `CLAUDE.md` | Update with new structure | Low |
| Old files | Remove deprecated theme variants | Low |

---

## Deprecation Notes

The following should be deprecated/removed:
- Theme style variants (modern_premium, academic_authority, etc.) → Use single `theme_ekio()`
- Complex font management → Use system fonts
- 35+ palette system → Consolidate to ~15 palettes

---

*Generated for EKIO Design System v2.0 Update*
