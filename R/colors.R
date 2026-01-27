# =============================================================================
# EKIO Design System v2.0 - Color Definitions
# =============================================================================

#' EKIO Primary Blue Scale
#'
#' Brand identity color scale from light (#EEF5FA) to dark (#0D1B2A).
#' Use for primary visualizations, sequential color scales, and brand consistency.
#'
#' @format Named character vector with 10 shades (50-900)
#' @export
#' @examples
#' ekio_blue["700"]  # Primary brand blue
#' ekio_blue["200"]  # Light blue for backgrounds
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
#'
#' Neutral grays for text, backgrounds, borders, and grids.
#' Designed to complement the blue scale without competing.
#'
#' @format Named character vector with 10 shades (50-900)
#' @export
#' @examples
#' ekio_gray["900"]  # Darkest text
#' ekio_gray["300"]  # Grid lines
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
#'
#' Secondary brand color for complementary use.
#' Works well alongside blue for two-variable sequential encoding.
#'
#' @format Named character vector with 10 shades (50-900)
#' @export
#' @examples
#' ekio_teal["700"]  # Strong teal
#' ekio_teal["300"]  # Light teal accent
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
#'
#' Accent color for highlights, warnings, and contrast against blue.
#' Use sparingly to draw attention to key data points.
#'
#' @format Named character vector with 9 shades (100-900)
#' @export
#' @examples
#' ekio_orange["600"]  # Primary orange accent
#' ekio_orange["300"]  # Soft orange for fills
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
#'
#' Individual accent colors for quick access.
#' These are the "hero" colors from each scale, ideal for categorical data.
#'
#' @format Named character vector with 8 accent colors
#' @export
#' @examples
#' ekio_accent["blue"]    # Primary blue
#' ekio_accent["orange"]  # Contrast accent
#' ekio_accent["teal"]    # Secondary accent
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

# ---- Internal Helper ----

#' Validate Palette Exists
#'
#' @param pal The palette value (possibly NULL if not found)
#' @param palette_name Name requested by user
#' @param available Character vector of available palette names
#' @param prefix Optional prefix for error message (e.g., "Sequential ", "External ")
#' @return The palette (invisibly) if valid
#' @keywords internal
#' @noRd
.validate_palette <- function(pal, palette_name, available, prefix = "") {
  if (is.null(pal)) {
    cli::cli_abort(c(
      "{prefix}palette {.val {palette_name}} not found.",
      "i" = "Available: {.val {available}}"
    ))
  }
  invisible(pal)
}

# ---- Qualitative Palette Function ----

#' Get EKIO Qualitative Palette
#'
#' Returns colors optimized for categorical data visualization.
#' Each palette is designed for specific use cases with tested color harmony.
#'
#' @param palette Character. Name of the palette. Options:
#'   \itemize{
#'     \item "cool": 3 colors, blue-teal harmony (2-3 categories)
#'     \item "minimal": 3 colors, blue + grays (subtle differentiation)
#'     \item "contrast": 6 colors, maximum distinguishability (default)
#'     \item "full": 8 colors, extended palette for many categories
#'     \item "muted": 5 colors, subtle for backgrounds/fills
#'     \item "binary": 2 colors, blue vs orange comparisons
#'     \item "political": 2 colors, blue vs red
#'   }
#' @param n Integer or NULL. Number of colors to return. If NULL, returns all.
#' @param reverse Logical. If TRUE, reverses the palette order.
#'
#' @return Character vector of hex color codes
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
    full = c(
      "#1E3A5F", "#DD6B20", "#2C7A7B", "#D69E2E",
      "#805AD5", "#C53030", "#38A169", "#718096"
    ),

    # Muted: subtle backgrounds/fills
    muted = c("#4A5568", "#718096", "#A0AEC0", "#CBD5E0", "#E2E8F0"),

    # Binary: two-group comparison (blue vs orange)
    binary = c("#1E3A5F", "#DD6B20"),

    # Political: blue vs red
    political = c("#1E3A5F", "#C53030")
  )

  pal <- palettes[[palette]]
  .validate_palette(pal, palette, names(palettes))

  if (reverse) pal <- rev(pal)
  if (!is.null(n)) pal <- pal[seq_len(min(n, length(pal)))]

  pal
}

# ---- Sequential Palette Function ----

#' Get EKIO Sequential Palette
#'
#' Returns colors for ordered/continuous data visualization.
#' Palettes go from light to dark by default.
#'
#' @param palette Character. Name of the sequential palette:
#'   \itemize{
#'     \item "blue": Primary brand sequential (default)
#'     \item "teal": Secondary brand sequential
#'     \item "gray": Neutral sequential
#'     \item "orange": Warm accent sequential
#'   }
#' @param n Integer. Number of colors to generate (interpolated if needed).
#' @param reverse Logical. If TRUE, reverses light-to-dark order.
#'
#' @return Character vector of hex color codes
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
  .validate_palette(pal, palette, names(palettes), prefix = "Sequential ")

  if (reverse) pal <- rev(pal)

  # Interpolate if n differs from palette length
  if (n != length(pal)) {
    pal <- grDevices::colorRampPalette(pal)(n)
  }

  pal
}

# ---- Diverging Palette Function ----

#' Get EKIO Diverging Palette
#'
#' Returns colors for data with meaningful center point (e.g., zero, average).
#' Middle color is neutral; extremes show opposite directions.
#'
#' @param palette Character. Name of the diverging palette:
#'   \itemize{
#'     \item "blue_orange": Primary diverging (default)
#'     \item "blue_red": Alternative for political/sentiment data
#'     \item "teal_amber": Warmer alternative
#'   }
#' @param n Integer. Number of colors (odd numbers recommended for clear midpoint).
#' @param reverse Logical. If TRUE, reverses the palette.
#'
#' @return Character vector of hex color codes
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
  .validate_palette(pal, palette, names(palettes), prefix = "Diverging ")

  if (reverse) pal <- rev(pal)

  if (n != length(pal)) {
    pal <- grDevices::colorRampPalette(pal)(n)
  }

  pal
}

# ---- Utility Functions ----

#' List Available EKIO Palettes
#'
#' Returns names of available palettes, optionally filtered by type.
#'
#' @param type Character. Type of palettes to list:
#'   \itemize{
#'     \item "qualitative": Categorical palettes (ekio_pal)
#'     \item "sequential": Ordered data palettes (ekio_seq_pal)
#'     \item "diverging": Centered data palettes (ekio_div_pal)
#'     \item "all": All palette types (default)
#'   }
#'
#' @return Character vector of palette names, or named list if type = "all"
#' @export
#'
#' @examples
#' list_ekio_palettes()
#' list_ekio_palettes("qualitative")
#' list_ekio_palettes("sequential")
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
