# ---- EKIO Color Scales ----

#' EKIO Primary Blue Scale
#'
#' Brand identity color scale from light to dark.
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
#'
#' @format Named character vector with 8 accent colors
#' @export
#' @examples
#' ekio_accent["blue"]    # Primary blue
#' ekio_accent["orange"]  # Contrast accent
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

# ---- Internal Sequential Palettes (for continuous scales) ----

.seq_palettes <- list(
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

# ---- Internal Palette Validation ----

.validate_palette <- function(pal, palette_name, available) {
  if (is.null(pal)) {
    cli::cli_abort(c(
      "Palette {.val {palette_name}} not found.",
      "i" = "Available: {.val {available}}"
    ))
  }
  invisible(pal)
}

# ---- Palette Function ----

#' Get Color Palette
#'
#' Returns colors for data visualization. Includes EKIO brand palettes,
#' curated small-group variants, and standard scientific palettes.
#'
#' @param palette Character. Name of the palette. See [list_ekio_palettes()]
#'   for all available options.
#' @param n Integer or NULL. Number of colors to return. If NULL, returns all.
#' @param reverse Logical. If TRUE, reverses the palette order.
#'
#' @return Character vector of hex color codes
#' @export
#'
#' @examples
#' ekio_pal("contrast")
#' ekio_pal("contrast", n = 4)
#' ekio_pal("binary", reverse = TRUE)
#' ekio_pal("okabe_ito")
ekio_pal <- function(palette = "contrast", n = NULL, reverse = FALSE) {

  palettes <- list(
    # ---- EKIO categorical palettes ----
    cool     = c("#1E3A5F", "#4A90C2", "#2C7A7B"),
    minimal  = c("#1E3A5F", "#4A5568", "#A0AEC0"),
    contrast = c("#1E3A5F", "#DD6B20", "#2C7A7B", "#D69E2E", "#805AD5", "#C53030"),
    full     = c(
      "#1E3A5F", "#DD6B20", "#2C7A7B", "#D69E2E",
      "#805AD5", "#C53030", "#38A169", "#718096"
    ),
    muted    = c("#4A5568", "#718096", "#A0AEC0", "#CBD5E0", "#E2E8F0"),
    binary   = c("#1E3A5F", "#DD6B20"),
    political = c("#1E3A5F", "#C53030"),

    # ---- Named small-group variants (curated for 2/3/4 groups) ----
    duo_warm   = c("#DD6B20", "#D69E2E"),
    duo_cool   = c("#1E3A5F", "#2C7A7B"),
    trio_bold  = c("#1E3A5F", "#DD6B20", "#2C7A7B"),
    trio_cool  = c("#3A6EA5", "#2C7A7B", "#805AD5"),
    quad_earth = c("#1E3A5F", "#DD6B20", "#2C7A7B", "#38A169"),
    quad_vivid = c("#3A6EA5", "#DD6B20", "#805AD5", "#C53030"),

    # ---- Scientific / external palettes ----
    okabe_ito = c(
      "#E69F00", "#56B4E9", "#009E73", "#F0E442",
      "#0072B2", "#D55E00", "#CC79A7", "#000000"
    ),
    viridis = c(
      "#440154", "#482677", "#3f4a8a", "#31678e", "#26838f",
      "#1f9d8a", "#6cce5a", "#b6de2b", "#fee825"
    ),
    inferno = c(
      "#000004", "#1b0c41", "#4a0c6b", "#781c6d", "#a52c60",
      "#cf4446", "#ed6925", "#fb9b06", "#f7d03c", "#fcffa4"
    ),
    plasma = c(
      "#0d0887", "#46039f", "#7201a8", "#9c179e", "#bd3786",
      "#d8576b", "#ed7953", "#fb9f3a", "#fdca26", "#f0f921"
    )
  )

  pal <- palettes[[palette]]
  .validate_palette(pal, palette, names(palettes))

  if (reverse) pal <- rev(pal)

  if (!is.null(n)) {
    if (n > length(pal)) {
      pal <- grDevices::colorRampPalette(pal)(n)
    } else {
      pal <- pal[seq_len(n)]
    }
  }

  pal
}

# ---- Palette Listing ----

#' List Available Palettes
#'
#' Returns names of all available palettes, optionally filtered by type.
#'
#' @param type Character. Type of palettes to list:
#'   "categorical", "small_group", "scientific", "sequential", or "all" (default).
#'
#' @return Character vector of palette names, or named list if type = "all"
#' @export
#'
#' @examples
#' list_ekio_palettes()
#' list_ekio_palettes("categorical")
list_ekio_palettes <- function(type = "all") {
  categorical <- c("cool", "minimal", "contrast", "full", "muted", "binary", "political")
  small_group <- c("duo_warm", "duo_cool", "trio_bold", "trio_cool", "quad_earth", "quad_vivid")
  scientific  <- c("okabe_ito", "viridis", "inferno", "plasma")
  sequential  <- names(.seq_palettes)

  switch(type,
    categorical = categorical,
    small_group = small_group,
    scientific  = scientific,
    sequential  = sequential,
    all = list(
      categorical = categorical,
      small_group = small_group,
      scientific  = scientific,
      sequential  = sequential
    )
  )
}

# ---- Palette Visualization ----

#' Display a Palette
#'
#' Visualizes a color palette as a horizontal bar chart with hex labels.
#'
#' @param palette Character or vector. Either a palette name or a vector of hex colors.
#' @param n Integer. Number of colors (used for interpolation on sequential palettes).
#' @param labels Logical. Show hex codes as labels (default: TRUE).
#'
#' @return A ggplot2 object (invisibly)
#' @export
#'
#' @examplesIf rlang::is_interactive()
#' show_ekio_palette("contrast")
#' show_ekio_palette(c("#1E3A5F", "#DD6B20", "#2C7A7B"))
show_ekio_palette <- function(palette, n = NULL, labels = TRUE) {

  if (length(palette) > 1) {
    colors <- palette
    title <- "Custom Palette"
  } else {
    colors <- ekio_pal(palette, n = n)
    title <- paste("Palette:", palette)
  }

  df <- data.frame(x = seq_along(colors), color = colors, stringsAsFactors = FALSE)

  # Text color based on luminance
  rgb_vals <- grDevices::col2rgb(colors)
  luminance <- rgb_vals[1, ] * 0.299 + rgb_vals[2, ] * 0.587 + rgb_vals[3, ] * 0.114
  text_colors <- ifelse(luminance > 150, "#1A202C", "#FFFFFF")

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = 1, fill = color)) +
    ggplot2::geom_tile(color = "white", linewidth = 2) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::labs(title = title) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", margin = ggplot2::margin(b = 10)),
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    )

  if (labels) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = color), size = 3, color = text_colors)
  }

  print(p)
  invisible(p)
}

#' Show All Palettes
#'
#' Lists all available palettes organized by type.
#'
#' @return NULL (invisibly). Prints palette information to console.
#' @export
#'
#' @examples
#' show_all_ekio_palettes()
show_all_ekio_palettes <- function() {

  palettes <- list_ekio_palettes("all")

  cli::cli_h1("Available Palettes")

  cli::cli_h2("Categorical")
  cli::cli_text("{.val {palettes$categorical}}")

  cli::cli_h2("Small Group Variants")
  cli::cli_text("{.val {palettes$small_group}}")

  cli::cli_h2("Scientific")
  cli::cli_text("{.val {palettes$scientific}}")

  cli::cli_h2("Sequential (for continuous scales)")
  cli::cli_text("{.val {palettes$sequential}}")

  cli::cli_text("")
  cli::cli_alert_info("Use {.code show_ekio_palette(\"name\")} to visualize")

  invisible(NULL)
}
