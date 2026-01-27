# =============================================================================
# Palette Visualization Utilities
# =============================================================================

#' Display EKIO Palette
#'
#' Visualizes a color palette as a horizontal bar chart with optional hex labels.
#'
#' @param palette Character or vector. Either a palette name or a vector of colors.
#' @param type Character. Type of palette if using a name:
#'   \itemize{
#'     \item "qualitative": Use \code{ekio_pal()} (default)
#'     \item "sequential": Use \code{ekio_seq_pal()}
#'     \item "diverging": Use \code{ekio_div_pal()}
#'   }
#' @param n Integer. Number of colors for sequential/diverging palettes.
#' @param labels Logical. Show hex codes as labels (default: TRUE).
#'
#' @return A ggplot2 object (invisibly)
#' @export
#'
#' @examplesIf rlang::is_interactive()
#' # Qualitative palette
#' show_ekio_palette("contrast")
#'
#' # Sequential palette
#' show_ekio_palette("blue", type = "sequential")
#'
#' # Diverging palette
#' show_ekio_palette("blue_orange", type = "diverging")
#'
#' # Custom colors
#' show_ekio_palette(c("#1E3A5F", "#DD6B20", "#2C7A7B"))
show_ekio_palette <- function(palette, type = "qualitative", n = 9, labels = TRUE) {

  # Get colors based on input
  if (length(palette) > 1) {
    # Vector of colors passed directly
    colors <- palette
    title <- "Custom Palette"
  } else {
    # Palette name
    colors <- switch(type,
      qualitative = ekio_pal(palette),
      sequential = ekio_seq_pal(palette, n = n),
      diverging = ekio_div_pal(palette, n = n),
      ekio_pal(palette)  # default
    )
    title <- paste("EKIO Palette:", palette)
  }

  # Create data frame
  df <- data.frame(
    x = seq_along(colors),
    color = colors,
    stringsAsFactors = FALSE
  )

  # Determine text color based on luminance
  get_text_color <- function(hex_colors) {
    rgb_vals <- grDevices::col2rgb(hex_colors)
    luminance <- (rgb_vals[1, ] * 0.299 + rgb_vals[2, ] * 0.587 + rgb_vals[3, ] * 0.114)
    ifelse(luminance > 150, "#1A202C", "#FFFFFF")
  }

  # Build plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = 1, fill = color)) +
    ggplot2::geom_tile(color = "white", linewidth = 2) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::labs(title = title) +
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
      color = get_text_color(colors)
    )
  }

  print(p)
  invisible(p)
}

#' Show All EKIO Palettes
#'
#' Lists all available EKIO palettes organized by type.
#' Use \code{show_ekio_palette()} to visualize individual palettes.
#'
#' @return NULL (invisibly). Prints palette information to console.
#' @export
#'
#' @examples
#' show_all_ekio_palettes()
show_all_ekio_palettes <- function() {

  palettes <- list_ekio_palettes("all")

  cli::cli_h1("EKIO Palettes")

  cli::cli_h2("Qualitative (use with ekio_pal)")
  cli::cli_text("{.val {palettes$qualitative}}")

  cli::cli_h2("Sequential (use with ekio_seq_pal)")
  cli::cli_text("{.val {palettes$sequential}}")

  cli::cli_h2("Diverging (use with ekio_div_pal)")
  cli::cli_text("{.val {palettes$diverging}}")

  cli::cli_text("")
  cli::cli_alert_info("Use {.code show_ekio_palette(\"name\")} to visualize a palette")
  cli::cli_alert_info("Use {.code list_external_palettes()} for non-EKIO palettes")

  invisible(NULL)
}
