# Enhanced package dependencies and imports
#' @import ggplot2
#' @import scales
#' @importFrom grDevices colorRampPalette
#' @importFrom grDevices pdfFonts
#' @importFrom utils installed.packages
NULL

#' List All Available EKIO Palettes
#'
#' Returns a comprehensive list of all available EKIO color palettes
#'
#' @param category Filter by category: "all", "ekio", "scientific", "categorical", "mapping"
#' @return Character vector of palette names
#' @examples
#' # See all available palettes
#' list_ekio_palettes()
#'
#' # Filter by category
#' list_ekio_palettes("categorical")
#' list_ekio_palettes("scientific")
#'
#' @export
list_ekio_palettes <- function(category = "all") {

  ekio_palettes <- c(
    "modern_premium", "academic_authority", "sophisticated_unique",
    "institutional_oxford", "professional_deep", "premium_steel",
    "midnight_steel", "midnight_warm", "asphalt_gradient"
  )

  categorical_palettes <- c(
    "categorical", "categorical_extended", "categorical_warm",
    "categorical_cool", "categorical_mixed", "categorical_muted"
  )

  scientific_palettes <- c(
    "viridis", "inferno", "plasma", "hokusai1", "hokusai2", "okabe_ito"
  )

  mapping_palettes <- c(
    "RdBu", "BrBG", "PuBuGn", "YlOrRd", "Greens", "Blues"
  )

  switch(category,
         "ekio" = ekio_palettes,
         "categorical" = categorical_palettes,
         "scientific" = scientific_palettes,
         "mapping" = mapping_palettes,
         "all" = c(ekio_palettes, categorical_palettes, scientific_palettes, mapping_palettes)
  )
}

#' Display EKIO Palette Visually
#'
#' Creates a visual representation of an EKIO color palette
#'
#' @param name Palette name
#' @param n Number of colors to show
#' @param indices Custom indices to display
#' @param show_names Logical, whether to show color names/codes
#' @return ggplot2 object
#' @examples
#' \dontrun{
#' # Basic palette display
#' show_ekio_palette("modern_premium")
#'
#' # Show specific colors
#' show_ekio_palette("modern_premium", indices = c(9, 7, 5, 3))
#'
#' # Show with color codes
#' show_ekio_palette("categorical", show_names = TRUE)
#' }
#'
#' @export
show_ekio_palette <- function(name = "modern_premium", n = NULL, indices = NULL, show_names = FALSE) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' needed for this function.", call. = FALSE)
  }

  # Get colors
  if (!is.null(indices)) {
    colors <- ekio_palette(name, indices = indices)
    n <- length(colors)
  } else {
    colors <- ekio_palette(name, n = n)
    n <- length(colors)
  }

  # Create data frame
  palette_data <- data.frame(
    x = 1:n,
    y = 1,
    color = colors,
    name = if(show_names) colors else "",
    stringsAsFactors = FALSE
  )

  # Create plot
  p <- ggplot2::ggplot(palette_data, ggplot2::aes(x = x, y = y, fill = color)) +
    ggplot2::geom_tile(color = "white", linewidth = 1) +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_void() +
    ggplot2::labs(
      title = paste("EKIO", name, "Palette"),
      subtitle = paste("Showing", n, "colors")
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12, color = "gray60")
    )

  # Add color names if requested
  if (show_names) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = name),
      color = "white",
      size = 3,
      fontface = "bold",
      family = "mono"
    )
  }

  return(p)
}
