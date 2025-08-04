#' Generate EKIO Color Palettes with Custom Indexing
#'
#' Generate discrete or continuous color palettes with support for custom indexing
#'
#' @param name Name of the palette. Options: "modern_premium", "academic_authority",
#'   "sophisticated_unique", "institutional_oxford", "professional_deep",
#'   "premium_steel", "categorical"
#' @param n Number of colors to generate
#' @param type Type of palette: "discrete" or "continuous"
#' @param reverse Logical, reverse the order of colors
#' @param indices Custom indices to select specific colors (e.g., c(9, 7, 5, 4))
#' @return Vector of colors
#' @examples
#' # Generate discrete palette
#' ekio_palette("modern_premium", n = 5)
#'
#' # Generate categorical colors
#' ekio_palette("categorical", n = 4)
#'
#' # Continuous palette
#' ekio_palette("academic_authority", n = 20, type = "continuous")
#'
#' # Custom indexing for non-obvious orders
#' ekio_palette("modern_premium", indices = c(9, 7, 5, 4))
#'
#' @export
ekio_palette <- function(name = "modern_premium", n = NULL, type = "discrete",
                         reverse = FALSE, indices = NULL) {

  # Get the base palette
  pal <- ekio_colors(name)

  # Handle custom indices first
  if (!is.null(indices)) {
    if (max(indices) > length(pal)) {
      stop("Index ", max(indices), " exceeds palette length (", length(pal), ")")
    }
    colors <- pal[indices]
    if (reverse) colors <- rev(colors)
    colors <- unname(colors)
    colors <- structure(colors, class = "palette", name = name, inds = indices)
    return(colors)
  }

  # Standard palette generation
  if (missing(n) | is.null(n)) {
    n <- length(pal)
  }

  if (type == "continuous") {
    # Use colorRampPalette for continuous
    pal_func <- colorRampPalette(pal)
    colors <- pal_func(n)
  } else {
    # For discrete, repeat or subset as needed
    if (n <= length(pal)) {
      colors <- pal[1:n]
    } else {
      # If more colors needed than available, interpolate
      pal_func <- colorRampPalette(pal)
      colors <- pal_func(n)
    }
  }

  if (reverse) {
    colors <- rev(colors)
  }

  colors <- unname(colors)
  colors <- structure(colors, class = "palette", name = name)

  return(colors)
}

#' @export
#' @importFrom grDevices rgb
#' @importFrom graphics image par rect text
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}
