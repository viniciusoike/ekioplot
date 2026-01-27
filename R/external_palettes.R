# =============================================================================
# External Color Palettes (Non-EKIO)
# =============================================================================
# These palettes are NOT part of the EKIO brand identity system.
# They are included for convenience when EKIO palettes are not appropriate
# (e.g., colorblind accessibility, scientific conventions).

#' Get External Color Palette
#'
#' Access well-known color palettes that are NOT part of the EKIO brand.
#' Use these when accessibility or convention requires a specific palette.
#'
#' @param palette Character. Name of the external palette:
#'   \itemize{
#'     \item "okabe_ito": Colorblind-friendly (8 colors) - recommended for accessibility
#'     \item "viridis": Perceptually uniform (9 colors) - good for heatmaps
#'     \item "inferno": Perceptually uniform warm (10 colors)
#'     \item "plasma": Perceptually uniform purple-yellow (10 colors)
#'   }
#' @param n Integer or NULL. Number of colors. If NULL, returns all.
#' @param reverse Logical. If TRUE, reverses the palette order.
#'
#' @return Character vector of hex color codes
#' @export
#'
#' @note These are NOT EKIO palettes. For brand-consistent visualizations,
#'   use \code{\link{ekio_pal}}, \code{\link{ekio_seq_pal}}, or
#'   \code{\link{ekio_div_pal}} instead.
#'
#' @examples
#' # Colorblind-friendly categorical palette
#' external_pal("okabe_ito")
#'
#' # Viridis for heatmaps
#' external_pal("viridis", n = 5)
external_pal <- function(palette = "okabe_ito", n = NULL, reverse = FALSE) {

  palettes <- list(
    # Okabe-Ito colorblind-friendly palette
    # Source: https://jfly.uni-koeln.de/color/
    okabe_ito = c(
      "#E69F00",
      "#56B4E9",
      "#009E73",
      "#F0E442",
      "#0072B2",
      "#D55E00",
      "#CC79A7",
      "#000000"
    ),

    # Viridis perceptually uniform palette
    viridis = c(
      "#440154",
      "#482677",
      "#3f4a8a",
      "#31678e",
      "#26838f",
      "#1f9d8a",
      "#6cce5a",
      "#b6de2b",
      "#fee825"
    ),

    # Inferno perceptually uniform (warm)
    inferno = c(
      "#000004",
      "#1b0c41",
      "#4a0c6b",
      "#781c6d",
      "#a52c60",
      "#cf4446",
      "#ed6925",
      "#fb9b06",
      "#f7d03c",
      "#fcffa4"
    ),

    # Plasma perceptually uniform
    plasma = c(
      "#0d0887",
      "#46039f",
      "#7201a8",
      "#9c179e",
      "#bd3786",
      "#d8576b",
      "#ed7953",
      "#fb9f3a",
      "#fdca26",
      "#f0f921"
    )
  )

  pal <- palettes[[palette]]
  .validate_palette(pal, palette, names(palettes), prefix = "External ")

  if (reverse) pal <- rev(pal)
  if (!is.null(n)) {
    if (n > length(pal)) {
      # Interpolate if more colors needed
      pal <- grDevices::colorRampPalette(pal)(n)
    } else {
      pal <- pal[seq_len(n)]
    }
  }

  pal
}

#' List Available External Palettes
#'
#' Returns names and descriptions of available external (non-EKIO) palettes.
#'
#' @return Named character vector with palette descriptions
#' @export
#'
#' @examples
#' list_external_palettes()
list_external_palettes <- function() {
  c(
    okabe_ito = "Colorblind-friendly categorical (8 colors)",
    viridis = "Perceptually uniform sequential (9 colors)",
    inferno = "Perceptually uniform warm sequential (10 colors)",
    plasma = "Perceptually uniform purple-yellow sequential (10 colors)"
  )
}

# ---- ggplot2 Scale Functions for External Palettes ----

#' Okabe-Ito Colorblind-Friendly Scale
#'
#' Discrete color scale using the Okabe-Ito colorblind-friendly palette.
#' Use this when accessibility is critical.
#'
#' @param reverse Logical. If TRUE, reverses the palette order.
#' @param ... Additional arguments passed to \code{ggplot2::discrete_scale}
#'
#' @return A ggplot2 scale object
#' @export
#'
#' @note This is NOT an EKIO palette. Use for accessibility requirements only.
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_okabe_ito()
scale_color_okabe_ito <- function(reverse = FALSE, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) external_pal("okabe_ito", n, reverse),
    ...
  )
}

#' @rdname scale_color_okabe_ito
#' @export
scale_colour_okabe_ito <- scale_color_okabe_ito

#' @rdname scale_color_okabe_ito
#' @export
scale_fill_okabe_ito <- function(reverse = FALSE, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) external_pal("okabe_ito", n, reverse),
    ...
  )
}

#' Viridis Color Scales
#'
#' Discrete and continuous scales using the viridis perceptually uniform palette.
#' Good choice for heatmaps and sequential data.
#'
#' @param reverse Logical. If TRUE, reverses the palette order.
#' @param ... Additional arguments passed to scale functions
#'
#' @return A ggplot2 scale object
#' @export
#'
#' @note This is NOT an EKIO palette. For brand consistency, use
#'   \code{scale_color_ekio_c("blue")} instead.
#'
#' @examples
#' library(ggplot2)
#' # Discrete
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_viridis_d()
#'
#' # Continuous
#' ggplot(mtcars, aes(wt, mpg, color = hp)) +
#'   geom_point() +
#'   scale_color_viridis_c()
scale_color_viridis_d <- function(reverse = FALSE, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) external_pal("viridis", n, reverse),
    ...
  )
}

#' @rdname scale_color_viridis_d
#' @export
scale_colour_viridis_d <- scale_color_viridis_d

#' @rdname scale_color_viridis_d
#' @export
scale_fill_viridis_d <- function(reverse = FALSE, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) external_pal("viridis", n, reverse),
    ...
  )
}

#' @rdname scale_color_viridis_d
#' @export
scale_color_viridis_c <- function(reverse = FALSE, ...) {
  ggplot2::scale_color_gradientn(
    colours = external_pal("viridis", n = 9, reverse = reverse),
    ...
  )
}

#' @rdname scale_color_viridis_d
#' @export
scale_colour_viridis_c <- scale_color_viridis_c

#' @rdname scale_color_viridis_d
#' @export
scale_fill_viridis_c <- function(reverse = FALSE, ...) {
  ggplot2::scale_fill_gradientn(
    colours = external_pal("viridis", n = 9, reverse = reverse),
    ...
  )
}
