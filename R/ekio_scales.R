#' Enhanced EKIO Discrete Color Scale with Custom Indexing
#'
#' ggplot2 discrete color scale with support for custom color indexing
#'
#' @param palette Name of EKIO palette to use
#' @param reverse Logical, reverse the palette order
#' @param indices Custom indices for specific colors (e.g., c(9, 7, 5))
#' @param ... Additional arguments passed to discrete_scale
#' @return ggplot2 scale
#' @examples
#' library(ggplot2)
#'
#' # Standard usage
#' ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_ekio_d("categorical")
#'
#' # Custom indexing for specific colors
#' ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_ekio_d("modern_premium", indices = c(9, 6, 3))
#'
#' @export
# scale_color_ekio_d <- function(palette = "categorical", reverse = FALSE, indices = NULL, ...) {
#   ekio_pal <- function(n) {
#     if (!is.null(indices)) {
#       ekio_palette(palette, indices = indices, reverse = reverse)
#     } else {
#       ekio_palette(palette, n, "discrete", reverse)
#     }
#   }
#   ggplot2::discrete_scale(aesthetics = "colour", palette = ekio_pal(n = n), ...)
# }


#' @export
scale_color_ekio_d <- function(palette = "categorical", reverse = FALSE, indices = NULL, ...) {

  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = \(n) ekio_palette(name = palette, reverse = reverse, n = n, indices = indices),
    ...
  )

}

#' @rdname scale_color_ekio_d
#' @export
scale_colour_ekio_d <- scale_color_ekio_d

#' Enhanced EKIO Discrete Fill Scale with Custom Indexing
#'
#' ggplot2 discrete fill scale with support for custom color indexing
#'
#' @inheritParams scale_color_ekio_d
#' @return ggplot2 scale
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
#'   geom_bar() +
#'   scale_fill_ekio_d("categorical_cool", indices = c(7, 4, 1))
#'
#' @export
scale_fill_ekio_d <- function(palette = "categorical", reverse = FALSE, indices = NULL, ...) {

    ggplot2::discrete_scale(
      aesthetics = "fill",
      palette = \(n) ekio_palette(name = palette, reverse = reverse, n = n, indices = indices)
    )
}

#' EKIO Continuous Color Scale
#'
#' ggplot2 continuous color scale using EKIO palettes
#'
#' @param palette Name of EKIO palette to use
#' @param reverse Logical, reverse the palette order
#' @param ... Additional arguments passed to scale_color_gradientn
#' @return ggplot2 scale
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg, color = hp)) +
#'   geom_point() +
#'   scale_color_ekio_c("modern_premium")
#'
#' @export
scale_color_ekio_c <- function(palette = "modern_premium", reverse = FALSE, ...) {
  colors <- ekio_palette(palette, type = "continuous", reverse = reverse)
  ggplot2::scale_color_gradientn(colours = colors, ...)
}

#' @rdname scale_color_ekio_c
#' @export
scale_colour_ekio_c <- scale_color_ekio_c

#' EKIO Continuous Fill Scale
#'
#' ggplot2 continuous fill scale using EKIO palettes
#'
#' @inheritParams scale_color_ekio_c
#' @return ggplot2 scale
#' @examples
#' library(ggplot2)
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_tile() +
#'   scale_fill_ekio_c("academic_authority")
#'
#' @export
scale_fill_ekio_c <- function(palette = "modern_premium", reverse = FALSE, ...) {
  colors <- ekio_palette(palette, type = "continuous", reverse = reverse)
  ggplot2::scale_fill_gradientn(colours = colors, ...)
}
