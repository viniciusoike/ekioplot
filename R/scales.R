# ---- Discrete Scales ----

#' Discrete Color Scale
#'
#' Apply qualitative palettes to discrete/categorical data.
#'
#' @param palette Character. Palette name (default: "full").
#'   See [ekio_pal()] for options.
#' @param reverse Logical. If TRUE, reverses the palette order.
#' @param ... Additional arguments passed to [ggplot2::discrete_scale()]
#'
#' @return A ggplot2 scale object
#' @export
#'
#' @seealso [ekio_pal()], [scale_fill_ekio_d()]
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point(size = 3) +
#'   scale_color_ekio_d()
scale_color_ekio_d <- function(palette = "full", reverse = FALSE, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) as.character(ekio_pal(palette, n, reverse)),
    ...
  )
}

#' @rdname scale_color_ekio_d
#' @export
scale_colour_ekio_d <- scale_color_ekio_d

#' @rdname scale_color_ekio_d
#' @export
scale_fill_ekio_d <- function(palette = "full", reverse = FALSE, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) as.character(ekio_pal(palette, n, reverse)),
    ...
  )
}

# ---- Continuous Scales ----

#' Continuous Color Scale
#'
#' Apply sequential, diverging or academy palettes to continuous/numeric data.
#'
#' @param palette Character. Palette name (default: "blue").
#'   See `list_ekio_palettes("sequential")`, `list_ekio_palettes("diverging")`
#'   and `list_ekio_palettes("academy")` for options.
#' @param reverse Logical. If TRUE, reverses the color order.
#' @param ... Additional arguments passed to [ggplot2::scale_color_gradientn()]
#'
#' @return A ggplot2 scale object
#' @export
#'
#' @seealso [scale_fill_ekio_c()], [list_ekio_palettes()]
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, color = hp)) +
#'   geom_point(size = 3) +
#'   scale_color_ekio_c()
#'
#' ggplot(mtcars, aes(wt, mpg, color = hp)) +
#'   geom_point(size = 3) +
#'   scale_color_ekio_c("stone")
scale_color_ekio_c <- function(palette = "blue", reverse = FALSE, ...) {
  pal <- .continuous_palette(palette)
  if (reverse) {
    pal <- rev(pal)
  }
  ggplot2::scale_color_gradientn(colours = pal, ...)
}

#' @rdname scale_color_ekio_c
#' @export
scale_colour_ekio_c <- scale_color_ekio_c

#' @rdname scale_color_ekio_c
#' @export
scale_fill_ekio_c <- function(palette = "blue", reverse = FALSE, ...) {
  pal <- .continuous_palette(palette)
  if (reverse) {
    pal <- rev(pal)
  }
  ggplot2::scale_fill_gradientn(colours = pal, ...)
}

# ---- Internal Helpers ----

.continuous_palette <- function(palette) {
  seq_pals <- .ekio_palettes$sequential
  div_pals <- .ekio_palettes$diverging
  aca_pals <- .ekio_palettes$academy
  pal <- seq_pals[[palette]] %||% div_pals[[palette]] %||% aca_pals[[palette]]
  if (is.null(pal)) {
    available <- c(names(seq_pals), names(div_pals), names(aca_pals))
    cli::cli_abort(c(
      "Continuous palette {.val {palette}} not found.",
      "i" = "Available: {.val {available}}"
    ))
  }
  unname(pal)
}
