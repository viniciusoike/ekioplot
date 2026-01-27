# =============================================================================
# EKIO ggplot2 Scale Functions
# =============================================================================

# ---- Discrete Scales ----

#' Discrete Color Scale for EKIO Palettes
#'
#' Apply EKIO qualitative palettes to discrete/categorical data.
#' Use this for bar charts, scatter plots with groups, etc.
#'
#' @param palette Character. Name of the qualitative palette.
#'   Options: "cool", "minimal", "contrast" (default), "full", "muted",
#'   "binary", "political". See \code{\link{ekio_pal}} for details.
#' @param reverse Logical. If TRUE, reverses the palette order.
#' @param ... Additional arguments passed to \code{ggplot2::discrete_scale}
#'
#' @return A ggplot2 scale object
#' @export
#'
#' @seealso \code{\link{ekio_pal}} for available palettes,
#'   \code{\link{scale_fill_ekio_d}} for fill aesthetic
#'
#' @examples
#' library(ggplot2)
#'
#' # Default contrast palette
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point(size = 3) +
#'   scale_color_ekio_d()
#'
#' # Cool palette for 3 groups
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point(size = 3) +
#'   scale_color_ekio_d("cool")
#'
#' # Binary palette for two-group comparison
#' ggplot(mtcars, aes(wt, mpg, color = factor(am))) +
#'   geom_point(size = 3) +
#'   scale_color_ekio_d("binary")
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

# ---- Continuous Scales ----

#' Continuous Color Scale for EKIO Sequential Palettes
#'
#' Apply EKIO sequential palettes to continuous/ordered data.
#' Use this for heatmaps, choropleths, scatter plots with numeric color.
#'
#' @param palette Character. Name of the sequential palette.
#'   Options: "blue" (default), "teal", "gray", "orange".
#'   See \code{\link{ekio_seq_pal}} for details.
#' @param reverse Logical. If TRUE, reverses light-to-dark order.
#' @param ... Additional arguments passed to \code{ggplot2::scale_color_gradientn}
#'
#' @return A ggplot2 scale object
#' @export
#'
#' @seealso \code{\link{ekio_seq_pal}} for available palettes,
#'   \code{\link{scale_fill_ekio_c}} for fill aesthetic
#'
#' @examples
#' library(ggplot2)
#'
#' # Blue sequential for continuous variable
#' ggplot(mtcars, aes(wt, mpg, color = hp)) +
#'   geom_point(size = 3) +
#'   scale_color_ekio_c()
#'
#' # Teal for alternative look
#' ggplot(mtcars, aes(wt, mpg, color = hp)) +
#'   geom_point(size = 3) +
#'   scale_color_ekio_c("teal")
#'
#' # Orange with reversed direction (dark to light)
#' ggplot(mtcars, aes(wt, mpg, color = hp)) +
#'   geom_point(size = 3) +
#'   scale_color_ekio_c("orange", reverse = TRUE)
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

# ---- Diverging Scales ----

#' Diverging Color Scale for EKIO
#'
#' Apply EKIO diverging palettes to data with meaningful center point.
#' Use this for deviation from average, positive/negative, etc.
#'
#' @param palette Character. Name of the diverging palette.
#'   Options: "blue_orange" (default), "blue_red", "teal_amber".
#'   See \code{\link{ekio_div_pal}} for details.
#' @param reverse Logical. If TRUE, reverses the palette direction.
#' @param ... Additional arguments passed to \code{ggplot2::scale_color_gradientn}
#'
#' @return A ggplot2 scale object
#' @export
#'
#' @seealso \code{\link{ekio_div_pal}} for available palettes,
#'   \code{\link{scale_fill_ekio_div}} for fill aesthetic
#'
#' @examples
#' library(ggplot2)
#'
#' # Create data with positive/negative values
#' df <- data.frame(
#'   x = 1:10,
#'   y = rnorm(10),
#'   z = rnorm(10)
#' )
#'
#' # Blue-orange diverging
#' ggplot(df, aes(x, y, color = z)) +
#'   geom_point(size = 4) +
#'   scale_color_ekio_div()
#'
#' # Blue-red for political/sentiment data
#' ggplot(df, aes(x, y, color = z)) +
#'   geom_point(size = 4) +
#'   scale_color_ekio_div("blue_red")
scale_color_ekio_div <- function(palette = "blue_orange", reverse = FALSE, ...) {
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
scale_fill_ekio_div <- function(palette = "blue_orange", reverse = FALSE, ...) {
  colors <- ekio_div_pal(palette, n = 7, reverse = reverse)
  ggplot2::scale_fill_gradientn(
    colours = colors,
    values = scales::rescale(c(-1, -0.5, -0.1, 0, 0.1, 0.5, 1)),
    ...
  )
}
