# ---- Internal Luminance Helper ----

.relative_luminance <- function(colors) {
  rgb <- grDevices::col2rgb(colors) / 255
  rgb <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  as.numeric(c(0.2126, 0.7152, 0.0722) %*% rgb)
}

# ---- Contrast Ratio ----

#' WCAG Contrast Ratio Between Two Colors
#'
#' Computes the contrast ratio between foreground and background colors as
#' defined by WCAG 2.1. Ratios range from 1 (no contrast) to 21
#' (black on white). WCAG requires at least 4.5 for normal text (level AA),
#' 3.0 for large text (AA), and 7.0 for normal text at level AAA.
#'
#' @param color Character. Foreground color(s) as hex codes or R color names.
#' @param background Character. Background color(s) (default: `"white"`).
#'   Recycled against `color` if needed.
#'
#' @return Numeric vector of contrast ratios between 1 and 21
#' @export
#'
#' @seealso [ekio_text_on()] to pick a readable text color for a background
#'
#' @examples
#' ekio_contrast("black", "white")
#' ekio_contrast(ekio_blue["700"])
#' ekio_contrast("white", ekio_blue)
ekio_contrast <- function(color, background = "white") {
  if (!is.character(color) || !is.character(background)) {
    cli::cli_abort(
      "{.arg color} and {.arg background} must be character vectors of colors."
    )
  }
  lum_fg <- .relative_luminance(color)
  lum_bg <- .relative_luminance(background)
  unname((pmax(lum_fg, lum_bg) + 0.05) / (pmin(lum_fg, lum_bg) + 0.05))
}

# ---- Text Color Picker ----

#' Pick a Readable Text Color for a Background
#'
#' Returns the text color (dark or light) with the higher WCAG contrast
#' ratio against each background color. Useful for labels placed on colored
#' fills, e.g. in [ggplot2::geom_text()] or gt table cells.
#'
#' @param background Character. Background color(s) as hex codes or R color
#'   names.
#' @param dark Character. Dark text color candidate (default: `"black"`).
#' @param light Character. Light text color candidate (default: `"white"`).
#'
#' @return Character vector of text colors, one per background. Names of
#'   `background` are preserved.
#' @export
#'
#' @seealso [ekio_contrast()] for the underlying contrast ratios
#'
#' @examples
#' ekio_text_on(ekio_blue["700"])
#' ekio_text_on(ekio_blue)
#' ekio_text_on(ekio_accent, dark = ekio_gray["900"])
ekio_text_on <- function(background, dark = "black", light = "white") {
  contrast_dark <- ekio_contrast(dark, background)
  contrast_light <- ekio_contrast(light, background)
  out <- ifelse(contrast_dark >= contrast_light, dark, light)
  names(out) <- names(background)
  out
}
