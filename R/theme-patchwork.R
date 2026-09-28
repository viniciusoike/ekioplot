#' Theme for patchwork annotations
#'
#' Returns the plot-level parts of [theme_ekio()] for use with
#' `patchwork::plot_annotation(theme = )`. The theme colors the outer canvas
#' and styles titles without changing the panels or axes of child plots.
#'
#' @param background Character. Background surface accepted by [theme_ekio()].
#' @param ... Additional arguments passed to [theme_ekio()].
#' @return A partial ggplot2 theme.
#' @seealso [theme_ekio()]
#' @export
#' @examples
#' theme_patchwork(background = "offwhite")
theme_patchwork <- function(background = "offwhite", ...) {
  base <- theme_ekio(background = background, ...)

  return(
    ggplot2::theme(
      plot.background = base$plot.background,
      plot.title = base$plot.title,
      plot.subtitle = base$plot.subtitle,
      plot.caption = base$plot.caption,
      plot.margin = base$plot.margin
    )
  )
}
