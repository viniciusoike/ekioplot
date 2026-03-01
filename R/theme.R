# ---- Internal Font Helper ----

.get_ekio_font <- function(type = "primary") {
  if (.Platform$OS.type == "unix" && Sys.info()["sysname"] == "Darwin") {
    return(if (type == "mono") "Monaco" else "Helvetica Neue")
  }
  if (.Platform$OS.type == "windows") {
    return(if (type == "mono") "Consolas" else "Arial")
  }
  if (type == "mono") "mono" else "sans"
}

# ---- Theme Functions ----

#' EKIO Theme for ggplot2
#'
#' A professional, minimal theme optimized for consulting deliverables.
#'
#' @param base_size Numeric. Base font size (default: 11)
#' @param base_family Character. Base font family. If empty string (default),
#'   uses system default with platform-specific detection.
#' @param grid Character. Grid lines to display:
#'   "y" (default), "x", "xy", or "none".
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @examplesIf rlang::is_interactive()
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_ekio()
theme_ekio <- function(base_size = 11, base_family = "", grid = "y") {

  colors <- list(
    text_dark  = "#1A202C",
    text_mid   = "#4A5568",
    text_light = "#718096",
    text_muted = "#A0AEC0",
    grid       = "#E2E8F0",
    background = "#FAFBFC",
    panel      = "#FFFFFF"
  )

  grid_y <- if (grid %in% c("y", "xy")) {
    ggplot2::element_line(color = colors$grid, linewidth = 0.4)
  } else {
    ggplot2::element_blank()
  }

  grid_x <- if (grid %in% c("x", "xy")) {
    ggplot2::element_line(color = colors$grid, linewidth = 0.4)
  } else {
    ggplot2::element_blank()
  }

  font_family <- if (base_family == "") .get_ekio_font("primary") else base_family

  ggplot2::theme_minimal(base_size = base_size, base_family = font_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = ggplot2::rel(1.4), face = "bold",
        color = colors$text_dark, margin = ggplot2::margin(b = 8), hjust = 0
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(1.05), color = colors$text_light,
        margin = ggplot2::margin(b = 16), hjust = 0
      ),
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(0.8), color = colors$text_muted,
        margin = ggplot2::margin(t = 12), hjust = 0
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",

      axis.title = ggplot2::element_text(
        size = ggplot2::rel(0.95), color = colors$text_mid
      ),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
      axis.text = ggplot2::element_text(
        size = ggplot2::rel(0.85), color = colors$text_light
      ),

      panel.grid.major.y = grid_y,
      panel.grid.major.x = grid_x,
      panel.grid.minor = ggplot2::element_blank(),

      panel.background = ggplot2::element_rect(fill = colors$panel, color = NA),
      plot.background = ggplot2::element_rect(fill = colors$background, color = NA),

      legend.position = "top",
      legend.justification = "left",
      legend.title = ggplot2::element_text(
        size = ggplot2::rel(0.9), color = colors$text_mid
      ),
      legend.text = ggplot2::element_text(
        size = ggplot2::rel(0.85), color = colors$text_light
      ),
      legend.key = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.margin = ggplot2::margin(b = 10),

      strip.text = ggplot2::element_text(
        size = ggplot2::rel(1), face = "bold",
        color = colors$text_dark, hjust = 0
      ),
      strip.background = ggplot2::element_blank(),

      plot.margin = ggplot2::margin(20, 20, 20, 20)
    )
}

#' EKIO Map Theme
#'
#' Variant of [theme_ekio()] optimized for maps and spatial visualizations.
#'
#' @inheritParams theme_ekio
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @examplesIf rlang::is_interactive()
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_ekio_map()
theme_ekio_map <- function(base_size = 11, base_family = "") {
  theme_ekio(base_size = base_size, base_family = base_family, grid = "none") +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      legend.position = "right",
      legend.justification = "top"
    )
}
