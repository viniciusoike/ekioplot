# ---- Font Helper ----

.get_ekio_font <- function(type = "primary") {
  if (.Platform$OS.type == "unix" && Sys.info()["sysname"] == "Darwin") {
    return(if (type == "mono") "Monaco" else "Helvetica Neue")
  }
  if (.Platform$OS.type == "windows") {
    return(if (type == "mono") "Consolas" else "Arial")
  }
  if (type == "mono") "mono" else "sans"
}


# ---- EKIO ggplot2 Theme ----

# Previous implementation using monolithic theme() call — kept for reference.
#
# theme_ekio <- function(base_size = 11, base_family = "", grid = "y") {
#   colors <- list(
#     text_dark  = "#1A202C",
#     text_mid   = "#4A5568",
#     text_light = "#718096",
#     text_muted = "#A0AEC0",
#     grid       = "#E2E8F0",
#     background = "#FAFBFC",
#     panel      = "#FAFBFC",
#     ekioblue   = ekioplot::ekio_blue["700"],
#     ekiowhite  = "#ffffff"
#   )
#
#   grid_y <- if (grid %in% c("y", "xy")) {
#     ggplot2::element_line(color = colors$grid, linewidth = 0.4)
#   } else {
#     ggplot2::element_blank()
#   }
#
#   grid_x <- if (grid %in% c("x", "xy")) {
#     ggplot2::element_line(color = colors$grid, linewidth = 0.4)
#   } else {
#     ggplot2::element_blank()
#   }
#
#   font_family <- if (base_family == "") {
#     .get_ekio_font("primary")
#   } else {
#     base_family
#   }
#
#   ggplot2::theme_minimal(base_size = base_size, base_family = font_family) +
#     ggplot2::theme(
#       plot.title              = ggplot2::element_text(size = ggplot2::rel(1.2), color = colors$text_dark,  margin = ggplot2::margin(b = 4), hjust = 0),
#       plot.subtitle           = ggplot2::element_text(size = ggplot2::rel(0.9), color = colors$text_light, margin = ggplot2::margin(b = 8), hjust = 0),
#       plot.caption            = ggplot2::element_text(size = ggplot2::rel(0.7), color = colors$text_muted, margin = ggplot2::margin(t = 8), hjust = 0),
#       plot.title.position     = "plot",
#       plot.caption.position   = "plot",
#       axis.title              = ggplot2::element_text(size = ggplot2::rel(0.9), color = colors$text_mid),
#       axis.text               = ggplot2::element_text(size = ggplot2::rel(0.8), color = colors$text_light),
#       panel.grid.major.y      = grid_y,
#       panel.grid.major.x      = grid_x,
#       panel.grid.minor        = ggplot2::element_blank(),
#       panel.background        = ggplot2::element_rect(fill = colors$panel,      color = NA),
#       plot.background         = ggplot2::element_rect(fill = colors$background, color = NA),
#       legend.position         = "top",
#       legend.justification    = "left",
#       legend.title            = ggplot2::element_text(size = ggplot2::rel(0.9), color = colors$text_mid),
#       legend.text             = ggplot2::element_text(size = ggplot2::rel(0.8), color = colors$text_light),
#       legend.key              = ggplot2::element_blank(),
#       legend.background       = ggplot2::element_blank(),
#       legend.margin           = ggplot2::margin(0, 0, 0, 0),
#       strip.text              = ggplot2::element_text(size = ggplot2::rel(0.9), color = colors$ekiowhite, hjust = 0.5),
#       strip.background        = ggplot2::element_rect(fill = colors$ekioblue, color = NA),
#       plot.margin             = ggplot2::margin(15, 10, 15, 10)
#     )
# }

#' Apply EKIO Theme to ggplot2 Plots
#'
#' A minimal, professional theme for EKIO visualizations built on
#' [ggplot2::theme_minimal()].
#'
#' @param base_size Numeric. Base font size in points (default: 11)
#' @param base_family Character. Font family. Defaults to the platform-appropriate
#'   EKIO font via \code{.get_ekio_font()}.
#' @param grid Character. Which major grid lines to show: `"y"` (default),
#'   `"x"`, `"xy"`, or `"none"`.
#'
#' @return A ggplot2 theme object
#' @export
theme_ekio <- function(base_size = 11, base_family = "", grid = "y") {
  colors <- list(
    text_dark  = ekio_gray["900"],  # #1A202C
    text_mid   = ekio_gray["700"],  # #4A5568
    text_light = ekio_gray["600"],  # #718096
    text_muted = ekio_gray["500"],  # #A0AEC0
    grid_line  = ekio_gray["300"],  # #E2E8F0
    background = ekio_gray["50"],   # #FAFBFC
    primary    = ekio_blue["700"],  # #1E3A5F
    white      = "#ffffff"
  )

  grid_y <- if (grid %in% c("y", "xy")) {
    ggplot2::element_line(color = colors$grid_line, linewidth = 0.4)
  } else {
    ggplot2::element_blank()
  }

  grid_x <- if (grid %in% c("x", "xy")) {
    ggplot2::element_line(color = colors$grid_line, linewidth = 0.4)
  } else {
    ggplot2::element_blank()
  }

  font_family <- if (base_family == "") {
    .get_ekio_font("primary")
  } else {
    base_family
  }

  ggplot2::theme_minimal(base_size = base_size, base_family = font_family) +
    ggplot2::theme_sub_plot(
      background       = ggplot2::element_rect(fill = colors$background, color = NA),
      title            = ggplot2::element_text(
        size   = ggplot2::rel(1.2),
        color  = colors$text_dark,
        margin = ggplot2::margin(b = 4),
        hjust  = 0
      ),
      title.position   = "plot",
      subtitle         = ggplot2::element_text(
        size   = ggplot2::rel(0.9),
        color  = colors$text_light,
        margin = ggplot2::margin(b = 8),
        hjust  = 0
      ),
      caption          = ggplot2::element_text(
        size   = ggplot2::rel(0.7),
        color  = colors$text_muted,
        margin = ggplot2::margin(t = 8),
        hjust  = 0
      ),
      caption.position = "plot",
      margin           = ggplot2::margin(15, 10, 15, 10)
    ) +
    ggplot2::theme_sub_panel(
      background   = ggplot2::element_rect(fill = colors$background, color = NA),
      grid.major.y = grid_y,
      grid.major.x = grid_x,
      grid.minor   = ggplot2::element_blank()
    ) +
    ggplot2::theme_sub_axis(
      title = ggplot2::element_text(size = ggplot2::rel(0.9), color = colors$text_mid),
      text  = ggplot2::element_text(size = ggplot2::rel(0.8), color = colors$text_light)
    ) +
    ggplot2::theme_sub_legend(
      position      = "top",
      justification = "left",
      title         = ggplot2::element_text(size = ggplot2::rel(0.9), color = colors$text_mid),
      text          = ggplot2::element_text(size = ggplot2::rel(0.8), color = colors$text_light),
      key           = ggplot2::element_blank(),
      background    = ggplot2::element_blank(),
      margin        = ggplot2::margin(0, 0, 0, 0)
    ) +
    ggplot2::theme_sub_strip(
      text       = ggplot2::element_text(
        size  = ggplot2::rel(0.9),
        color = colors$white,
        hjust = 0.5
      ),
      background = ggplot2::element_rect(fill = colors$primary, color = NA)
    )
}


# ---- EKIO Map Theme ----

# Previous implementation — kept for reference.
#
# theme_ekio_map <- function(base_size = 11, base_family = "") {
#   theme_ekio(base_size = base_size, base_family = base_family, grid = "none") +
#     ggplot2::theme(
#       axis.title           = ggplot2::element_blank(),
#       axis.text            = ggplot2::element_blank(),
#       axis.ticks           = ggplot2::element_blank(),
#       legend.position      = "right",
#       legend.justification = "top"
#     )
# }

#' Apply EKIO Map Theme to ggplot2 Plots
#'
#' A variant of [theme_ekio()] with axes and grid removed, suited for
#' choropleth and spatial maps.
#'
#' @inheritParams theme_ekio
#'
#' @return A ggplot2 theme object
#' @export
theme_ekio_map <- function(base_size = 11, base_family = "") {
  theme_ekio(base_size = base_size, base_family = base_family, grid = "none") +
    ggplot2::theme_sub_axis(
      title = ggplot2::element_blank(),
      text  = ggplot2::element_blank(),
      ticks = ggplot2::element_blank()
    ) +
    ggplot2::theme_sub_legend(
      position      = "right",
      justification = "top"
    )
}
