# ---- Font Helper ----

#' EKIO Font Family
#'
#' Returns the platform-appropriate EKIO font family. EKIO chart and table
#' output uses a sans stack only: Helvetica Neue on macOS, Arial on Windows,
#' and the generic device font elsewhere.
#'
#' This is the canonical accessor for EKIO brand type. Packages that style
#' other output (for example gt tables) should call it rather than repeating
#' the platform logic.
#'
#' @param type Character. `"primary"` for the sans stack (default) or
#'   `"mono"` for the monospace stack.
#'
#' @return Character. A font family name.
#' @export
#'
#' @examples
#' ekio_font()
#' ekio_font("mono")
ekio_font <- function(type = c("primary", "mono")) {
  type <- match.arg(type)
  if (.Platform$OS.type == "unix" && Sys.info()["sysname"] == "Darwin") {
    return(if (type == "mono") "Monaco" else "Helvetica Neue")
  }
  if (.Platform$OS.type == "windows") {
    return(if (type == "mono") "Consolas" else "Arial")
  }
  if (type == "mono") "mono" else "sans"
}


# ---- EKIO ggplot2 Theme ----

#' Apply EKIO Theme to ggplot2 Plots
#'
#' A minimal, professional theme for EKIO visualizations built on
#' [ggplot2::theme_minimal()].
#'
#' @param base_size Numeric. Base font size in points (default: 11)
#' @param base_family Character. Font family. Defaults to the platform-appropriate
#'   EKIO font via [ekio_font()].
#' @param grid Character. Which major grid lines to show: `"y"` (default),
#'   `"x"`, `"xy"`, or `"none"`.
#'
#' @return A ggplot2 theme object
#' @export
theme_ekio <- function(base_size = 11, base_family = "", grid = "y") {
  colors <- list(
    text_dark = .ekio("gray", 900),
    text_mid = .ekio("gray", 700),
    text_light = .ekio("gray", 600),
    text_muted = .ekio("gray", 500),
    grid_line = .ekio("gray", 300),
    background = .ekio("gray", 100),
    primary = .ekio("blue", 700),
    white = "#ffffff"
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
    ekio_font("primary")
  } else {
    base_family
  }

  ggplot2::theme_minimal(base_size = base_size, base_family = font_family) +
    ggplot2::theme_sub_plot(
      background = ggplot2::element_rect(fill = colors$background, color = NA),
      title = ggplot2::element_text(
        size = ggplot2::rel(1.2),
        color = colors$text_dark,
        margin = ggplot2::margin(b = 4),
        hjust = 0
      ),
      title.position = "plot",
      subtitle = ggplot2::element_text(
        size = ggplot2::rel(0.9),
        color = colors$text_light,
        margin = ggplot2::margin(b = 8),
        hjust = 0
      ),
      caption = ggplot2::element_text(
        size = ggplot2::rel(0.7),
        color = colors$text_muted,
        margin = ggplot2::margin(t = 8),
        hjust = 0
      ),
      caption.position = "plot",
      margin = ggplot2::margin(15, 10, 15, 10)
    ) +
    ggplot2::theme_sub_panel(
      background = ggplot2::element_rect(fill = colors$background, color = NA),
      grid.major.y = grid_y,
      grid.major.x = grid_x,
      grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::theme_sub_axis(
      title = ggplot2::element_text(
        size = ggplot2::rel(0.9),
        color = colors$text_mid
      ),
      text = ggplot2::element_text(
        size = ggplot2::rel(0.8),
        color = colors$text_light
      )
    ) +
    ggplot2::theme_sub_legend(
      position = "top",
      justification = "left",
      title = ggplot2::element_text(
        size = ggplot2::rel(0.9),
        color = colors$text_mid
      ),
      text = ggplot2::element_text(
        size = ggplot2::rel(0.8),
        color = colors$text_light
      ),
      key = ggplot2::element_blank(),
      background = ggplot2::element_blank(),
      margin = ggplot2::margin(0, 0, 0, 0)
    ) +
    ggplot2::theme_sub_strip(
      text = ggplot2::element_text(
        size = ggplot2::rel(0.9),
        color = colors$white,
        hjust = 0.5
      ),
      background = ggplot2::element_rect(fill = colors$primary, color = NA)
    )
}
