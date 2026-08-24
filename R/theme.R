#' Apply EKIO Theme to ggplot2 Plots
#'
#' A minimal, professional theme for EKIO visualizations built on
#' [theme_minimal()].
#'
#' @param base_size Numeric. Base font size in points (default: 11).
#' @param font_title Character. Font family passed only to the chart title.
#'   Defaults to 'Lora'.
#' @param font_text Character. Font family passed to all textual elements
#'   except the title. Defaults to 'Lato'.
#' @param title_align Argument passed to [ggplot2::theme()].
#'   Can be one of 'plot' or 'panel'.
#' @param grid Character. Which major grid lines to show: `"y"` (default),
#'   `"x"`, `"xy"`, or `"none"`. Only the requested grid themes are added.
#' @param ticks Character. Which axis ticks and lines to show: `"x"` (default),
#'   `"y"`, `"xy"`, or `"none"`. This is independent of `grid`.
#' @param background Character. Plot and panel background: `"offwhite"`
#'   (default, `#FEFEFE`), `"white"` (`#FFFFFF`), `"gray"` (the brand
#'   `gray.100`), or `"transparent"`.
#' @importFrom ggplot2 theme_minimal theme %+replace% element_blank element_line
#'   element_rect element_text margin rel theme_sub_plot theme_sub_panel
#'   theme_sub_axis theme_sub_axis_x theme_sub_axis_y theme_sub_legend
#'   theme_sub_strip
#' @param ... Additional arguments passed to [ggplot2::theme_minimal()].
#' @return A ggplot2 theme object
#' @export
#'
#' @examples
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point() +
#'   theme_ekio(font_title = "serif", font_text = "sans")
theme_ekio <- function(
  base_size = 11,
  font_title = "Lora",
  font_text = "Lato",
  title_align = "plot",
  grid = "y",
  ticks = "x",
  background = "offwhite",
  ...
) {
  grid <- match.arg(grid, c("y", "x", "xy", "none"))
  ticks <- match.arg(ticks, c("x", "y", "xy", "none"))
  background <- match.arg(
    background,
    c("offwhite", "white", "gray", "transparent")
  )
  if (missing(font_title)) {
    font_title <- getOption("ekioplot.font_title", font_title)
  }
  if (missing(font_text)) {
    font_text <- getOption("ekioplot.font_text", font_text)
  }

  # NA rather than "transparent" so element_rect() draws nothing at all
  bg <- switch(
    background,
    offwhite = .ekio("basic", "offwhite"),
    white = .ekio("basic", "white"),
    gray = .ekio("gray", 100),
    transparent = NA
  )

  colors <- list(
    text_dark = .ekio("gray", 900),
    text_mid = .ekio("gray", 700),
    text_light = .ekio("gray", 600),
    text_muted = .ekio("gray", 500),
    text_invert = .ekio("gray", 100),
    grid_line = .ekio("gray", 200),
    primary = .ekio("blue", 700)
  )

  grid_y <- if (grid %in% c("y", "xy")) {
    theme_sub_panel(
      grid.major.y = element_line(color = colors$grid_line, linewidth = 0.4)
    )
  }

  grid_x <- if (grid %in% c("x", "xy")) {
    theme_sub_panel(
      grid.major.x = element_line(color = colors$grid_line, linewidth = 0.4)
    )
  }

  grid_theme <- theme()
  if (!is.null(grid_y)) {
    grid_theme <- grid_theme + grid_y
  }
  if (!is.null(grid_x)) {
    grid_theme <- grid_theme + grid_x
  }

  axis_theme <- theme()
  if (ticks %in% c("x", "xy")) {
    axis_theme <- axis_theme +
      theme_sub_axis_x(
        ticks = element_line(color = colors$text_dark, linewidth = 0.25),
        line = element_line(color = colors$text_dark, linewidth = 0.25)
      )
  }
  if (ticks %in% c("y", "xy")) {
    axis_theme <- axis_theme +
      theme_sub_axis_y(
        ticks = element_line(color = colors$text_dark, linewidth = 0.25),
        line = element_line(color = colors$text_dark, linewidth = 0.25)
      )
  }

  # Font detection and fallback ----
  font_title <- detect_font(font_title, fallback_chain = "serif")
  font_text <- detect_font(font_text, fallback_chain = "sans")

  theme_minimal(
    base_size = base_size,
    base_family = font_text,
    paper = bg,
    ...
  ) %+replace%
    theme_sub_plot(
      background = element_rect(fill = bg, color = NA),
      title = element_text(
        family = font_title,
        size = rel(1.2),
        color = colors$text_dark,
        margin = margin(b = 4),
        hjust = 0
      ),
      title.position = title_align,
      subtitle = element_text(
        family = font_text,
        size = rel(0.9),
        color = colors$text_light,
        margin = margin(b = 8),
        hjust = 0
      ),
      caption = element_text(
        family = font_text,
        size = rel(0.7),
        color = colors$text_muted,
        margin = margin(t = 8),
        hjust = 0
      ),
      caption.position = title_align,
      margin = margin(15, 10, 15, 10)
    ) +
    theme_sub_panel(
      background = element_rect(fill = bg, color = NA),
      grid.minor = element_blank(),
      grid.major = element_blank()
    ) +
    grid_theme +
    axis_theme +
    theme_sub_axis(
      title = element_text(
        size = rel(0.9),
        color = colors$text_mid
      ),
      text = element_text(
        size = rel(0.8),
        color = colors$text_light
      )
    ) +
    theme_sub_legend(
      position = "top",
      justification = "left",
      title = element_text(
        size = rel(0.9),
        color = colors$text_mid
      ),
      text = element_text(
        size = rel(0.8),
        color = colors$text_light
      ),
      key = element_blank(),
      background = element_blank(),
      margin = margin(2, 2, 2, 2)
    ) +
    theme_sub_strip(
      text = element_text(
        size = rel(0.9),
        color = colors$text_invert,
        hjust = 0.5,
        margin = margin(2, 2, 2, 2)
      ),
      background = element_rect(fill = colors$primary, color = NA)
    )
}


#' @keywords internal
#' @noRd
detect_font <- function(font_name, fallback_chain = "sans") {
  match_font_family <- function(name, available_fonts) {
    if (name %in% available_fonts) {
      return(name)
    }
  }

  rlang::try_fetch(
    {
      # Check both registered (bundled) and system-installed fonts
      available_fonts <- unique(c(
        systemfonts::registry_fonts()$family,
        systemfonts::system_fonts()$family
      ))

      resolved <- match_font_family(font_name, available_fonts)
      if (!is.null(resolved)) {
        return(resolved)
      }

      for (fallback_font in fallback_chain) {
        if (fallback_font %in% c("serif", "sans", "mono")) {
          return(fallback_font)
        }
        resolved <- match_font_family(fallback_font, available_fonts)
        if (!is.null(resolved)) return(resolved)
      }

      fallback_chain[length(fallback_chain)]
    },
    error = function(cnd) {
      fallback_chain[length(fallback_chain)]
    }
  )
}
