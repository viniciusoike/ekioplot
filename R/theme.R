# =============================================================================
# EKIO ggplot2 Theme
# =============================================================================

#' EKIO Theme for ggplot2
#'
#' A professional, minimal theme optimized for consulting deliverables.
#' Features clear typography, subtle grids, and warm gray backgrounds.
#'
#' @param base_size Numeric. Base font size (default: 11)
#' @param base_family Character. Base font family. If empty string (default),
#'   uses system default with platform-specific detection.
#' @param grid Character. Grid lines to display:
#'   \itemize{
#'     \item "y": Horizontal grid only (default) - best for bar charts
#'     \item "x": Vertical grid only
#'     \item "xy": Both grids - best for scatter plots
#'     \item "none": No grid lines - best for maps
#'   }
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic usage
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_ekio()
#'
#' # With x-y grid for scatter plots
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_ekio(grid = "xy")
#'
#' # No grid for cleaner look
#' ggplot(mtcars, aes(factor(cyl))) +
#'   geom_bar() +
#'   theme_ekio(grid = "none")
theme_ekio <- function(base_size = 11, base_family = "", grid = "y") {

  # Color definitions (from EKIO Design System v2.0)
  colors <- list(
    text_dark   = "#1A202C",
    text_mid    = "#4A5568",
    text_light  = "#718096",
    text_muted  = "#A0AEC0",
    grid        = "#E2E8F0",
    background  = "#FAFBFC",
    panel       = "#FFFFFF"
  )

  # Grid element handling
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

  # Font detection
  font_family <- if (base_family == "") {
    get_ekio_font("primary")
  } else {
    base_family
  }

  ggplot2::theme_minimal(base_size = base_size, base_family = font_family) +
    ggplot2::theme(
      # Plot titles
      plot.title = ggplot2::element_text(
        size = ggplot2::rel(1.4),
        face = "bold",
        color = colors$text_dark,
        margin = ggplot2::margin(b = 8),
        hjust = 0
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(1.05),
        color = colors$text_light,
        margin = ggplot2::margin(b = 16),
        hjust = 0
      ),
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(0.8),
        color = colors$text_muted,
        margin = ggplot2::margin(t = 12),
        hjust = 0
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",

      # Axis titles
      axis.title = ggplot2::element_text(
        size = ggplot2::rel(0.95),
        color = colors$text_mid
      ),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),

      # Axis text
      axis.text = ggplot2::element_text(
        size = ggplot2::rel(0.85),
        color = colors$text_light
      ),

      # Grid
      panel.grid.major.y = grid_y,
      panel.grid.major.x = grid_x,
      panel.grid.minor = ggplot2::element_blank(),

      # Background
      panel.background = ggplot2::element_rect(fill = colors$panel, color = NA),
      plot.background = ggplot2::element_rect(fill = colors$background, color = NA),

      # Legend
      legend.position = "top",
      legend.justification = "left",
      legend.title = ggplot2::element_text(
        size = ggplot2::rel(0.9),
        color = colors$text_mid
      ),
      legend.text = ggplot2::element_text(
        size = ggplot2::rel(0.85),
        color = colors$text_light
      ),
      legend.key = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.margin = ggplot2::margin(b = 10),

      # Facets
      strip.text = ggplot2::element_text(
        size = ggplot2::rel(1),
        face = "bold",
        color = colors$text_dark,
        hjust = 0
      ),
      strip.background = ggplot2::element_blank(),

      # Margins
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    )
}

#' EKIO Map Theme
#'
#' A variant of theme_ekio optimized for choropleth maps and spatial visualizations.
#' Removes axis elements and positions legend on the right.
#'
#' @inheritParams theme_ekio
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Example with simple polygon data
#' # ggplot(map_data, aes(fill = value)) +
#' #   geom_sf() +
#' #   scale_fill_ekio_c() +
#' #   theme_ekio_map()
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

#' EKIO Presentation Theme
#'
#' A larger variant of theme_ekio optimized for presentations and slides.
#' Increases base font size and margins for better readability on projectors.
#'
#' @inheritParams theme_ekio
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point(size = 4) +
#'   theme_ekio_presentation() +
#'   labs(title = "Vehicle Analysis")
theme_ekio_presentation <- function(base_size = 16, base_family = "", grid = "y") {
  theme_ekio(base_size = base_size, base_family = base_family, grid = grid) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.6)),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(1.1)),
      plot.margin = ggplot2::margin(30, 30, 30, 30)
    )
}
