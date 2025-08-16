#' Enhanced EKIO ggplot2 Theme with Font Management
#'
#' A complete ggplot2 theme following EKIO design principles with enhanced font support
#'
#' @param style Theme style. Options: "modern_premium", "academic_authority",
#'   "sophisticated_unique", "institutional_oxford", "professional_deep", "premium_steel"
#' @param base_size Base font size
#' @param base_family Base font family (auto-detected if not specified)
#' @param base_line_size Base size for line elements
#' @param base_rect_size Base size for rect elements
#' @param use_ekio_fonts Logical, whether to use EKIO font system
#' @return ggplot2 theme
#' @examples
#' library(ggplot2)
#'
#' # Basic usage
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   theme_ekio() +
#'   labs(title = "Brazilian Vehicle Analysis")
#'
#' # With font loading
#' \dontrun{
#' load_ekio_fonts()
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   theme_ekio("academic_authority", use_ekio_fonts = TRUE)
#' }
#'
#' @export
theme_ekio <- function(
  style = "modern_premium",
  base_size = 12,
  base_family = "",
  base_line_size = base_size / 22,
  base_rect_size = base_size / 22,
  use_ekio_fonts = FALSE
) {
  # Define style-specific colors
  style_colors <- list(
    "modern_premium" = list(primary = "#2C6BB3", grid = "#e3ecf5"),
    "academic_authority" = list(primary = "#0F3A65", grid = "#e0e6ed"),
    "sophisticated_unique" = list(primary = "#5F9EA0", grid = "#e8f4f4"),
    "institutional_oxford" = list(primary = "#1E5F9F", grid = "#e2eaf4"),
    "professional_deep" = list(primary = "#0F3A65", grid = "#e0e6ed"),
    "premium_steel" = list(primary = "#4682B4", grid = "#e1eaf2")
  )

  colors <- style_colors[[style]]
  if (is.null(colors)) {
    colors <- style_colors[["modern_premium"]]
  }

  # Enhanced font selection
  font_family <- if (base_family == "" && use_ekio_fonts) {
    # Try to get best EKIO font
    tryCatch(
      {
        get_ekio_font("primary")
      },
      error = function(e) {
        # Fallback font selection
        if (.Platform$OS.type == "unix" && Sys.info()["sysname"] == "Darwin") {
          # macOS - prefer Avenir, fallback to Helvetica Neue
          if ("Avenir" %in% names(grDevices::pdfFonts())) {
            "Avenir"
          } else if ("Helvetica Neue" %in% names(grDevices::pdfFonts())) {
            "Helvetica Neue"
          } else {
            "sans"
          }
        } else {
          # Other systems
          "sans"
        }
      }
    )
  } else if (base_family == "") {
    # Standard font detection without EKIO system
    if (.Platform$OS.type == "unix" && Sys.info()["sysname"] == "Darwin") {
      if ("Avenir" %in% names(grDevices::pdfFonts())) {
        "Avenir"
      } else if ("Helvetica Neue" %in% names(grDevices::pdfFonts())) {
        "Helvetica Neue"
      } else {
        "sans"
      }
    } else {
      "sans"
    }
  } else {
    base_family
  }

  # Build theme using EKIO specifications
  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = font_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) +
    ggplot2::theme(
      # Text elements following EKIO hierarchy
      text = ggplot2::element_text(
        family = font_family,
        color = "#2c3e50"
      ),

      # Plot title: 16px, normal weight, primary color
      plot.title = ggplot2::element_text(
        size = ggplot2::rel(1.33), # 16px relative to base_size=12
        color = "#000000",
        face = "plain",
        hjust = 0,
        margin = ggplot2::margin(b = base_size * 0.8)
      ),

      # Plot subtitle: 12px, gray
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(1), # 12px
        color = "#7f8c8d",
        hjust = 0,
        margin = ggplot2::margin(b = base_size * 1.2)
      ),

      # Caption: smaller, gray, left-aligned
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(0.8),
        color = "#7f8c8d",
        hjust = 0,
        margin = ggplot2::margin(t = base_size * 0.8)
      ),

      # Axis titles: 11px, gray
      axis.title = ggplot2::element_text(
        size = ggplot2::rel(0.92), # 11px
        color = "#7f8c8d"
      ),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = base_size * 0.6)
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = base_size * 0.6),
        angle = 90
      ),

      # Axis text: 10px, gray
      axis.text = ggplot2::element_text(
        size = ggplot2::rel(0.83), # 10px
        color = "#7f8c8d"
      ),

      # Grid lines: light gray, major only
      panel.grid.major = ggplot2::element_line(
        colour = colors$grid,
        linewidth = base_line_size * 0.8
      ),
      panel.grid.minor = ggplot2::element_blank(),

      # Panel and plot background
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),

      # Legend styling
      legend.position = "bottom",
      legend.title = ggplot2::element_text(
        size = ggplot2::rel(0.92),
        color = "#2c3e50"
      ),
      legend.text = ggplot2::element_text(
        size = ggplot2::rel(0.83),
        color = "#2c3e50"
      ),
      legend.key = ggplot2::element_rect(fill = "white", colour = NA),
      legend.margin = ggplot2::margin(t = base_size * 0.8),

      # Strip text for facets (dark blue background, white text)
      strip.text = ggplot2::element_text(
        size = ggplot2::rel(0.92),
        color = "white",
        face = "bold",
        margin = ggplot2::margin(
          base_size * 0.4,
          base_size * 0.4,
          base_size * 0.4,
          base_size * 0.4
        )
      ),
      strip.background = ggplot2::element_rect(
        fill = colors$primary,
        colour = NA
      ),

      # Panel spacing and borders
      panel.spacing = ggplot2::unit(base_size * 0.8, "pt"),

      # Plot margins following 8px grid system
      plot.margin = ggplot2::margin(
        t = base_size * 1.5, # 18px top
        r = base_size * 1.5, # 18px right
        b = base_size * 1.5, # 18px bottom
        l = base_size * 1.5 # 18px left
      ),

      # Remove axis ticks for cleaner look
      axis.ticks = ggplot2::element_blank(),

      # Ensure complete theme
      complete = TRUE
    )
}
