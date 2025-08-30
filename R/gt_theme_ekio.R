ekio_modern_premium_table <- list(
  font_family = "Avenir",
  primary_color = "#2C6BB3",
  text_color = "#2c3e50",
  title_color = "#2C6BB3",
  caption_color = "#7f8c8d",
  border_color = "#e3ecf5",
  stripe_bg = "#f8f9fa",
  light_bg = "#f4f7fb"
)

ekio_academic_authority_table <- list(
  font_family = "Lato",
  primary_color = "#0F3A65",
  text_color = "#2c3e50",
  title_color = "#0F3A65",
  caption_color = "#7f8c8d",
  border_color = "#e0e6ed",
  stripe_bg = "#f8f9fa",
  light_bg = "#f2f4f7"
)

ekio_sophisticated_unique_table <- list(
  font_family = "Avenir",
  primary_color = "#5F9EA0",
  text_color = "#2c3e50",
  title_color = "#5F9EA0",
  caption_color = "#7f8c8d",
  border_color = "#e8f4f4",
  stripe_bg = "#f8f9fa",
  light_bg = "#f6fafa"
)

ekio_institutional_oxford_table <- list(
  font_family = "Helvetica Neue",
  primary_color = "#1E5F9F",
  text_color = "#2c3e50",
  title_color = "#1E5F9F",
  caption_color = "#7f8c8d",
  border_color = "#e2eaf4",
  stripe_bg = "#f8f9fa",
  light_bg = "#f3f6fa"
)

ekio_professional_deep_table <- list(
  font_family = "Helvetica Neue",
  primary_color = "#0F3A65",
  text_color = "#2c3e50",
  title_color = "#0F3A65",
  caption_color = "#7f8c8d",
  border_color = "#e0e6ed",
  stripe_bg = "#f8f9fa",
  light_bg = "#f2f4f7"
)

ekio_premium_steel_table <- list(
  font_family = "Avenir",
  primary_color = "#4682B4",
  text_color = "#2c3e50",
  title_color = "#4682B4",
  caption_color = "#7f8c8d",
  border_color = "#e1eaf2",
  stripe_bg = "#f8f9fa",
  light_bg = "#f4f7fa"
)

#' Apply EKIO Theme to GT Tables
#'
#' Applies professional EKIO branding and styling to gt table objects with various
#' predefined themes optimized for different presentation contexts.
#'
#' @param data A gt table object
#' @param style Character. Theme style to apply. Options: "modern_premium",
#'   "academic_authority", "sophisticated_unique", "institutional_oxford",
#'   "professional_deep", "premium_steel"
#' @param table_width Character. Width of the table (default: "100%")
#' @param font_size Numeric. Base font size in pixels (default: 14)
#' @param add_footer Logical. Whether to add automatic EKIO footer (default: TRUE)
#'
#' @return A styled gt table object
#' @export
#'
#' @examples
#' \dontrun{
#' library(gt)
#' library(dplyr)
#'
#' mtcars %>%
#'   head(10) %>%
#'   gt() %>%
#'   gt_theme_ekio(style = "modern_premium")
#' }
gt_theme_ekio <- function(
  data,
  style = "modern_premium",
  table_width = "100%",
  font_size = 14,
  add_footer = TRUE
) {
  if (!inherits(data, "gt_tbl")) {
    stop("Input must be a gt table object")
  }

  theme_config <- switch(
    style,
    "modern_premium" = ekio_modern_premium_table,
    "academic_authority" = ekio_academic_authority_table,
    "sophisticated_unique" = ekio_sophisticated_unique_table,
    "institutional_oxford" = ekio_institutional_oxford_table,
    "professional_deep" = ekio_professional_deep_table,
    "premium_steel" = ekio_premium_steel_table,
    ekio_modern_premium_table
  )

  styled_table <- data |>
    gt::opt_table_font(font = theme_config$font_family) |>
    gt::tab_options(
      table.width = table_width,
      table.font.size = gt::px(font_size),
      table.font.color = theme_config$text_color,
      table.font.weight = "normal",

      heading.background.color = "white",
      heading.title.font.size = gt::px(font_size + 6),
      heading.title.font.weight = "600",
      heading.subtitle.font.size = gt::px(font_size),
      heading.subtitle.font.weight = "normal",
      heading.padding = gt::px(8),
      heading.border.bottom.style = "solid",
      heading.border.bottom.width = gt::px(3),
      heading.border.bottom.color = theme_config$primary_color,

      column_labels.background.color = theme_config$primary_color,
      column_labels.font.color = "white",
      column_labels.font.size = gt::px(font_size - 1),
      column_labels.font.weight = "600",
      column_labels.padding = gt::px(10),
      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(2),
      column_labels.border.bottom.color = theme_config$border_color,

      data_row.padding = gt::px(8),
      row.striping.include_table_body = TRUE,
      row.striping.background_color = theme_config$stripe_bg,

      table.border.top.style = "solid",
      table.border.top.width = gt::px(2),
      table.border.top.color = theme_config$primary_color,
      table.border.bottom.style = "solid",
      table.border.bottom.width = gt::px(3),
      table.border.bottom.color = theme_config$primary_color,
      table.border.left.style = "none",
      table.border.right.style = "none",

      source_notes.font.size = gt::px(font_size - 3),
      source_notes.font.color = theme_config$caption_color,
      source_notes.border.lr.style = "none",
      source_notes.padding = gt::px(10),
      source_notes.background.color = theme_config$light_bg,

      footnotes.font.size = gt::px(font_size - 3),
      footnotes.font.color = theme_config$caption_color,
      footnotes.padding = gt::px(8),
      footnotes.background.color = theme_config$light_bg
    ) |>

    gt::tab_style(
      style = list(
        gt::cell_text(color = "white", weight = "600"),
        gt::cell_fill(color = theme_config$primary_color)
      ),
      locations = gt::cells_column_labels()
    ) |>

    gt::tab_style(
      style = list(
        gt::cell_text(weight = "700", color = "white"),
        gt::cell_fill(color = theme_config$primary_color)
      ),
      locations = gt::cells_column_spanners()
    ) |>

    gt::tab_style(
      style = gt::cell_borders(
        sides = c("bottom"),
        color = theme_config$border_color,
        weight = gt::px(1)
      ),
      locations = gt::cells_body()
    ) |>

    gt::tab_style(
      style = list(
        gt::cell_text(
          color = theme_config$caption_color,
          size = gt::px(font_size - 3),
          style = "italic"
        ),
        gt::cell_fill(color = theme_config$light_bg)
      ),
      locations = gt::cells_source_notes()
    ) |>

    gt::tab_style(
      style = gt::cell_text(
        color = theme_config$title_color,
        weight = "600",
        align = "left"
      ),
      locations = gt::cells_title()
    ) |>

    gt::tab_style(
      style = gt::cell_text(
        color = theme_config$caption_color,
        weight = "normal",
        size = gt::px(font_size),
        align = "left"
      ),
      locations = gt::cells_title(groups = "subtitle")
    )

  if (add_footer) {
    styled_table <- styled_table |>
      gt::tab_source_note(
        source_note = "EKIO"
      )
  }

  return(styled_table)
}

#' Simple EKIO Table Theme
#'
#' Quick styling function for common table use cases with minimal footer.
#'
#' @param data A gt table object
#' @param style Character. Theme style (default: "modern_premium")
#'
#' @return A styled gt table object
#' @export
gt_ekio_simple <- function(data, style = "modern_premium") {
  gt_theme_ekio(data, style = style, font_size = 12, add_footer = FALSE)
}

#' Executive Presentation Table Theme
#'
#' Professional styling optimized for executive presentations and reports.
#'
#' @param data A gt table object
#' @param style Character. Theme style (default: "premium_steel")
#'
#' @return A styled gt table object
#' @export
gt_ekio_executive <- function(data, style = "premium_steel") {
  gt_theme_ekio(data, style = style, font_size = 16, add_footer = TRUE)
}

#' Government Report Table Theme
#'
#' Academic styling suitable for government reports and official documentation.
#'
#' @param data A gt table object
#' @param style Character. Theme style (default: "academic_authority")
#'
#' @return A styled gt table object
#' @export
gt_ekio_government <- function(data, style = "academic_authority") {
  gt_theme_ekio(data, style = style, font_size = 13, add_footer = TRUE)
}
