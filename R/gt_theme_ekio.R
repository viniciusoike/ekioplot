# =============================================================================
# EKIO GT Table Themes (Simplified)
# =============================================================================

#' Apply EKIO Theme to GT Tables
#'
#' Applies professional EKIO branding and styling to gt table objects.
#' Uses a single consistent color scheme matching the ggplot2 theme.
#'
#' @param data A gt table object
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
#' mtcars |>
#'   head(10) |>
#'   gt() |>
#'   gt_theme_ekio()
#' }
gt_theme_ekio <- function(
  data,
  table_width = "100%",
  font_size = 14,
  add_footer = TRUE
) {
  if (!inherits(data, "gt_tbl")) {
    cli::cli_abort("{.arg data} must be a gt table object")
  }

  # EKIO color scheme (matching theme_ekio)
  colors <- list(
    primary = "#1E3A5F",
    text = "#1A202C",
    text_light = "#718096",
    border = "#E2E8F0",
    stripe_bg = "#F7FAFC",
    light_bg = "#FAFBFC"
  )

  # Get font
  font_family <- get_ekio_font("primary")

  styled_table <- data |>
    gt::opt_table_font(font = font_family) |>
    gt::tab_options(
      table.width = table_width,
      table.font.size = gt::px(font_size),
      table.font.color = colors$text,
      table.font.weight = "normal",

      heading.background.color = "white",
      heading.title.font.size = gt::px(font_size + 6),
      heading.title.font.weight = "600",
      heading.subtitle.font.size = gt::px(font_size),
      heading.subtitle.font.weight = "normal",
      heading.padding = gt::px(8),
      heading.border.bottom.style = "solid",
      heading.border.bottom.width = gt::px(3),
      heading.border.bottom.color = colors$primary,

      column_labels.background.color = colors$primary,
      column_labels.font.color = "white",
      column_labels.font.size = gt::px(font_size - 1),
      column_labels.font.weight = "600",
      column_labels.padding = gt::px(10),
      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(2),
      column_labels.border.bottom.color = colors$border,

      data_row.padding = gt::px(8),
      row.striping.include_table_body = TRUE,
      row.striping.background_color = colors$stripe_bg,

      table.border.top.style = "solid",
      table.border.top.width = gt::px(2),
      table.border.top.color = colors$primary,
      table.border.bottom.style = "solid",
      table.border.bottom.width = gt::px(3),
      table.border.bottom.color = colors$primary,
      table.border.left.style = "none",
      table.border.right.style = "none",

      source_notes.font.size = gt::px(font_size - 3),
      source_notes.font.color = colors$text_light,
      source_notes.border.lr.style = "none",
      source_notes.padding = gt::px(10),
      source_notes.background.color = colors$light_bg,

      footnotes.font.size = gt::px(font_size - 3),
      footnotes.font.color = colors$text_light,
      footnotes.padding = gt::px(8),
      footnotes.background.color = colors$light_bg
    ) |>

    gt::tab_style(
      style = list(
        gt::cell_text(color = "white", weight = "600"),
        gt::cell_fill(color = colors$primary)
      ),
      locations = gt::cells_column_labels()
    ) |>

    gt::tab_style(
      style = list(
        gt::cell_text(weight = "700", color = "white"),
        gt::cell_fill(color = colors$primary)
      ),
      locations = gt::cells_column_spanners()
    ) |>

    gt::tab_style(
      style = gt::cell_borders(
        sides = "bottom",
        color = colors$border,
        weight = gt::px(1)
      ),
      locations = gt::cells_body()
    ) |>

    gt::tab_style(
      style = gt::cell_text(
        color = colors$primary,
        weight = "600",
        align = "left"
      ),
      locations = gt::cells_title()
    ) |>

    gt::tab_style(
      style = gt::cell_text(
        color = colors$text_light,
        weight = "normal",
        size = gt::px(font_size),
        align = "left"
      ),
      locations = gt::cells_title(groups = "subtitle")
    )

  if (add_footer) {
    styled_table <- styled_table |>
      gt::tab_source_note(source_note = "EKIO")
  }

  styled_table
}

#' Simple EKIO Table Theme
#'
#' Quick styling function for common table use cases with minimal footer.
#'
#' @param data A gt table object
#'
#' @return A styled gt table object
#' @export
#'
#' @examples
#' \dontrun{
#' mtcars |> head(5) |> gt() |> gt_ekio_simple()
#' }
gt_ekio_simple <- function(data) {
  gt_theme_ekio(data, font_size = 12, add_footer = FALSE)
}
