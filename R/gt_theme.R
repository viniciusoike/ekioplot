# ---- GT Table Theme ----

#' Apply EKIO Theme to GT Tables
#'
#' Professional EKIO branding and styling for gt table objects.
#'
#' @param data A gt table object
#' @param table_width Character. Width of the table (default: "100%")
#' @param font_size Numeric. Base font size in pixels (default: 14)
#' @param stripe Logical. Apply alternating row striping (default: TRUE)
#' @param add_footer Logical. Add automatic EKIO footer (default: TRUE)
#'
#' @return A styled gt table object
#' @export
#'
#' @examples
#' \dontrun{
#' library(gt)
#' head(mtcars, 10) |>
#'   gt() |>
#'   gt_theme_ekio()
#' }
gt_theme_ekio <- function(
  data,
  table_width = "100%",
  font_size = 14,
  stripe = TRUE,
  add_footer = TRUE
) {
  if (!inherits(data, "gt_tbl")) {
    cli::cli_abort("{.arg data} must be a gt table object")
  }

  # Reference package palettes directly to stay in sync with any future changes
  colors <- list(
    primary      = ekio_blue["700"],   # #1E3A5F — headers, accents
    primary_dark = ekio_blue["800"],   # #1B3A4B — grand summary background
    primary_light = ekio_blue["100"],  # #D4E8F5 — summary row tint
    row_group_bg = ekio_blue["50"],    # #EEF5FA — row group label background
    text         = ekio_gray["900"],   # #1A202C
    text_mid     = ekio_gray["700"],   # #4A5568
    text_light   = ekio_gray["600"],   # #718096
    border       = ekio_gray["300"],   # #E2E8F0
    stripe_bg    = ekio_gray["100"],   # #F7FAFC
    light_bg     = ekio_gray["50"]     # #FAFBFC
  )

  font_family <- .get_ekio_font("primary")

  styled_table <- data |>
    gt::opt_table_font(font = font_family) |>
    gt::tab_options(
      table.width = table_width,
      table.font.size = gt::px(font_size),
      table.font.color = colors$text,
      table.font.weight = "normal",
      table.background.color = colors$light_bg,

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
      column_labels.font.size = gt::px(font_size - 1),
      column_labels.font.weight = "600",
      column_labels.padding = gt::px(10),
      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(2),
      column_labels.border.bottom.color = colors$border,

      row_group.background.color = colors$row_group_bg,
      row_group.font.weight = "600",
      row_group.padding = gt::px(6),
      row_group.border.top.style = "solid",
      row_group.border.top.width = gt::px(1),
      row_group.border.top.color = colors$border,
      row_group.border.bottom.style = "solid",
      row_group.border.bottom.width = gt::px(1),
      row_group.border.bottom.color = colors$border,

      stub.background.color = colors$stripe_bg,
      stub.font.weight = "600",
      stub.border.style = "solid",
      stub.border.width = gt::px(1),
      stub.border.color = colors$border,

      data_row.padding = gt::px(8),
      row.striping.include_table_body = stripe,
      row.striping.background_color = colors$stripe_bg,

      summary_row.background.color = colors$primary_light,
      summary_row.padding = gt::px(8),
      summary_row.border.style = "solid",
      summary_row.border.width = gt::px(1),
      summary_row.border.color = colors$border,

      grand_summary_row.background.color = colors$primary_dark,
      grand_summary_row.padding = gt::px(8),
      grand_summary_row.border.style = "solid",
      grand_summary_row.border.width = gt::px(2),
      grand_summary_row.border.color = colors$primary,

      table.border.top.style = "solid",
      table.border.top.width = gt::px(2),
      table.border.top.color = colors$primary,
      table.border.bottom.style = "solid",
      table.border.bottom.width = gt::px(3),
      table.border.bottom.color = colors$primary,
      table.border.left.style = "none",
      table.border.right.style = "none",

      source_notes.font.size = gt::px(font_size - 3),
      source_notes.border.lr.style = "none",
      source_notes.padding = gt::px(10),
      source_notes.background.color = colors$light_bg,

      footnotes.font.size = gt::px(font_size - 3),
      footnotes.padding = gt::px(8),
      footnotes.background.color = colors$light_bg
    ) |>
    # Column labels: white text on primary blue
    gt::tab_style(
      style = list(
        gt::cell_text(color = "white", weight = "600"),
        gt::cell_fill(color = colors$primary)
      ),
      locations = gt::cells_column_labels()
    ) |>
    # Spanner labels: same treatment as column labels
    gt::tab_style(
      style = list(
        gt::cell_text(weight = "700", color = "white"),
        gt::cell_fill(color = colors$primary)
      ),
      locations = gt::cells_column_spanners()
    ) |>
    # Subtle bottom border on every body row
    gt::tab_style(
      style = gt::cell_borders(
        sides = "bottom", color = colors$border, weight = gt::px(1)
      ),
      locations = gt::cells_body()
    ) |>
    # Table title: primary blue, bold, left-aligned
    gt::tab_style(
      style = gt::cell_text(
        color = colors$primary, weight = "600", align = "left"
      ),
      locations = gt::cells_title(groups = "title")
    ) |>
    # Subtitle: muted, smaller, left-aligned
    gt::tab_style(
      style = gt::cell_text(
        color = colors$text_light, weight = "normal",
        size = gt::px(font_size), align = "left"
      ),
      locations = gt::cells_title(groups = "subtitle")
    ) |>
    # Stub: mid-tone text
    gt::tab_style(
      style = gt::cell_text(color = colors$text_mid, weight = "600"),
      locations = gt::cells_stub()
    ) |>
    # Row group labels: primary blue
    gt::tab_style(
      style = gt::cell_text(color = colors$primary, weight = "600"),
      locations = gt::cells_row_groups()
    ) |>
    # Summary rows: primary blue text on light blue background
    gt::tab_style(
      style = gt::cell_text(color = colors$primary, weight = "600"),
      locations = gt::cells_summary()
    ) |>
    # Grand summary: white text on dark blue background
    gt::tab_style(
      style = gt::cell_text(color = "white", weight = "700"),
      locations = gt::cells_grand_summary()
    ) |>
    # Source notes and footnotes: muted text
    gt::tab_style(
      style = gt::cell_text(color = colors$text_light),
      locations = gt::cells_source_notes()
    ) |>
    gt::tab_style(
      style = gt::cell_text(color = colors$text_light),
      locations = gt::cells_footnotes()
    )

  if (add_footer) {
    styled_table <- styled_table |>
      gt::tab_source_note(source_note = "EKIO")
  }

  styled_table
}
