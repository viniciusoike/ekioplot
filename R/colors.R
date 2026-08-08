# Color data lives in R/sysdata.rda, built from inst/ekio-palettes.yaml by
# data-raw/palettes.R. Do not hardcode hex codes in this file.

# ---- Internal Token Access ----

# Single brand color by scale and shade, e.g. .ekio("blue", 700).
# Used by theme(), recipes, and gt styling so they never carry literal hex.
.ekio <- function(scale, shade) {
  s <- .ekio_scales[[scale]]
  if (is.null(s)) {
    cli::cli_abort(c(
      "Unknown color scale {.val {scale}}.",
      "i" = "Available: {.val {names(.ekio_scales)}}"
    ))
  }
  # Single-bracket: [[ errors on a missing name instead of returning NULL
  hex <- s[as.character(shade)]
  if (is.na(hex)) {
    cli::cli_abort(c(
      "Unknown shade {.val {as.character(shade)}} for scale {.val {scale}}.",
      "i" = "Available: {.val {names(s)}}"
    ))
  }
  unname(hex)
}

# ---- Internal Palette Lookup ----

.palette_group <- function(palette) {
  for (group in names(.ekio_palettes)) {
    if (palette %in% names(.ekio_palettes[[group]])) {
      return(group)
    }
  }
  NULL
}

.all_palette_names <- function() {
  unlist(lapply(.ekio_palettes, names), use.names = FALSE)
}

# Sequential and diverging palettes are ramps: asking for n colors should
# span the whole range rather than take the n lightest.
.continuous_groups <- c("sequential", "diverging")

# ---- Palette Function ----

#' Get Color Palette
#'
#' Returns colors for data visualization. Includes EKIO brand scales, curated
#' categorical and small-group palettes, and standard scientific palettes.
#' When printed interactively, displays the palette as a colored swatch with
#' hex labels.
#'
#' Brand scales (`"blue"`, `"gray"`, `"teal"`, `"orange"`, `"purple"`,
#' `"red"`, `"green"`, `"amber"`) are nine-step ramps running light to dark,
#' named by shade. Position and shade are aligned by construction, so
#' `ekio_pal("blue")[7]` and `ekio_pal("blue")["700"]` are the same color.
#'
#' @param palette Character. Name of the palette. See [list_ekio_palettes()]
#'   for all available options.
#' @param n Integer or NULL. Number of colors to return. If NULL, returns all.
#'   For sequential and diverging palettes, `n` colors are interpolated across
#'   the full range. For categorical, small-group, and scientific palettes the
#'   first `n` colors are taken, interpolating only if `n` exceeds the palette
#'   length.
#' @param reverse Logical. If TRUE, reverses the palette order.
#'
#' @return Object of class \code{ekio_palette} (a character vector of hex
#'   codes). Printing displays a visual swatch. Use [as.character()] to
#'   strip the class.
#' @export
#'
#' @examples
#' ekio_pal("contrast")
#' ekio_pal("contrast", n = 4)
#' ekio_pal("binary", reverse = TRUE)
#' ekio_pal("okabe_ito")
#'
#' # Brand scales are named by shade; position i is shade i * 100
#' ekio_pal("blue")["700"]
#' ekio_pal("blue")[7]
ekio_pal <- function(palette = "contrast", n = NULL, reverse = FALSE) {
  group <- .palette_group(palette)
  if (is.null(group)) {
    cli::cli_abort(c(
      "Palette {.val {palette}} not found.",
      "i" = "Available: {.val {.all_palette_names()}}"
    ))
  }

  pal <- .ekio_palettes[[group]][[palette]]

  if (reverse) {
    pal <- rev(pal)
  }

  # n matching the palette length is a no-op, so shade names survive it
  if (!is.null(n) && n != length(pal)) {
    if (group %in% .continuous_groups || n > length(pal)) {
      pal <- grDevices::colorRampPalette(unname(pal))(n)
    } else {
      pal <- pal[seq_len(n)]
    }
  }

  structure(pal, class = c("ekio_palette", "character"), palette = palette)
}

#' @export
print.ekio_palette <- function(x, ...) {
  position <- hex <- label <- text_color <- NULL

  hex_codes <- as.character(x)
  labels <- if (is.null(names(x))) hex_codes else names(x)

  df <- data.frame(
    position = seq_along(hex_codes),
    hex = hex_codes,
    label = labels,
    text_color = unname(ekio_text_on(hex_codes, dark = .ekio("gray", 900))),
    stringsAsFactors = FALSE
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = position, y = 1, fill = hex)) +
    ggplot2::geom_tile(
      width = 0.9,
      height = 1,
      color = "white",
      linewidth = 1
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_text(
      ggplot2::aes(label = label, color = text_color),
      size = 4,
      fontface = "bold",
      angle = 90
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::theme_void() +
    ggplot2::labs(title = paste0("Palette: ", attr(x, "palette"))) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = 13,
        face = "bold",
        margin = ggplot2::margin(b = 10)
      ),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )

  print(p)
  invisible(x)
}

#' @export
as.character.ekio_palette <- function(x, ...) {
  x <- unclass(x)
  attributes(x) <- NULL
  x
}

# ---- Palette Listing ----

#' List Available Palettes
#'
#' Returns names of all available palettes, optionally filtered by type.
#' When \code{verbose = TRUE}, prints a formatted summary to the console.
#'
#' @param type Character. Type of palettes to list:
#'   "categorical", "small_group", "scientific", "sequential", "diverging",
#'   or "all" (default).
#' @param verbose Logical. If TRUE, prints a formatted summary of the
#'   selected type(s) and returns the result invisibly (default: FALSE).
#'
#' @return Character vector of palette names, or named list if type = "all".
#'   Invisibly returned when \code{verbose = TRUE}.
#' @export
#'
#' @examples
#' list_ekio_palettes()
#' list_ekio_palettes("categorical")
#' list_ekio_palettes("diverging")
#' list_ekio_palettes(verbose = TRUE)
list_ekio_palettes <- function(type = "all", verbose = FALSE) {
  groups <- lapply(.ekio_palettes, names)

  valid_types <- c(names(groups), "all")
  if (!type %in% valid_types) {
    cli::cli_abort(c(
      "Unknown palette type {.val {type}}.",
      "i" = "Available types: {.val {valid_types}}"
    ))
  }

  result <- if (type == "all") groups else groups[[type]]

  if (verbose) {
    headers <- c(
      categorical = "Categorical",
      small_group = "Small Group Variants",
      scientific = "Scientific",
      sequential = "Sequential (brand scales, for continuous fills)",
      diverging = "Diverging (for continuous scales)"
    )
    shown <- if (type == "all") names(groups) else type

    cli::cli_h1("Available Palettes")
    for (nm in shown) {
      cli::cli_h2(headers[[nm]] %||% nm)
      cli::cli_text("{.val {groups[[nm]]}}")
    }
    cli::cli_text("")
    cli::cli_alert_info("Print {.fun ekio_pal} to see the palette swatch")

    return(invisible(result))
  }

  result
}

# ---- Deprecated ----

#' Show All Palettes
#'
#' @description
#' Deprecated. Use [list_ekio_palettes()] with `verbose = TRUE` instead.
#'
#' @return The palette list, invisibly (as returned by
#'   [list_ekio_palettes()] with `verbose = TRUE`).
#' @keywords internal
#' @export
#'
#' @examples
#' list_ekio_palettes(verbose = TRUE)
show_all_ekio_palettes <- function() {
  cli::cli_warn(
    c(
      "{.fn show_all_ekio_palettes} was deprecated in ekioplot 0.5.1.",
      "i" = "Use {.code list_ekio_palettes(verbose = TRUE)} instead."
    ),
    .frequency = "once",
    .frequency_id = "show_all_ekio_palettes"
  )
  list_ekio_palettes(verbose = TRUE)
}
