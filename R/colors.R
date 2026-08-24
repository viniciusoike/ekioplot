# Color data lives in R/sysdata.rda, built from inst/ekio-palettes.yaml by
# data-raw/palettes.R. Do not hardcode hex codes in this file.

# ---- Internal Token Access ----

# Single brand color, e.g. .ekio("blue", 700) or .ekio("basic", "white").
# Used by theme(), recipes, and gt styling so they never carry literal hex.
#
# `group` is resolved as a scale first, then as a palette. The two never
# disagree: sequential palettes are their scale, so .ekio("blue", 700) means
# the same thing either way.
.ekio <- function(group, n) {
  if (!rlang::is_string(group)) {
    cli::cli_abort("{.arg group} must be a single scale or palette name.")
  }
  if (length(n) != 1L) {
    cli::cli_abort("{.arg n} must be a single shade, position, or name.")
  }

  if (group %in% names(.ekio_scales)) {
    .get_ekio_scale(group, n)
  } else if (!is.null(.palette_group(group))) {
    .get_ekio_palette(group, n)
  } else {
    # Local bindings: cli reads a `{.name}` interpolation as a style
    palettes <- .all_palette_names(tokens = TRUE)
    cli::cli_abort(c(
      "Unknown color scale or palette {.val {group}}.",
      "i" = "Scales: {.val {names(.ekio_scales)}}",
      "i" = "Palettes: {.val {palettes}}"
    ))
  }
}

# One color from a nine-step brand scale, by shade (100..900). Position and
# shade are aligned by construction, so 1..9 is accepted as shorthand.
# .ekio() has already checked that the scale exists.
.get_ekio_scale <- function(scale, shade) {
  s <- .ekio_scales[[scale]]
  if (is.numeric(shade) && shade %in% seq_along(s)) {
    shade <- shade * 100
  }
  # Single-bracket: [[ errors on a missing name instead of returning NA
  hex <- s[as.character(shade)]
  if (is.na(hex)) {
    cli::cli_abort(c(
      "Unknown shade {.val {as.character(shade)}} for scale {.val {scale}}.",
      "i" = "Available: {.val {names(s)}}"
    ))
  }
  unname(hex)
}

# One color from a palette, by position or — where the palette carries names,
# as `basic` and the sequential scales do — by name. .ekio() has already
# checked that the palette exists.
.get_ekio_palette <- function(palette, n) {
  pal <- .ekio_palettes[[.palette_group(palette)]][[palette]]
  # Single-bracket so an out-of-range position or unknown name gives NA
  hex <- pal[n]
  if (length(hex) != 1L || is.na(hex)) {
    cli::cli_abort(c(
      "Unknown color {.val {as.character(n)}} in palette {.val {palette}}.",
      "i" = if (is.null(names(pal))) {
        "Available: positions 1 to {length(pal)}."
      } else {
        "Available: {.val {names(pal)}}"
      }
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

# Groups that exist only as brand tokens for .ekio(). `basic` is
# white/offwhite/black: plot surfaces and inverted text, not something data
# maps onto, so ekio_pal() and list_ekio_palettes() do not offer it.
.token_groups <- "basic"

.palette_groups <- function(tokens = FALSE) {
  nms <- names(.ekio_palettes)
  if (tokens) nms else setdiff(nms, .token_groups)
}

.all_palette_names <- function(tokens = FALSE) {
  groups <- .ekio_palettes[.palette_groups(tokens)]
  unlist(lapply(groups, names), use.names = FALSE)
}

# A palette a user may ask for by name, as opposed to an internal token group
.is_user_palette <- function(palette) {
  group <- .palette_group(palette)
  !is.null(group) && !group %in% .token_groups
}

# Sequential and diverging palettes are ramps: asking for n colors should
# span the whole range rather than take the n lightest.
.continuous_groups <- c("sequential", "diverging")

# Accent palettes keep their main color first and allow a small number of
# receding grays for charts whose number of series changes.
.variable_accent_palettes <- c("accent_blue", "accent_orange")
.variable_accent_default_n <- 4L
.variable_accent_max_n <- 6L

# ---- Palette Function ----

#' Get Color Palette
#'
#' Returns colors for data visualization. Includes EKIO brand scales, accent
#' and categorical palettes, and standard scientific palettes.
#' When printed interactively, displays the palette as a colored swatch with
#' hex labels.
#'
#' Brand scales (`"blue"`, `"gray"`, `"stone"`, `"teal"`, `"green"`,
#' `"orange"`, `"red"`) are nine-step ramps running light to dark, named by
#' shade. Position and shade are aligned by construction, so
#' `ekio_pal("blue")[7]` and `ekio_pal("blue")["700"]` are the same color.
#'
#' `"gold"` is an accent rather than a scale: three colors named `"light"`,
#' `"mid"` and `"deep"`, because a nine-step gold ramp turns brown at the
#' dark end. They sit on the same lightness rungs as scale shades 300, 400
#' and 500.
#'
#' `"accent_blue"` and `"accent_orange"` put one main color before a sequence
#' of receding grays. They return four colors by default; `n` can be set from
#' 2 to 6 to match the number of series while keeping the main color first.
#'
#' @param palette Character. Name of the palette. See [list_ekio_palettes()]
#'   for all available options.
#' @param n Integer or NULL. Number of colors to return. If NULL, returns all,
#'   except accent palettes, which return four by default. For sequential and
#'   diverging palettes, `n` colors are interpolated across the full range. For
#'   accent palettes, `n` can be between 2 and 6. For other categorical and
#'   scientific palettes the first `n` colors are taken, interpolating only if
#'   `n` exceeds the palette length.
#' @param reverse Logical. If TRUE, reverses the palette order.
#'
#' @source The brand scales are generated from one OKLCH specification in
#'   `data-raw/build-ramps.R`: a shared lightness spine anchored on the brand
#'   navy, a shared chroma arc, and a hue path per family. The scientific
#'   palettes come from matplotlib (`"viridis"`, `"inferno"`, `"plasma"`) and
#'   from Okabe & Ito (`"okabe_ito"`). Notices are in `inst/COPYRIGHTS`.
#'
#' @return Object of class \code{ekio_palette} (a character vector of hex
#'   codes). Printing displays a visual swatch. Use [as.character()] to
#'   strip the class.
#' @export
#'
#' @examples
#' ekio_pal("full")
#' ekio_pal("full", n = 4)
#' ekio_pal("full", reverse = TRUE)
#' ekio_pal("accent_blue", n = 5)
#' ekio_pal("okabe_ito")
#'
#' # Brand scales are named by shade; position i is shade i * 100
#' ekio_pal("blue")["700"]
#' ekio_pal("blue")[7]
#'
#' # gold is an accent, named rather than numbered
#' ekio_pal("gold")["mid"]
ekio_pal <- function(palette = "full", n = NULL, reverse = FALSE) {
  if (!.is_user_palette(palette)) {
    available <- .all_palette_names()
    cli::cli_abort(c(
      "Palette {.val {palette}} not found.",
      "i" = "Available: {.val {available}}"
    ))
  }

  group <- .palette_group(palette)
  pal <- .ekio_palettes[[group]][[palette]]
  is_variable_accent <- palette %in% .variable_accent_palettes

  if (is_variable_accent) {
    if (is.null(n)) {
      n <- .variable_accent_default_n
    }
    if (n < 2L || n > .variable_accent_max_n) {
      cli::cli_abort(
        "{.arg n} for {.val {palette}} must be between 2 and 6."
      )
    }
    pal <- pal[seq_len(n)]
  }

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

# Split from print() so the swatch can be built and tested without opening a
# graphics device.
.palette_plot <- function(x) {
  position <- hex <- label <- text_color <- NULL

  hex_codes <- as.character(x)
  labels <- hex_codes

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

  p
}

#' @export
print.ekio_palette <- function(x, ...) {
  print(.palette_plot(x))
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
#'
#' @param type Character. Type of palettes to list:
#'   "accent", "categorical", "scientific", "sequential", "diverging", or
#'   "all" (default).
#' @return Character vector of palette names, or named list if type = "all".
#' @export
#'
#' @examples
#' list_ekio_palettes()
#' list_ekio_palettes("categorical")
list_ekio_palettes <- function(type = "all") {
  groups <- lapply(.ekio_palettes[.palette_groups()], names)

  valid_types <- c(names(groups), "all")
  if (!type %in% valid_types) {
    cli::cli_abort(c(
      "Unknown palette type {.val {type}}.",
      "i" = "Available types: {.val {valid_types}}"
    ))
  }

  result <- if (type == "all") groups else groups[[type]]

  result
}
