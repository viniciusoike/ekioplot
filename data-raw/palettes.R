# Build R/sysdata.rda from inst/ekio-palettes.yaml.
#
# inst/ekio-palettes.yaml is the single source of truth for EKIO brand color.
# Edit that file, then run this script. tests/testthat/test-palette-data.R
# guards against the built object drifting from the YAML.

library(yaml)

source("data-raw/build-ramps.R")

yaml_path <- "inst/ekio-palettes.yaml"
spec <- yaml::read_yaml(yaml_path)

shades <- as.character(seq(100, 900, by = 100))

# ---- Scales ----

.ekio_scales <- lapply(spec$scales, function(s) {
  v <- unlist(s)
  names(v) <- as.character(names(s))
  v[shades]
})

# ---- Validation ----

stopifnot(
  "every scale must have exactly 9 shades" = all(lengths(.ekio_scales) == 9),
  "every scale must be named 100..900" = all(vapply(
    .ekio_scales,
    function(x) identical(names(x), shades),
    logical(1)
  )),
  "every scale value must be a 6-digit hex code" = all(grepl(
    "^#[0-9A-Fa-f]{6}$",
    unlist(.ekio_scales)
  ))
)

# The scales and accent tokens are generated from the OKLCH spec in
# data-raw/build-ramps.R. Hand-editing the YAML would silently break the
# shared spine, so check the whole block rather than trusting it.
accents <- ekio_build_accents()
for (nm in names(accents)) {
  from_yaml <- unlist(spec$palettes$accent[[nm]])
  if (!identical(from_yaml[names(accents[[nm]])], accents[[nm]])) {
    stop(
      "YAML accent '", nm, "' does not match data-raw/build-ramps.R.",
      call. = FALSE
    )
  }
}

generated <- ekio_build_scales()
if (!identical(.ekio_scales, generated)) {
  drift <- names(generated)[!vapply(
    names(generated),
    function(nm) identical(.ekio_scales[[nm]], generated[[nm]]),
    logical(1)
  )]
  stop(
    "YAML scales do not match data-raw/build-ramps.R: ",
    paste(union(drift, setdiff(names(.ekio_scales), names(generated))),
      collapse = ", "
    ),
    ". Edit the spec and paste the regenerated block.",
    call. = FALSE
  )
}

# ---- Color math ----

oklab_l <- function(x) {
  srgb <- grDevices::col2rgb(x) / 255
  lin <- ifelse(srgb <= 0.04045, srgb / 12.92, ((srgb + 0.055) / 1.055)^2.4)
  l <- 0.4122214708 * lin[1, ] + 0.5363325363 * lin[2, ] + 0.0514459929 * lin[3, ]
  m <- 0.2119034982 * lin[1, ] + 0.6806995451 * lin[2, ] + 0.1073969566 * lin[3, ]
  s <- 0.0883024619 * lin[1, ] + 0.2817188376 * lin[2, ] + 0.6299787005 * lin[3, ]
  0.2104542553 * l^(1 / 3) + 0.7936177850 * m^(1 / 3) - 0.0040720468 * s^(1 / 3)
}

relative_luminance <- function(x) {
  srgb <- grDevices::col2rgb(x) / 255
  lin <- ifelse(srgb <= 0.03928, srgb / 12.92, ((srgb + 0.055) / 1.055)^2.4)
  0.2126 * lin[1, ] + 0.7152 * lin[2, ] + 0.0722 * lin[3, ]
}

contrast_ratio <- function(a, b) {
  la <- relative_luminance(a)
  lb <- relative_luminance(b)
  (pmax(la, lb) + 0.05) / (pmin(la, lb) + 0.05)
}

# Naive luminance, kept for the ramp and pivot ordering checks below.
lum <- function(x) {
  rgb <- grDevices::col2rgb(x)
  as.numeric(0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ])
}

# ---- Scale invariants ----

# Ramps must run light to dark without reversing.
for (nm in names(.ekio_scales)) {
  if (any(diff(lum(.ekio_scales[[nm]])) >= 0)) {
    stop("scale '", nm, "' is not monotonically darkening", call. = FALSE)
  }
}

# Every scale sits on the shared lightness spine, which is what makes shade
# number mean the same visual weight in every family.
for (nm in names(.ekio_scales)) {
  off <- abs(oklab_l(.ekio_scales[[nm]]) - ekio_ramp_spec$spine)
  if (any(off > 0.015)) {
    stop(
      "scale '", nm, "' departs from the lightness spine at shade ",
      shades[which.max(off)], " (off by ", round(max(off), 3), ")",
      call. = FALSE
    )
  }
}

# Shade 500 is the text-safe tier: it must clear WCAG AA on the off-white
# surface, because theme_ekio() uses gray.500 for muted text.
offwhite <- spec$palettes$basic$basic$offwhite
for (nm in names(.ekio_scales)) {
  cr <- contrast_ratio(.ekio_scales[[nm]][["500"]], offwhite)
  if (cr < 4.5) {
    stop(
      "scale '", nm, "' shade 500 gives only ", round(cr, 2),
      ":1 against the off-white surface (AA needs 4.5:1)",
      call. = FALSE
    )
  }
}

# gold has no scale, so it has no 500 to carry the text-safe promise. Its
# `deep` token does that job instead.
gold_deep <- accents$gold[["deep"]]
if (contrast_ratio(gold_deep, offwhite) < 4.5) {
  stop(
    "gold.deep gives only ", round(contrast_ratio(gold_deep, offwhite), 2),
    ":1 against the off-white surface (AA needs 4.5:1)",
    call. = FALSE
  )
}

# ---- Token resolution ----

# A palette written as a YAML mapping rather than a sequence is a named token
# group - `basic`, and the accent tokens. Members of other palettes can point
# at one the same way they point at a scale shade, so `gold.light` and
# `basic.pivot` resolve by the same rule as `blue.700`.
token_groups <- list()
for (group in names(spec$palettes)) {
  for (nm in names(spec$palettes[[group]])) {
    pal <- spec$palettes[[group]][[nm]]
    if (!is.null(names(pal))) {
      token_groups[[nm]] <- unlist(pal)
    }
  }
}

resolve <- function(x) {
  vapply(
    x,
    function(tok) {
      if (startsWith(tok, "#")) {
        return(toupper(tok))
      }
      parts <- strsplit(tok, ".", fixed = TRUE)[[1]]
      if (length(parts) != 2) {
        stop("unknown token '", tok, "'", call. = FALSE)
      }
      # Scales win: a sequential palette is its scale, so the two agree
      group <- if (parts[1] %in% names(.ekio_scales)) {
        .ekio_scales[[parts[1]]]
      } else {
        token_groups[[parts[1]]]
      }
      if (is.null(group)) {
        stop("unknown scale or token group in '", tok, "'", call. = FALSE)
      }
      hex <- group[[parts[2]]]
      if (is.null(hex)) {
        stop("unknown member in token '", tok, "'", call. = FALSE)
      }
      hex
    },
    character(1),
    USE.NAMES = FALSE
  )
}

.ekio_palettes <- lapply(names(spec$palettes), function(group) {
  lapply(spec$palettes[[group]], function(pal) {
    out <- resolve(pal)
    # Sequential palettes are the scales themselves, so they keep shade names.
    # Other palettes are ordered sets where position carries no meaning, but a
    # YAML mapping (rather than a list) opts a palette into named lookup.
    if (group == "sequential") {
      names(out) <- shades
    } else if (!is.null(names(pal))) {
      names(out) <- names(pal)
    }
    out
  })
})
names(.ekio_palettes) <- names(spec$palettes)

# ---- Palette invariants ----

# Sequential palettes must match their scale exactly.
for (nm in names(.ekio_palettes$sequential)) {
  stopifnot(identical(
    unname(.ekio_palettes$sequential[[nm]]),
    unname(.ekio_scales[[nm]])
  ))
}

# A diverging palette's neutral pivot must be its lightest color, so the
# visual center of the scale lands on the data's zero rather than one slot
# off it.
for (nm in names(.ekio_palettes$diverging)) {
  pal <- .ekio_palettes$diverging[[nm]]
  mid <- (length(pal) + 1) / 2
  if (mid %% 1 != 0) {
    stop("diverging palette '", nm, "' must have an odd length", call. = FALSE)
  }
  l <- lum(pal)
  if (l[mid] != max(l)) {
    stop(
      "diverging palette '",
      nm,
      "': pivot is not the lightest color ",
      "(pivot ",
      round(l[mid]),
      ", lightest ",
      round(max(l)),
      " at position ",
      which.max(l),
      ")",
      call. = FALSE
    )
  }
}

usethis::use_data(
  .ekio_scales,
  .ekio_palettes,
  internal = TRUE,
  overwrite = TRUE
)
