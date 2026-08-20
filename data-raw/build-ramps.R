# Generate the eight EKIO brand scales from one OKLCH specification.
#
# Sourcing this file defines `ekio_ramp_spec`, `ekio_build_scales()` and the
# OKLCH helpers. data-raw/palettes.R sources it and checks that
# inst/ekio-palettes.yaml still matches, so the shipped hex can never drift
# from the spec. Run this file directly to print a YAML `scales:` block.

# ---- Specification ---------------------------------------------------------

# One lightness spine for every scale. blue.700 is pinned to the brand navy
# #1E3A5F (L .346); 100-700 are evenly spaced, and 800/900 tighten because
# dark shades need finer discrimination.
#
# One chroma arc, normalized to its peak at 400-500, taken from the blue ramp.
# Each scale scales the arc by its own cmax, so shade number carries the same
# visual weight in every family.

ekio_ramp_spec <- list(
  shades = seq(100, 900, by = 100),
  spine = c(0.965, 0.862, 0.759, 0.655, 0.552, 0.449, 0.346, 0.282, 0.218),
  arc = c(0.27, 0.52, 0.73, 1.00, 1.00, 0.89, 0.71, 0.53, 0.35),
  scales = list(
    # hue: one OKLCH angle, or nine (one per shade) to let a ramp drift
    blue = list(hue = c(236, 238, 240, 244, 250, 256, 256, 255, 252), cmax = 0.104),
    gray = list(hue = 255, cmax = 0.010),
    stone = list(hue = 75, cmax = 0.012),
    teal = list(hue = c(200, 199, 198, 196, 194, 193, 194, 196, 199), cmax = 0.090),
    green = list(hue = c(155, 154, 153, 152, 151, 150, 150, 151, 152), cmax = 0.095),
    gold = list(hue = c(95, 92, 88, 84, 80, 76, 72, 70, 68), cmax = 0.125),
    orange = list(hue = c(62, 60, 57, 54, 51, 48, 45, 43, 41), cmax = 0.145),
    red = list(hue = c(30, 29, 28, 27, 26, 25, 24, 23, 22), cmax = 0.135)
  )
)

# ---- OKLCH to sRGB ---------------------------------------------------------

.oklab_to_linear_rgb <- function(L, a, b) {
  l <- (L + 0.3963377774 * a + 0.2158037573 * b)^3
  m <- (L - 0.1055613458 * a - 0.0638541728 * b)^3
  s <- (L - 0.0894841775 * a - 1.2914855480 * b)^3
  c(
    4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s,
    -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s,
    -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s
  )
}

.linear_to_srgb <- function(x) {
  ifelse(x <= 0.0031308, 12.92 * x, 1.055 * x^(1 / 2.4) - 0.055)
}

# Convert one OKLCH color to hex, holding L and hue fixed and reducing chroma
# until the color falls inside sRGB. Clipping RGB directly would shift both
# lightness and hue; the warm scales need this at their darkest steps.
oklch_hex <- function(L, C, H) {
  h <- H * pi / 180
  rgb <- NULL
  for (k in seq(1, 0, by = -0.002)) {
    rgb <- .oklab_to_linear_rgb(L, C * k * cos(h), C * k * sin(h))
    if (all(rgb > -1e-4 & rgb < 1 + 1e-4)) break
  }
  v <- round(.linear_to_srgb(pmin(pmax(rgb, 0), 1)) * 255)
  sprintf("#%02X%02X%02X", v[1], v[2], v[3])
}

# ---- Build -----------------------------------------------------------------

ekio_build_scales <- function(spec = ekio_ramp_spec) {
  shades <- as.character(spec$shades)
  n <- length(shades)

  out <- lapply(spec$scales, function(s) {
    hue <- if (length(s$hue) == 1L) rep(s$hue, n) else s$hue
    stopifnot(length(hue) == n)
    hex <- vapply(
      seq_len(n),
      function(i) oklch_hex(spec$spine[i], s$cmax * spec$arc[i], hue[i]),
      character(1)
    )
    stats::setNames(hex, shades)
  })

  out
}

# Emit a YAML `scales:` block ready to paste into inst/ekio-palettes.yaml.
ekio_scales_yaml <- function(scales = ekio_build_scales()) {
  blocks <- vapply(
    names(scales),
    function(nm) {
      rows <- sprintf('    %s: "%s"', names(scales[[nm]]), scales[[nm]])
      paste0("  ", nm, ":\n", paste(rows, collapse = "\n"))
    },
    character(1)
  )
  paste0("scales:\n", paste(blocks, collapse = "\n\n"), "\n")
}

# Print the block only when this file is the script being run, not when
# data-raw/palettes.R sources it.
.run_as_script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
if (length(.run_as_script) && basename(.run_as_script[1]) == "build-ramps.R") {
  cat(ekio_scales_yaml())
}
rm(.run_as_script)
