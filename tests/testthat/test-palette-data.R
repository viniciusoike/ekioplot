# ---- Guard against R/sysdata.rda drifting from inst/ekio-palettes.yaml ----
#
# sysdata.rda is a build artifact of data-raw/palettes.R. If the YAML is edited
# without rerunning that script, the shipped colors silently diverge from the
# source of truth. These tests re-resolve the YAML and compare.

read_spec <- function() {
  skip_if_not_installed("yaml")
  path <- system.file("ekio-palettes.yaml", package = "ekioplot")
  skip_if(identical(path, ""), "ekio-palettes.yaml not installed")
  yaml::read_yaml(path)
}

test_that("scales match the YAML source of truth", {
  spec <- read_spec()
  shades <- as.character(seq(100, 900, by = 100))

  expect_named(
    ekioplot:::.ekio_scales,
    names(spec$scales),
    ignore.order = FALSE
  )

  for (nm in names(spec$scales)) {
    from_yaml <- unlist(spec$scales[[nm]])
    names(from_yaml) <- as.character(names(spec$scales[[nm]]))
    expect_identical(
      ekioplot:::.ekio_scales[[nm]],
      from_yaml[shades],
      info = nm
    )
  }
})

test_that("palettes resolve from the YAML source of truth", {
  spec <- read_spec()
  scales <- ekioplot:::.ekio_scales

  basic <- spec$palettes$basic$basic

  resolve <- function(tok) {
    if (startsWith(tok, "#")) {
      return(toupper(tok))
    }
    parts <- strsplit(tok, ".", fixed = TRUE)[[1]]
    if (identical(parts[1], "basic")) {
      basic[[parts[2]]]
    } else {
      scales[[parts[1]]][[parts[2]]]
    }
  }

  for (group in names(spec$palettes)) {
    for (nm in names(spec$palettes[[group]])) {
      expected <- vapply(spec$palettes[[group]][[nm]], resolve, character(1),
        USE.NAMES = FALSE
      )
      expect_identical(
        unname(ekioplot:::.ekio_palettes[[group]][[nm]]),
        expected,
        info = paste(group, nm)
      )
    }
  }
})

test_that("diverging pivots are the lightest color in their palette", {
  lum <- function(x) {
    rgb <- grDevices::col2rgb(x)
    0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]
  }
  for (nm in list_ekio_palettes("diverging")) {
    pal <- as.character(ekio_pal(nm))
    expect_true(length(pal) %% 2 == 1, info = nm)
    l <- lum(pal)
    expect_identical(
      which.max(l),
      as.integer((length(pal) + 1) / 2),
      info = nm
    )
  }
})

test_that("every token in the YAML resolves to a real shade", {
  spec <- read_spec()
  scales <- ekioplot:::.ekio_scales

  basic <- spec$palettes$basic$basic

  tokens <- unlist(spec$palettes, use.names = FALSE)
  tokens <- tokens[!startsWith(tokens, "#")]

  for (tok in unique(tokens)) {
    parts <- strsplit(tok, ".", fixed = TRUE)[[1]]
    expect_length(parts, 2)
    group <- if (identical(parts[1], "basic")) basic else scales[[parts[1]]]
    expect_false(is.null(group), info = tok)
    expect_false(is.null(group[[parts[2]]]), info = tok)
  }
})

# ---- The shared spine ----
#
# Every scale is generated against one lightness spine, which is what makes
# shade number mean the same visual weight in every family. Compare against
# `blue` rather than hard-coded values: the spine is defined in
# data-raw/build-ramps.R and the invariant is that the families agree.

oklab_l <- function(x) {
  srgb <- grDevices::col2rgb(x) / 255
  lin <- ifelse(srgb <= 0.04045, srgb / 12.92, ((srgb + 0.055) / 1.055)^2.4)
  l <- 0.4122214708 * lin[1, ] + 0.5363325363 * lin[2, ] + 0.0514459929 * lin[3, ]
  m <- 0.2119034982 * lin[1, ] + 0.6806995451 * lin[2, ] + 0.1073969566 * lin[3, ]
  s <- 0.0883024619 * lin[1, ] + 0.2817188376 * lin[2, ] + 0.6299787005 * lin[3, ]
  0.2104542553 * l^(1 / 3) + 0.7936177850 * m^(1 / 3) - 0.0040720468 * s^(1 / 3)
}

test_that("every scale sits on the same lightness spine as blue", {
  scales <- ekioplot:::.ekio_scales
  spine <- oklab_l(scales$blue)

  for (nm in names(scales)) {
    expect_lt(max(abs(oklab_l(scales[[nm]]) - spine)), 0.015, label = nm)
  }
})

test_that("shade 500 clears WCAG AA on the off-white surface", {
  scales <- ekioplot:::.ekio_scales
  offwhite <- ekioplot:::.ekio("basic", "offwhite")

  for (nm in names(scales)) {
    expect_gte(ekio_contrast(scales[[nm]][["500"]], offwhite), 4.5)
  }
})
