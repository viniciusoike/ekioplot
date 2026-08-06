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

  resolve <- function(tok) {
    if (startsWith(tok, "#")) {
      return(toupper(tok))
    }
    parts <- strsplit(tok, ".", fixed = TRUE)[[1]]
    scales[[parts[1]]][[parts[2]]]
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

  tokens <- unlist(spec$palettes, use.names = FALSE)
  tokens <- tokens[!startsWith(tokens, "#")]

  for (tok in unique(tokens)) {
    parts <- strsplit(tok, ".", fixed = TRUE)[[1]]
    expect_length(parts, 2)
    expect_true(parts[1] %in% names(scales), info = tok)
    expect_false(is.null(scales[[parts[1]]][[parts[2]]]), info = tok)
  }
})
