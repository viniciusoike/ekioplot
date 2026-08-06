# ---- Tests for Color System v3.0 ----

test_that("ekio_pal returns correct structure", {
  colors_default <- ekio_pal()
  expect_type(colors_default, "character")
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors_default)))

  expect_length(ekio_pal("contrast"), 6)
  expect_length(ekio_pal("cool"), 3)
  expect_length(ekio_pal("full"), 8)
  expect_length(ekio_pal("binary"), 2)

  pal_rev <- ekio_pal("contrast", reverse = TRUE)
  pal_norm <- ekio_pal("contrast")
  expect_equal(as.character(pal_rev), rev(pal_norm))
})

test_that("ekio_pal n parameter works", {
  expect_length(ekio_pal("contrast", n = 4), 4)
  expect_length(ekio_pal("contrast", n = 2), 2)

  # Interpolation when n > palette length
  interp <- ekio_pal("binary", n = 5)
  expect_length(interp, 5)
})

test_that("small group variants work", {
  expect_length(ekio_pal("duo_warm"), 2)
  expect_length(ekio_pal("duo_cool"), 2)
  expect_length(ekio_pal("trio_bold"), 3)
  expect_length(ekio_pal("trio_cool"), 3)
  expect_length(ekio_pal("quad_earth"), 4)
  expect_length(ekio_pal("quad_vivid"), 4)
})

test_that("scientific palettes are accessible via ekio_pal", {
  expect_length(ekio_pal("okabe_ito"), 8)
  expect_length(ekio_pal("viridis"), 9)
  expect_length(ekio_pal("inferno"), 10)
  expect_length(ekio_pal("plasma"), 10)

  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", ekio_pal("okabe_ito"))))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", ekio_pal("viridis"))))
})

test_that("sequential palettes are accessible via ekio_pal", {
  seq_names <- list_ekio_palettes("sequential")
  for (pal in seq_names) {
    colors <- ekio_pal(pal)
    expect_type(colors, "character")
    expect_true(length(colors) >= 9)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors)))
  }
})

test_that("diverging palettes are accessible via ekio_pal", {
  div_names <- list_ekio_palettes("diverging")
  expect_true(length(div_names) >= 3)
  for (pal in div_names) {
    colors <- ekio_pal(pal)
    expect_type(colors, "character")
    expect_true(length(colors) >= 11)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors)))
  }
})

test_that("list_ekio_palettes returns correct structure", {
  all_palettes <- list_ekio_palettes("all")
  expect_type(all_palettes, "list")
  expect_true("categorical" %in% names(all_palettes))
  expect_true("small_group" %in% names(all_palettes))
  expect_true("scientific" %in% names(all_palettes))
  expect_true("sequential" %in% names(all_palettes))
  expect_true("diverging" %in% names(all_palettes))

  expect_true("contrast" %in% list_ekio_palettes("categorical"))
  expect_true("okabe_ito" %in% list_ekio_palettes("scientific"))
  expect_true("duo_warm" %in% list_ekio_palettes("small_group"))
  expect_true("blue" %in% list_ekio_palettes("sequential"))
  expect_true("purple" %in% list_ekio_palettes("sequential"))
  expect_true("blue_orange" %in% list_ekio_palettes("diverging"))
})

test_that("list_ekio_palettes verbose prints summary and returns invisibly", {
  out <- cli::cli_fmt(res <- withVisible(list_ekio_palettes(verbose = TRUE)))
  expect_false(res$visible)
  expect_identical(res$value, list_ekio_palettes())
  expect_true(any(grepl("Available Palettes", out)))
  expect_true(any(grepl("Diverging", out)))
})

test_that("list_ekio_palettes verbose respects the type filter", {
  out <- cli::cli_fmt(res <- list_ekio_palettes("categorical", verbose = TRUE))
  expect_identical(res, list_ekio_palettes("categorical"))
  expect_true(any(grepl("Categorical", out)))
  expect_false(any(grepl("Diverging|Sequential|Scientific", out)))
})

test_that("show_ekio_palette warns about deprecation but still works", {
  expect_warning(
    show_ekio_palette("contrast"),
    "deprecated"
  )
})

test_that("ekio_pal returns ekio_palette class that auto-prints", {
  p <- ekio_pal("contrast")
  expect_s3_class(p, "ekio_palette")
  expect_type(p, "character")
  expect_length(p, 6)

  stripped <- as.character(p)
  expect_type(stripped, "character")
  expect_null(attr(stripped, "class"))
})

test_that("show_all_ekio_palettes warns about deprecation but still works", {
  expect_warning(
    res <- suppressMessages(show_all_ekio_palettes()),
    "deprecated"
  )
  expect_identical(res, list_ekio_palettes())
})

test_that("retired color vectors are gone", {
  for (nm in c(
    "ekio_blue", "ekio_gray", "ekio_teal", "ekio_orange", "ekio_accent"
  )) {
    expect_false(nm %in% getNamespaceExports("ekioplot"))
  }
})

test_that("brand scales are nine steps named 100..900", {
  shades <- as.character(seq(100, 900, by = 100))
  for (nm in list_ekio_palettes("sequential")) {
    pal <- ekio_pal(nm)
    expect_length(pal, 9)
    expect_named(pal, shades)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", as.character(pal))))
  }
})

test_that("position and shade are aligned in brand scales", {
  for (nm in list_ekio_palettes("sequential")) {
    pal <- ekio_pal(nm)
    for (i in seq_len(9)) {
      expect_identical(
        unname(pal[i]),
        unname(pal[as.character(i * 100)]),
        info = paste0(nm, " position ", i)
      )
    }
  }
})

test_that("brand scales darken monotonically", {
  lum <- function(x) {
    rgb <- grDevices::col2rgb(x)
    0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]
  }
  for (nm in list_ekio_palettes("sequential")) {
    expect_true(
      all(diff(lum(as.character(ekio_pal(nm)))) < 0),
      info = nm
    )
  }
})

test_that("n interpolates across ramps but truncates categorical palettes", {
  # Sequential: n = 3 spans the full range rather than taking the 3 lightest
  three <- as.character(ekio_pal("blue", n = 3))
  expect_length(three, 3)
  expect_identical(three[1], unname(as.character(ekio_pal("blue"))[1]))
  expect_identical(three[3], unname(as.character(ekio_pal("blue"))[9]))

  # Categorical: n = 3 takes the first 3, which are ordered by preference
  expect_identical(
    as.character(ekio_pal("contrast", n = 3)),
    as.character(ekio_pal("contrast"))[1:3]
  )
})

test_that("invalid palette names produce errors", {
  expect_error(ekio_pal("nonexistent_palette"))
})
