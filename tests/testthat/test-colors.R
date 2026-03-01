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

test_that("list_ekio_palettes returns correct structure", {
  all_palettes <- list_ekio_palettes("all")
  expect_type(all_palettes, "list")
  expect_true("categorical" %in% names(all_palettes))
  expect_true("small_group" %in% names(all_palettes))
  expect_true("scientific" %in% names(all_palettes))
  expect_true("sequential" %in% names(all_palettes))

  expect_true("contrast" %in% list_ekio_palettes("categorical"))
  expect_true("okabe_ito" %in% list_ekio_palettes("scientific"))
  expect_true("duo_warm" %in% list_ekio_palettes("small_group"))
  expect_true("blue" %in% list_ekio_palettes("sequential"))
})

test_that("color scale vectors exist and are valid", {
  expect_type(ekio_blue, "character")
  expect_true(length(ekio_blue) >= 9)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", ekio_blue)))
  expect_true("500" %in% names(ekio_blue))

  expect_type(ekio_gray, "character")
  expect_true(length(ekio_gray) >= 9)

  expect_type(ekio_teal, "character")
  expect_true(length(ekio_teal) >= 9)

  expect_type(ekio_orange, "character")
  expect_true(length(ekio_orange) >= 8)

  expect_type(ekio_accent, "character")
  expect_true("blue" %in% names(ekio_accent))
  expect_true("orange" %in% names(ekio_accent))
})

test_that("invalid palette names produce errors", {
  expect_error(ekio_pal("nonexistent_palette"))
})
