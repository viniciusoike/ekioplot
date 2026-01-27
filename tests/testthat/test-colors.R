# =============================================================================
# Tests for EKIO Color System v2.0
# =============================================================================

test_that("ekio_pal returns correct structure", {

  # Test default palette
  colors_default <- ekio_pal()
  expect_type(colors_default, "character")
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors_default)))

  # Test specific palettes
  colors_contrast <- ekio_pal("contrast")
  expect_length(colors_contrast, 6)

  colors_cool <- ekio_pal("cool")
  expect_length(colors_cool, 3)

  colors_full <- ekio_pal("full")
  expect_length(colors_full, 8)

  # Test reverse
  pal_rev <- ekio_pal("contrast", reverse = TRUE)
  pal_norm <- ekio_pal("contrast", reverse = FALSE)
  expect_equal(as.character(pal_rev), rev(pal_norm))
})

test_that("ekio_seq_pal generates sequential colors", {

  # Test default (blue)
  seq_default <- ekio_seq_pal()
  expect_length(seq_default, 9)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", seq_default)))

  # Test different n values
  seq_5 <- ekio_seq_pal("blue", n = 5)
  expect_length(seq_5, 5)

  seq_11 <- ekio_seq_pal("teal", n = 11)
  expect_length(seq_11, 11)

  # Test all sequential palettes
  expect_no_error(ekio_seq_pal("gray", n = 7))
  expect_no_error(ekio_seq_pal("orange", n = 7))
})

test_that("ekio_div_pal generates diverging colors", {

  # Test default
  div_default <- ekio_div_pal()
  expect_length(div_default, 7)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", div_default)))

  # Test different n values
  div_5 <- ekio_div_pal("blue_orange", n = 5)
  expect_length(div_5, 5)

  div_11 <- ekio_div_pal("blue_red", n = 11)
  expect_length(div_11, 11)

  # Test all diverging palettes
  expect_no_error(ekio_div_pal("blue_orange", n = 9))
  expect_no_error(ekio_div_pal("blue_red", n = 9))
  expect_no_error(ekio_div_pal("teal_amber", n = 9))
})

test_that("list_ekio_palettes returns correct structure", {

  # Test all palettes
  all_palettes <- list_ekio_palettes("all")
  expect_type(all_palettes, "list")
  expect_true("qualitative" %in% names(all_palettes))
  expect_true("sequential" %in% names(all_palettes))
  expect_true("diverging" %in% names(all_palettes))

  # Test qualitative
  qual <- list_ekio_palettes("qualitative")
  expect_true("contrast" %in% qual)
  expect_true("cool" %in% qual)

  # Test sequential
  seq <- list_ekio_palettes("sequential")
  expect_true("blue" %in% seq)
  expect_true("teal" %in% seq)

  # Test diverging
  div <- list_ekio_palettes("diverging")
  expect_true("blue_orange" %in% div)
})

test_that("color scale vectors exist and are valid", {

  # Test ekio_blue
  expect_type(ekio_blue, "character")
  expect_true(length(ekio_blue) >= 9)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", ekio_blue)))
  expect_true("500" %in% names(ekio_blue))

  # Test ekio_gray
  expect_type(ekio_gray, "character")
  expect_true(length(ekio_gray) >= 9)

  # Test ekio_teal
  expect_type(ekio_teal, "character")
  expect_true(length(ekio_teal) >= 9)

  # Test ekio_orange
  expect_type(ekio_orange, "character")
  expect_true(length(ekio_orange) >= 8)

  # Test ekio_accent
  expect_type(ekio_accent, "character")
  expect_true("blue" %in% names(ekio_accent))
  expect_true("orange" %in% names(ekio_accent))
})

test_that("invalid palette names produce errors", {

  expect_error(ekio_pal("nonexistent_palette"))
  expect_error(ekio_seq_pal("nonexistent_palette"))
  expect_error(ekio_div_pal("nonexistent_palette"))
})
