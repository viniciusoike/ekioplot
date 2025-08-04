test_that("ekio_colors returns correct structure", {

  # Test default (all colors)
  colors_all <- ekio_colors()
  expect_type(colors_all, "character")
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors_all)))
  expect_true("midnight_blue" %in% names(colors_all))

  # Test specific palettes
  colors_cat <- ekio_colors("categorical")
  expect_length(colors_cat, 7)
  expect_equal(colors_cat[1], c("#2C6BB3"))

  # Test modern premium palette
  colors_mp <- ekio_colors("modern_premium")
  expect_length(colors_mp, 9)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors_mp)))
})

test_that("ekio_palette generates correct number of colors", {

  # Test discrete palette
  pal_5 <- ekio_palette("categorical", n = 5)
  expect_length(pal_5, 5)

  # Test continuous palette
  pal_cont <- ekio_palette("modern_premium", n = 20, type = "continuous")
  expect_length(pal_cont, 20)

  # Test reverse
  pal_rev <- ekio_palette("categorical", n = 3, reverse = TRUE)
  pal_norm <- ekio_palette("categorical", n = 3, reverse = FALSE)
  expect_equal(as.character(pal_rev), rev(pal_norm))
})
