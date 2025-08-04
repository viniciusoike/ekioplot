test_that("Enhanced color system works", {

  # Test new palettes
  expect_true("hokusai1" %in% list_ekio_palettes("scientific"))
  expect_true("RdBu" %in% list_ekio_palettes("mapping"))
  expect_true("categorical_warm" %in% list_ekio_palettes("categorical"))

  # Test custom palettes
  midnight_colors <- ekio_colors("midnight_steel")
  expect_length(midnight_colors, 9)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", midnight_colors)))

  # Test color indexing
  indexed_colors <- ekio_palette("modern_premium", indices = c(9, 7, 5))
  expect_length(indexed_colors, 3)

  # Test new categorical options
  warm_colors <- ekio_colors("categorical_warm")
  expect_length(warm_colors, 7)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", warm_colors)))
})

test_that("Font system works", {
  skip_if_not_installed("extrafont")

  # Test font checking
  font_status <- check_ekio_fonts()
  expect_type(font_status, "list")
  expect_true("system_available" %in% names(font_status))

  # Test font loading (without actually installing)
  fonts <- load_ekio_fonts(install_missing = FALSE, verbose = FALSE)
  expect_type(fonts, "list")

  # Test font selection
  primary_font <- get_ekio_font("primary", fonts)
  expect_type(primary_font, "character")
  expect_length(primary_font, 1)
})

test_that("Enhanced scales work with custom indexing", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)

  # Test scale with custom indices
  p1 <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
    geom_point() +
    scale_color_ekio_d("modern_premium", indices = c(9, 6, 3))

  expect_s3_class(p1, "ggplot")
  expect_no_error(ggplot_build(p1))

  # Test new palette types
  p2 <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
    geom_point() +
    scale_color_ekio_d("categorical_warm")

  expect_s3_class(p2, "ggplot")
  expect_no_error(ggplot_build(p2))
})

test_that("Show palette function works", {
  skip_if_not_installed("ggplot2")

  # Test basic palette display
  p1 <- show_ekio_palette("modern_premium")
  expect_s3_class(p1, "ggplot")

  # Test with custom indices
  p2 <- show_ekio_palette("modern_premium", indices = c(9, 7, 5))
  expect_s3_class(p2, "ggplot")

  # Should not error when building
  expect_no_error(ggplot_build(p1))
  expect_no_error(ggplot_build(p2))
})
