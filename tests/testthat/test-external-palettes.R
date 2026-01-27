# =============================================================================
# Tests for External Palettes
# =============================================================================

test_that("external_pal returns correct colors", {

  # Test okabe_ito
  okabe <- external_pal("okabe_ito")
  expect_type(okabe, "character")
  expect_length(okabe, 8)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", okabe)))

  # Test with n specified
  okabe_5 <- external_pal("okabe_ito", n = 5)
  expect_length(okabe_5, 5)

  # Test reverse
  okabe_rev <- external_pal("okabe_ito", reverse = TRUE)
  expect_equal(as.character(okabe_rev), rev(okabe))
})

test_that("viridis palettes work", {

  # Test viridis
  viridis_cols <- external_pal("viridis", n = 10)
  expect_length(viridis_cols, 10)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", viridis_cols)))

  # Test other viridis variants
  expect_no_error(external_pal("inferno", n = 10))
  expect_no_error(external_pal("plasma", n = 10))
})

test_that("list_external_palettes works", {

  palettes <- list_external_palettes()
  expect_type(palettes, "character")
  # Returns named vector - check names
  expect_true("okabe_ito" %in% names(palettes))
  expect_true("viridis" %in% names(palettes))
})

test_that("external scale functions work with ggplot2", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)

  # Test okabe_ito scales
  p1 <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
    geom_point() +
    scale_color_okabe_ito()

  expect_no_error(ggplot_build(p1))

  p2 <- ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
    geom_bar() +
    scale_fill_okabe_ito()

  expect_no_error(ggplot_build(p2))
})

test_that("viridis scale functions work with ggplot2", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)

  # Test discrete viridis
  p1 <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
    geom_point() +
    scale_color_viridis_d()

  expect_no_error(ggplot_build(p1))

  # Test continuous viridis
  p2 <- ggplot(mtcars, aes(x = wt, y = mpg, color = hp)) +
    geom_point() +
    scale_color_viridis_c()

  expect_no_error(ggplot_build(p2))
})

test_that("invalid external palette produces error", {

  expect_error(external_pal("nonexistent_palette"))
})
