# =============================================================================
# Tests for EKIO Theme v2.0
# =============================================================================

test_that("theme_ekio creates valid ggplot2 theme", {
  skip_if_not_installed("ggplot2")

  # Test default theme
  theme_default <- theme_ekio()
  expect_s3_class(theme_default, "theme")
  expect_s3_class(theme_default, "gg")

  # Test with custom base_size
  theme_large <- theme_ekio(base_size = 14)
  expect_s3_class(theme_large, "theme")

  # Test with base_family
  theme_font <- theme_ekio(base_family = "sans")
  expect_s3_class(theme_font, "theme")
})

test_that("theme_ekio grid parameter works", {
  skip_if_not_installed("ggplot2")

  # Test all grid options
  theme_y <- theme_ekio(grid = "y")
  expect_s3_class(theme_y, "theme")

  theme_x <- theme_ekio(grid = "x")
  expect_s3_class(theme_x, "theme")

  theme_xy <- theme_ekio(grid = "xy")
  expect_s3_class(theme_xy, "theme")

  theme_none <- theme_ekio(grid = "none")
  expect_s3_class(theme_none, "theme")
})

test_that("theme_ekio works in complete plot", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)

  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    theme_ekio() +
    labs(title = "Test Plot", subtitle = "Subtitle")

  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot_build(p))
})

test_that("theme_ekio_map works", {
  skip_if_not_installed("ggplot2")

  # Test map theme
  theme_map <- theme_ekio_map()
  expect_s3_class(theme_map, "theme")
  expect_s3_class(theme_map, "gg")

  # Test with custom size
  theme_map_lg <- theme_ekio_map(base_size = 14)
  expect_s3_class(theme_map_lg, "theme")

  # Use in a plot
  library(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    theme_ekio_map()

  expect_no_error(ggplot_build(p))
})

test_that("theme_ekio_presentation works", {
  skip_if_not_installed("ggplot2")

  # Test presentation theme
  theme_pres <- theme_ekio_presentation()
  expect_s3_class(theme_pres, "theme")
  expect_s3_class(theme_pres, "gg")

  # Test with grid parameter
  theme_pres_xy <- theme_ekio_presentation(grid = "xy")
  expect_s3_class(theme_pres_xy, "theme")

  # Use in a plot
  library(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    theme_ekio_presentation() +
    labs(title = "Presentation Title")

  expect_no_error(ggplot_build(p))
})

test_that("themes work with facets", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)

  # Test with facet_wrap
  p1 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    facet_wrap(~cyl) +
    theme_ekio()

  expect_no_error(ggplot_build(p1))

  # Test with facet_grid
  p2 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    facet_grid(vs ~ am) +
    theme_ekio()

  expect_no_error(ggplot_build(p2))
})
