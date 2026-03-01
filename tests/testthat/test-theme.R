# ---- Tests for Theme v3.0 ----

test_that("theme_ekio creates valid ggplot2 theme", {
  skip_if_not_installed("ggplot2")

  theme_default <- theme_ekio()
  expect_s3_class(theme_default, "theme")
  expect_s3_class(theme_default, "gg")

  theme_large <- theme_ekio(base_size = 14)
  expect_s3_class(theme_large, "theme")

  theme_font <- theme_ekio(base_family = "sans")
  expect_s3_class(theme_font, "theme")
})

test_that("theme_ekio grid parameter works", {
  skip_if_not_installed("ggplot2")

  expect_s3_class(theme_ekio(grid = "y"), "theme")
  expect_s3_class(theme_ekio(grid = "x"), "theme")
  expect_s3_class(theme_ekio(grid = "xy"), "theme")
  expect_s3_class(theme_ekio(grid = "none"), "theme")
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
  library(ggplot2)

  theme_map <- theme_ekio_map()
  expect_s3_class(theme_map, "theme")

  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    theme_ekio_map()
  expect_no_error(ggplot_build(p))
})

test_that("themes work with facets", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  p1 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    facet_wrap(~cyl) +
    theme_ekio()
  expect_no_error(ggplot_build(p1))
})
