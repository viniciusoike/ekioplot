# =============================================================================
# Tests for EKIO Scale Functions v2.0
# =============================================================================

test_that("discrete scales work with ggplot2", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)

  # Test discrete color scale
  p1 <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
    geom_point() +
    scale_color_ekio_d("contrast")

  expect_s3_class(p1, "ggplot")
  expect_no_error(ggplot_build(p1))

  # Test discrete fill scale
  p2 <- ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
    geom_bar() +
    scale_fill_ekio_d("cool")

  expect_s3_class(p2, "ggplot")
  expect_no_error(ggplot_build(p2))
})

test_that("continuous scales work with ggplot2", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)

  # Test continuous color scale
  p1 <- ggplot(mtcars, aes(x = wt, y = mpg, color = hp)) +
    geom_point() +
    scale_color_ekio_c("blue")

  expect_s3_class(p1, "ggplot")
  expect_no_error(ggplot_build(p1))

  # Test continuous fill scale
  p2 <- ggplot(mtcars, aes(x = wt, y = mpg, fill = hp)) +
    geom_point(shape = 21, size = 3) +
    scale_fill_ekio_c("teal")

  expect_s3_class(p2, "ggplot")
  expect_no_error(ggplot_build(p2))
})

test_that("diverging scales work with ggplot2", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)

  # Create data with positive and negative values
  df <- data.frame(
    x = 1:10,
    y = c(-5, -3, -1, 0, 1, 2, 3, 4, 5, 6)
  )

  # Test diverging color scale
  p1 <- ggplot(df, aes(x = x, y = y, color = y)) +
    geom_point(size = 3) +
    scale_color_ekio_div("blue_orange")

  expect_s3_class(p1, "ggplot")
  expect_no_error(ggplot_build(p1))

  # Test diverging fill scale
  p2 <- ggplot(df, aes(x = factor(x), y = 1, fill = y)) +
    geom_tile() +
    scale_fill_ekio_div("blue_red")

  expect_s3_class(p2, "ggplot")
  expect_no_error(ggplot_build(p2))
})

test_that("all palette options work in scales", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)

  # Test all qualitative palettes
  qual_palettes <- list_ekio_palettes("qualitative")
  for (pal in qual_palettes) {
    p <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
      geom_point() +
      scale_color_ekio_d(pal)
    expect_no_error(ggplot_build(p))
  }

  # Test all sequential palettes
  seq_palettes <- list_ekio_palettes("sequential")
  for (pal in seq_palettes) {
    p <- ggplot(mtcars, aes(x = wt, y = mpg, color = hp)) +
      geom_point() +
      scale_color_ekio_c(pal)
    expect_no_error(ggplot_build(p))
  }

  # Test all diverging palettes
  div_palettes <- list_ekio_palettes("diverging")
  for (pal in div_palettes) {
    df <- data.frame(x = 1:5, y = c(-2, -1, 0, 1, 2))
    p <- ggplot(df, aes(x = x, y = 1, fill = y)) +
      geom_tile() +
      scale_fill_ekio_div(pal)
    expect_no_error(ggplot_build(p))
  }
})

test_that("scale reverse parameter works", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)

  # Test that reverse works without error
  p1 <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
    geom_point() +
    scale_color_ekio_d("contrast", reverse = TRUE)

  expect_no_error(ggplot_build(p1))

  p2 <- ggplot(mtcars, aes(x = wt, y = mpg, color = hp)) +
    geom_point() +
    scale_color_ekio_c("blue", reverse = TRUE)

  expect_no_error(ggplot_build(p2))
})

test_that("British spelling aliases work", {
  skip_if_not_installed("ggplot2")

  # scale_colour_* should be aliases

  expect_equal(
    body(scale_colour_ekio_d),
    body(scale_color_ekio_d)
  )

  expect_equal(
    body(scale_colour_ekio_c),
    body(scale_color_ekio_c)
  )

  expect_equal(
    body(scale_colour_ekio_div),
    body(scale_color_ekio_div)
  )
})
