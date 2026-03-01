# ---- Tests for Scale Functions v3.0 ----

test_that("discrete scales work with ggplot2", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  p1 <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
    geom_point() +
    scale_color_ekio_d("contrast")
  expect_s3_class(p1, "ggplot")
  expect_no_error(ggplot_build(p1))

  p2 <- ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
    geom_bar() +
    scale_fill_ekio_d("cool")
  expect_s3_class(p2, "ggplot")
  expect_no_error(ggplot_build(p2))
})

test_that("continuous scales work with ggplot2", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  p1 <- ggplot(mtcars, aes(x = wt, y = mpg, color = hp)) +
    geom_point() +
    scale_color_ekio_c("blue")
  expect_s3_class(p1, "ggplot")
  expect_no_error(ggplot_build(p1))

  p2 <- ggplot(mtcars, aes(x = wt, y = mpg, fill = hp)) +
    geom_point(shape = 21, size = 3) +
    scale_fill_ekio_c("teal")
  expect_s3_class(p2, "ggplot")
  expect_no_error(ggplot_build(p2))
})

test_that("all categorical palettes work in discrete scales", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  qual_palettes <- list_ekio_palettes("categorical")
  for (pal in qual_palettes) {
    p <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
      geom_point() +
      scale_color_ekio_d(pal)
    expect_no_error(ggplot_build(p))
  }
})

test_that("all sequential palettes work in continuous scales", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  seq_palettes <- list_ekio_palettes("sequential")
  for (pal in seq_palettes) {
    p <- ggplot(mtcars, aes(x = wt, y = mpg, color = hp)) +
      geom_point() +
      scale_color_ekio_c(pal)
    expect_no_error(ggplot_build(p))
  }
})

test_that("scale reverse parameter works", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)

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

  expect_equal(body(scale_colour_ekio_d), body(scale_color_ekio_d))
  expect_equal(body(scale_colour_ekio_c), body(scale_color_ekio_c))
})

test_that("invalid continuous palette name errors", {
  expect_error(scale_color_ekio_c("nonexistent"))
})
