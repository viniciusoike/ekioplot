test_that("EKIO scales work with ggplot2", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)

  # Test discrete color scale
  p1 <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
    geom_point() +
    scale_color_ekio_d("categorical")

  expect_s3_class(p1, "ggplot")

  # Test continuous fill scale
  p2 <- ggplot(mtcars, aes(x = wt, y = mpg, fill = hp)) +
    geom_point(shape = 21) +
    scale_fill_ekio_c("modern_premium")

  expect_s3_class(p2, "ggplot")

  # Test that scales can be built
  expect_no_error(ggplot_build(p1))
  expect_no_error(ggplot_build(p2))
})
