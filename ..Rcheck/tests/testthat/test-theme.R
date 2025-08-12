test_that("theme_ekio creates valid ggplot2 theme", {
  skip_if_not_installed("ggplot2")

  # Test default theme
  theme_default <- theme_ekio()
  expect_s3_class(theme_default, "theme")
  expect_s3_class(theme_default, "gg")

  # Test different styles
  theme_academic <- theme_ekio("academic_authority")
  expect_s3_class(theme_academic, "theme")

  # Test with custom base_size
  theme_large <- theme_ekio(base_size = 14)
  expect_s3_class(theme_large, "theme")
})

test_that("theme_ekio works in complete plot", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)

  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    theme_ekio() +
    labs(title = "Test Plot")

  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot_build(p))
})
