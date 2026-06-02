# ---- Tests for Recipe Functions ----

# ---- Histogram ----

test_that("ekio_histogram works with defaults", {
  skip_if_not_installed("ggplot2")
  p <- ekio_histogram(mtcars, mpg)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("ekio_histogram works with static color", {
  skip_if_not_installed("ggplot2")
  p <- ekio_histogram(mtcars, mpg, fill = "steelblue")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("ekio_histogram works with variable mapping", {
  skip_if_not_installed("ggplot2")
  p <- ekio_histogram(mtcars, mpg, fill = factor(cyl), palette = "cool")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("ekio_histogram bin methods work", {
  skip_if_not_installed("ggplot2")
  expect_no_error(ggplot2::ggplot_build(ekio_histogram(mtcars, mpg, bins = "FD")))
  expect_no_error(ggplot2::ggplot_build(ekio_histogram(mtcars, mpg, bins = "scott")))
  expect_no_error(ggplot2::ggplot_build(ekio_histogram(mtcars, mpg, binwidth = 2)))
})

test_that("ekio_histogram rejects non-data-frame input", {
  expect_error(ekio_histogram(1:10, x))
})

# ---- Line Plot ----

test_that("ekio_lineplot works with defaults", {
  skip_if_not_installed("ggplot2")
  p <- ekio_lineplot(ggplot2::economics, date, unemploy)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("ekio_lineplot works with static color", {
  skip_if_not_installed("ggplot2")
  p <- ekio_lineplot(ggplot2::economics, date, unemploy, color = "red")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("ekio_lineplot works with variable mapping", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(
    x = rep(1:10, 2),
    y = c(1:10, 10:1),
    g = rep(c("A", "B"), each = 10)
  )
  p <- ekio_lineplot(df, x, y, color = g, palette = "binary")
  expect_no_error(ggplot2::ggplot_build(p))
})

# ---- Scatter Plot ----

test_that("ekio_scatterplot works with defaults", {
  skip_if_not_installed("ggplot2")
  p <- ekio_scatterplot(mtcars, wt, mpg)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("ekio_scatterplot works with static color", {
  skip_if_not_installed("ggplot2")
  p <- ekio_scatterplot(mtcars, wt, mpg, color = "darkgreen")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("ekio_scatterplot works with variable mapping", {
  skip_if_not_installed("ggplot2")
  p <- ekio_scatterplot(mtcars, wt, mpg, color = factor(cyl))
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("ekio_scatterplot works with size mapping", {
  skip_if_not_installed("ggplot2")
  p <- ekio_scatterplot(mtcars, wt, mpg, size = hp)
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("ekio_scatterplot works with color + size mapping", {
  skip_if_not_installed("ggplot2")
  p <- ekio_scatterplot(mtcars, wt, mpg, color = factor(cyl), size = hp)
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("ekio_scatterplot smooth works", {
  skip_if_not_installed("ggplot2")
  p <- ekio_scatterplot(mtcars, wt, mpg, add_smooth = TRUE)
  expect_no_error(ggplot2::ggplot_build(p))
})

# ---- Bar Plot ----

test_that("ekio_barplot works with defaults", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(x = c("A", "B", "C"), y = c(10, 20, 15))
  p <- ekio_barplot(df, x, y)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("ekio_barplot works with static color", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(x = c("A", "B", "C"), y = c(10, 20, 15))
  p <- ekio_barplot(df, x, y, fill = "coral")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("ekio_barplot works with variable mapping", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(
    x = c("A", "B", "C"),
    y = c(10, 20, 15),
    g = c("X", "Y", "X")
  )
  p <- ekio_barplot(df, x, y, fill = g)
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("ekio_barplot horizontal works", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(x = c("A", "B", "C"), y = c(10, 20, 15))
  p <- ekio_barplot(df, x, y, horizontal = TRUE)
  expect_no_error(ggplot2::ggplot_build(p))
})

# ---- Area Plot ----

test_that("ekio_areaplot works with defaults", {
  skip_if_not_installed("ggplot2")
  p <- ekio_areaplot(ggplot2::economics, date, unemploy)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("ekio_areaplot works with static color", {
  skip_if_not_installed("ggplot2")
  p <- ekio_areaplot(ggplot2::economics, date, unemploy, fill = "steelblue")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("ekio_areaplot works with variable mapping", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(
    x = rep(1:10, 3),
    y = c(1:10, 2:11, 3:12),
    g = rep(c("A", "B", "C"), each = 10)
  )
  p <- ekio_areaplot(df, x, y, fill = g)
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("ekio_areaplot fill position works", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(
    x = rep(1:10, 2),
    y = c(1:10, 10:1),
    g = rep(c("A", "B"), each = 10)
  )
  p <- ekio_areaplot(df, x, y, fill = g, position = "fill")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("ekio_areaplot rejects non-data-frame input", {
  expect_error(ekio_areaplot(1:10, x, y))
})

# ---- Aesthetic Detection ----

test_that("palette ignored warning fires for static color + palette", {
  skip_if_not_installed("ggplot2")
  expect_warning(
    ekio_histogram(mtcars, mpg, fill = "steelblue", palette = "cool"),
    "palette.*ignored"
  )
})

test_that("invalid color string produces error", {
  skip_if_not_installed("ggplot2")
  expect_error(
    ekio_histogram(mtcars, mpg, fill = "not_a_color"),
    "not a valid color"
  )
})
