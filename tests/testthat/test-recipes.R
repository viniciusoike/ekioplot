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

test_that("an unknown column errors at the recipe, not at build time", {
  skip_if_not_installed("ggplot2")
  expect_error(
    ekio_scatterplot(mtcars, wt, mpg, color = nope),
    "Can't evaluate"
  )
})

test_that("build-time aesthetics are left for ggplot2 to resolve", {
  skip_if_not_installed("ggplot2")
  p <- ekio_histogram(mtcars, mpg, fill = ggplot2::after_stat(count > 5))
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("dots override the recipe's geom defaults", {
  skip_if_not_installed("ggplot2")
  # The mapped arm sets alpha = 0.7; the user's value should win
  p <- ekio_histogram(mtcars, mpg, fill = factor(cyl), alpha = 0.3)
  expect_equal(unique(ggplot2::layer_data(p)$alpha), 0.3)
})

# ---- Plot Labels ----

test_that("recipes set title, subtitle, and caption", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(x = 1:10, y = 1:10)
  recipes <- list(
    histogram = \(...) ekio_histogram(df, x, ...),
    lineplot = \(...) ekio_lineplot(df, x, y, ...),
    scatterplot = \(...) ekio_scatterplot(df, x, y, ...),
    barplot = \(...) ekio_barplot(df, x, y, ...),
    areaplot = \(...) ekio_areaplot(df, x, y, ...)
  )

  for (nm in names(recipes)) {
    labels <- recipes[[nm]](title = "T", subtitle = "S", caption = "C")$labels
    expect_equal(labels$title, "T", info = nm)
    expect_equal(labels$subtitle, "S", info = nm)
    expect_equal(labels$caption, "C", info = nm)
  }
})

test_that("label defaults leave axis labels derived from the data", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(x = 1:10, y = 1:10)

  # labs(title = NULL) is inert, but labs(x = NULL) would drop the axis
  # label ggplot2 derives from the mapping. Guard against that regression.
  # Derived labels only materialize at build time, not on the raw plot.
  labels <- ggplot2::ggplot_build(ekio_lineplot(df, x, y))$plot$labels
  expect_null(labels$title)
  expect_equal(labels$x, "x")
  expect_equal(labels$y, "y")
})

# ---- Continuous mappings ----

test_that("binned, bar and band recipes reject continuous mappings", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(x = 1:5, y = c(1, 2, 3, 4, 5), v = c(1.5, 2, 3, 4, 5), g = letters[1:5])

  expect_error(ekio_histogram(df, y, fill = v), "must map a discrete variable")
  expect_error(ekio_lineplot(df, x, y, color = v), "must map a discrete variable")
  expect_error(ekio_barplot(df, g, y, fill = v), "must map a discrete variable")
  expect_error(ekio_areaplot(df, x, y, fill = v), "must map a discrete variable")
})

test_that("scatterplot warns on a continuous mapping but still builds", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(x = 1:5, y = c(1, 2, 3, 4, 5), v = c(1.5, 2, 3, 4, 5))

  # "contrast" is categorical: defaulting to it errored in scale_*_ekio_c()
  expect_warning(p <- ekio_scatterplot(df, x, y, color = v), "continuous")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("discrete mappings keep the categorical default", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(x = letters[1:5], y = 1:5)

  expect_no_error(ggplot2::ggplot_build(ekio_barplot(df, x, y, fill = x)))
  expect_equal(.default_palette(NULL, list(is_continuous = FALSE)), "contrast")
  expect_equal(.default_palette(NULL, list(is_continuous = TRUE)), "blue")
  expect_equal(.default_palette("cool", list(is_continuous = TRUE)), "cool")
})

test_that("all recipes reject non-data-frame input", {
  expect_error(ekio_histogram(1:10, x))
  expect_error(ekio_lineplot(1:10, x, y))
  expect_error(ekio_scatterplot(1:10, x, y))
  expect_error(ekio_barplot(1:10, x, y))
  expect_error(ekio_areaplot(1:10, x, y))
})
