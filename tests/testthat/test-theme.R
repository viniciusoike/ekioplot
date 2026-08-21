# ---- Tests for Theme v3.0 ----

test_that("theme_ekio creates valid ggplot2 theme", {
  skip_if_not_installed("ggplot2")

  theme_default <- theme_ekio()
  expect_s3_class(theme_default, "theme")
  expect_s3_class(theme_default, "gg")

  theme_large <- theme_ekio(base_size = 14)
  expect_s3_class(theme_large, "theme")

  theme_font <- theme_ekio(font_title = "serif", font_text = "sans")
  expect_s3_class(theme_font, "theme")
})

test_that("font options override branded defaults", {
  old_options <- options(
    ekioplot.font_title = "serif",
    ekioplot.font_text = "sans"
  )
  on.exit(options(old_options), add = TRUE)

  theme <- theme_ekio()
  expect_equal(theme$plot.title$family, "serif")
  expect_equal(theme$plot.subtitle$family, "sans")
})

test_that("theme_ekio grid parameter works", {
  skip_if_not_installed("ggplot2")

  grid_colour <- .ekio("gray", 200)
  theme_y <- theme_ekio(grid = "y")
  theme_x <- theme_ekio(grid = "x")
  theme_xy <- theme_ekio(grid = "xy")
  theme_none <- theme_ekio(grid = "none")

  expect_equal(theme_y$panel.grid.major.y$colour, grid_colour)
  expect_equal(theme_x$panel.grid.major.x$colour, grid_colour)
  expect_equal(theme_xy$panel.grid.major.y$colour, grid_colour)
  expect_equal(theme_xy$panel.grid.major.x$colour, grid_colour)
  expect_null(theme_y$panel.grid.major.x)
  expect_null(theme_x$panel.grid.major.y)
  expect_null(theme_none$panel.grid.major.y)
  expect_null(theme_none$panel.grid.major.x)
})

test_that("axis ticks are independent of major grids", {
  skip_if_not_installed("ggplot2")

  theme_x <- theme_ekio(grid = "none", ticks = "x")
  theme_y <- theme_ekio(grid = "none", ticks = "y")
  theme_xy <- theme_ekio(grid = "none", ticks = "xy")
  theme_none <- theme_ekio(grid = "none", ticks = "none")

  expect_s3_class(theme_x$axis.ticks.x, "element_line")
  expect_null(theme_x$axis.ticks.y)
  expect_null(theme_y$axis.ticks.x)
  expect_s3_class(theme_y$axis.ticks.y, "element_line")
  expect_s3_class(theme_xy$axis.ticks.x, "element_line")
  expect_s3_class(theme_xy$axis.ticks.y, "element_line")
  expect_null(theme_none$axis.ticks.x)
  expect_null(theme_none$axis.ticks.y)
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

test_that("theme_ekio_map is removed", {
  expect_false("theme_ekio_map" %in% getNamespaceExports("ekioplot"))
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

test_that("grid argument is validated like ticks", {
  expect_error(theme_ekio(grid = "bogus"))
  expect_error(theme_ekio(ticks = "bogus"))
})

test_that("theme_ekio sets paper from the brand background token", {
  # colors$off_white was a typo for colors$offwhite, so paper silently went
  # NULL. ggplot2 consumes `paper` into rect$fill rather than storing it.
  expect_equal(theme_ekio()$rect$fill, .ekio("basic", "offwhite"))
})

test_that("background sets paper, plot and panel together", {
  expected <- c(
    offwhite = .ekio("basic", "offwhite"),
    white = .ekio("basic", "white"),
    gray = .ekio("gray", 100)
  )
  for (nm in names(expected)) {
    th <- theme_ekio(background = nm)
    expect_equal(th$rect$fill, expected[[nm]], info = nm)
    expect_equal(th$plot.background$fill, expected[[nm]], info = nm)
    expect_equal(th$panel.background$fill, expected[[nm]], info = nm)
  }

  th <- theme_ekio(background = "transparent")
  expect_true(is.na(th$plot.background$fill))
  expect_true(is.na(th$panel.background$fill))

  expect_error(theme_ekio(background = "bogus"))
})
