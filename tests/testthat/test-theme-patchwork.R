test_that("theme_patchwork carries plot styling from theme_ekio", {
  base <- theme_ekio(background = "#F9F8F3", base_size = 14)
  patchwork <- theme_patchwork(background = "#F9F8F3", base_size = 14)

  expect_s3_class(patchwork, "theme")
  expect_equal(patchwork$plot.background, base$plot.background)
  expect_equal(patchwork$plot.title, base$plot.title)
  expect_equal(patchwork$plot.subtitle, base$plot.subtitle)
  expect_equal(patchwork$plot.caption, base$plot.caption)
  expect_equal(patchwork$plot.margin, base$plot.margin)
  expect_null(patchwork$panel.background)
  expect_null(patchwork$axis.text)
})
