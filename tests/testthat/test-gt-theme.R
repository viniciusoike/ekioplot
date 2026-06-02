# ---- Tests for GT Theme ----

test_that("gt_theme_ekio returns styled gt table", {
  skip_if_not_installed("gt")
  tbl <- gt::gt(head(mtcars, 5))
  styled <- gt_theme_ekio(tbl)
  expect_s3_class(styled, "gt_tbl")
})

test_that("gt_theme_ekio rejects non-gt input", {
  expect_error(gt_theme_ekio(mtcars), "gt table object")
})

test_that("gt_theme_ekio without footer works", {
  skip_if_not_installed("gt")
  tbl <- gt::gt(head(mtcars, 5))
  styled <- gt_theme_ekio(tbl, add_footer = FALSE)
  expect_s3_class(styled, "gt_tbl")
})

test_that("gt_theme_ekio without striping works", {
  skip_if_not_installed("gt")
  tbl <- gt::gt(head(mtcars, 5))
  styled <- gt_theme_ekio(tbl, stripe = FALSE)
  expect_s3_class(styled, "gt_tbl")
})

test_that("gt_theme_ekio custom sizing works", {
  skip_if_not_installed("gt")
  tbl <- gt::gt(head(mtcars, 5))
  styled <- gt_theme_ekio(tbl, table_width = "80%", font_size = 12)
  expect_s3_class(styled, "gt_tbl")
})
