# ---- Tests for Accessibility Helpers ----

test_that("ekio_contrast returns known WCAG values", {
  expect_equal(ekio_contrast("black", "white"), 21)
  expect_equal(ekio_contrast("white", "white"), 1)
  expect_equal(ekio_contrast("black", "white"), ekio_contrast("white", "black"))
})

test_that("ekio_contrast is vectorized and recycles", {
  ratios <- ekio_contrast("white", ekio_blue)
  expect_length(ratios, length(ekio_blue))
  expect_type(ratios, "double")
  expect_true(all(ratios >= 1 & ratios <= 21))
})

test_that("ekio_contrast validates input", {
  expect_snapshot(ekio_contrast(1), error = TRUE)
})

test_that("ekio_text_on picks the higher-contrast text color", {
  expect_equal(unname(ekio_text_on(ekio_blue["700"])), "white")
  expect_equal(unname(ekio_text_on(ekio_blue["50"])), "black")
  expect_equal(unname(ekio_text_on("white")), "black")
})

test_that("ekio_text_on is vectorized and preserves names", {
  out <- ekio_text_on(ekio_blue)
  expect_length(out, length(ekio_blue))
  expect_named(out, names(ekio_blue))
  expect_setequal(unique(out), c("black", "white"))
})

test_that("ekio_text_on accepts custom candidates", {
  out <- ekio_text_on(ekio_accent, dark = ekio_gray["900"], light = "#FFFFFF")
  expect_in(unname(out), c(unname(ekio_gray["900"]), "#FFFFFF"))
})
