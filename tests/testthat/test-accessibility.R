# ---- Tests for Accessibility Helpers ----

test_that("ekio_contrast returns known WCAG values", {
  expect_equal(ekio_contrast("black", "white"), 21)
  expect_equal(ekio_contrast("white", "white"), 1)
  expect_equal(ekio_contrast("black", "white"), ekio_contrast("white", "black"))
})

test_that("ekio_contrast is vectorized and recycles", {
  blue <- ekio_pal("blue")
  ratios <- ekio_contrast("white", blue)
  expect_length(ratios, length(blue))
  expect_type(ratios, "double")
  expect_true(all(ratios >= 1 & ratios <= 21))
})

test_that("ekio_contrast validates input", {
  expect_snapshot(ekio_contrast(1), error = TRUE)
})

test_that("ekio_text_on picks the higher-contrast text color", {
  expect_equal(unname(ekio_text_on(ekio_pal("blue")["700"])), "white")
  expect_equal(unname(ekio_text_on(ekio_pal("blue")["100"])), "black")
  expect_equal(unname(ekio_text_on("white")), "black")
})

test_that("ekio_text_on is vectorized and preserves names", {
  blue <- ekio_pal("blue")
  out <- ekio_text_on(blue)
  expect_length(out, length(blue))
  expect_named(out, names(blue))
  expect_setequal(unique(out), c("black", "white"))
})

test_that("ekio_text_on accepts custom candidates", {
  ink <- unname(ekio_pal("gray")["900"])
  out <- ekio_text_on(ekio_pal("full"), dark = ink, light = "#FFFFFF")
  expect_in(unname(out), c(ink, "#FFFFFF"))
})
