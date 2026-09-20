# ---- Tests for brand surfaces ----

test_that("ekio_surface returns the same hex as the theme background", {
  for (nm in c("offwhite", "white", "cold", "gray")) {
    expect_equal(
      ekio_surface(nm),
      theme_ekio(background = nm)$panel.background$fill,
      info = nm
    )
  }
})

test_that("ekio_surface returns NA_character_ for transparent", {
  expect_identical(ekio_surface("transparent"), NA_character_)
})

test_that("ekio_surface passes hex codes through unchanged", {
  expect_equal(ekio_surface("#F0EAD6"), "#F0EAD6")
  expect_equal(ekio_surface("#EEE"), "#EEE")
})

test_that("ekio_surface validates its input", {
  expect_error(ekio_surface("ivory"), "must be one of")
  expect_error(ekio_surface("#GGGGGG"), "must be one of")
  expect_error(ekio_surface(c("white", "cold")), "single string")
})
