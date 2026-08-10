# ---- Tests for Color System v3.0 ----

test_that("ekio_pal returns correct structure", {
  colors_default <- ekio_pal()
  expect_type(colors_default, "character")
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors_default)))

  expect_length(ekio_pal("contrast"), 6)
  expect_length(ekio_pal("cool"), 3)
  expect_length(ekio_pal("full"), 8)
  expect_length(ekio_pal("binary"), 2)

  pal_rev <- ekio_pal("contrast", reverse = TRUE)
  pal_norm <- ekio_pal("contrast")
  expect_equal(as.character(pal_rev), rev(pal_norm))
})

test_that("ekio_pal n parameter works", {
  expect_length(ekio_pal("contrast", n = 4), 4)
  expect_length(ekio_pal("contrast", n = 2), 2)

  # Interpolation when n > palette length
  interp <- ekio_pal("binary", n = 5)
  expect_length(interp, 5)
})

test_that("small group variants work", {
  expect_length(ekio_pal("duo_warm"), 2)
  expect_length(ekio_pal("duo_cool"), 2)
  expect_length(ekio_pal("trio_bold"), 3)
  expect_length(ekio_pal("trio_cool"), 3)
  expect_length(ekio_pal("quad_earth"), 4)
  expect_length(ekio_pal("quad_vivid"), 4)
})

test_that("scientific palettes are accessible via ekio_pal", {
  expect_length(ekio_pal("okabe_ito"), 8)
  expect_length(ekio_pal("viridis"), 9)
  expect_length(ekio_pal("inferno"), 10)
  expect_length(ekio_pal("plasma"), 10)

  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", ekio_pal("okabe_ito"))))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", ekio_pal("viridis"))))
})

test_that("sequential palettes are accessible via ekio_pal", {
  seq_names <- list_ekio_palettes("sequential")
  for (pal in seq_names) {
    colors <- ekio_pal(pal)
    expect_type(colors, "character")
    expect_true(length(colors) >= 9)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors)))
  }
})

test_that("diverging palettes are accessible via ekio_pal", {
  div_names <- list_ekio_palettes("diverging")
  expect_true(length(div_names) >= 3)
  for (pal in div_names) {
    colors <- ekio_pal(pal)
    expect_type(colors, "character")
    expect_true(length(colors) >= 11)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors)))
  }
})

test_that("list_ekio_palettes returns correct structure", {
  all_palettes <- list_ekio_palettes("all")
  expect_type(all_palettes, "list")
  expect_true("categorical" %in% names(all_palettes))
  expect_true("small_group" %in% names(all_palettes))
  expect_true("scientific" %in% names(all_palettes))
  expect_true("sequential" %in% names(all_palettes))
  expect_true("diverging" %in% names(all_palettes))

  expect_true("contrast" %in% list_ekio_palettes("categorical"))
  expect_true("okabe_ito" %in% list_ekio_palettes("scientific"))
  expect_true("duo_warm" %in% list_ekio_palettes("small_group"))
  expect_true("blue" %in% list_ekio_palettes("sequential"))
  expect_true("purple" %in% list_ekio_palettes("sequential"))
  expect_true("blue_orange" %in% list_ekio_palettes("diverging"))
})

test_that("list_ekio_palettes verbose prints summary and returns invisibly", {
  out <- cli::cli_fmt(res <- withVisible(list_ekio_palettes(verbose = TRUE)))
  expect_false(res$visible)
  expect_identical(res$value, list_ekio_palettes())
  expect_true(any(grepl("Available Palettes", out)))
  expect_true(any(grepl("Diverging", out)))
})

test_that("list_ekio_palettes verbose respects the type filter", {
  out <- cli::cli_fmt(res <- list_ekio_palettes("categorical", verbose = TRUE))
  expect_identical(res, list_ekio_palettes("categorical"))
  expect_true(any(grepl("Categorical", out)))
  expect_false(any(grepl("Diverging|Sequential|Scientific", out)))
})

test_that("ekio_pal returns ekio_palette class that auto-prints", {
  p <- ekio_pal("contrast")
  expect_s3_class(p, "ekio_palette")
  expect_type(p, "character")
  expect_length(p, 6)

  stripped <- as.character(p)
  expect_type(stripped, "character")
  expect_null(attr(stripped, "class"))
})

test_that("show_all_ekio_palettes warns about deprecation but still works", {
  expect_warning(
    res <- suppressMessages(show_all_ekio_palettes()),
    "deprecated"
  )
  expect_identical(res, list_ekio_palettes())
})

test_that("retired color vectors are gone", {
  for (nm in c(
    "ekio_blue", "ekio_gray", "ekio_teal", "ekio_orange", "ekio_accent"
  )) {
    expect_false(nm %in% getNamespaceExports("ekioplot"))
  }
})

test_that("brand scales are nine steps named 100..900", {
  shades <- as.character(seq(100, 900, by = 100))
  for (nm in list_ekio_palettes("sequential")) {
    pal <- ekio_pal(nm)
    expect_length(pal, 9)
    expect_named(pal, shades)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", as.character(pal))))
  }
})

test_that("position and shade are aligned in brand scales", {
  for (nm in list_ekio_palettes("sequential")) {
    pal <- ekio_pal(nm)
    for (i in seq_len(9)) {
      expect_identical(
        unname(pal[i]),
        unname(pal[as.character(i * 100)]),
        info = paste0(nm, " position ", i)
      )
    }
  }
})

test_that("brand scales darken monotonically", {
  lum <- function(x) {
    rgb <- grDevices::col2rgb(x)
    0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]
  }
  for (nm in list_ekio_palettes("sequential")) {
    expect_true(
      all(diff(lum(as.character(ekio_pal(nm)))) < 0),
      info = nm
    )
  }
})

test_that("n interpolates across ramps but truncates categorical palettes", {
  # Sequential: n = 3 spans the full range rather than taking the 3 lightest
  three <- as.character(ekio_pal("blue", n = 3))
  expect_length(three, 3)
  expect_identical(three[1], unname(as.character(ekio_pal("blue"))[1]))
  expect_identical(three[3], unname(as.character(ekio_pal("blue"))[9]))

  # Categorical: n = 3 takes the first 3, which are ordered by preference
  expect_identical(
    as.character(ekio_pal("contrast", n = 3)),
    as.character(ekio_pal("contrast"))[1:3]
  )
})

test_that("the internal token accessor rejects bad scales and shades", {
  expect_identical(
    ekioplot:::.ekio("blue", 700),
    unname(as.character(ekio_pal("blue")["700"]))
  )
  expect_error(ekioplot:::.ekio("blue", 50), "Unknown shade")
  expect_error(ekioplot:::.ekio("blue", 999), "Unknown shade")
  expect_error(ekioplot:::.ekio("chartreuse", 500), "Unknown color scale")
})

test_that("n equal to the palette length preserves shade names", {
  full <- ekio_pal("blue")
  same <- ekio_pal("blue", n = length(full))
  expect_identical(as.character(same), as.character(full))
  expect_named(same, names(full))
  expect_identical(unname(same["700"]), unname(full["700"]))
})

test_that("invalid palette names produce errors", {
  expect_error(ekio_pal("nonexistent_palette"))
})

# ---- .ekio token access ----

test_that(".ekio resolves scales by shade and by position", {
  expect_identical(.ekio("blue", 700), unname(.ekio_scales$blue[["700"]]))
  expect_identical(.ekio("blue", 7), .ekio("blue", 700))
  expect_identical(.ekio("gray", "100"), .ekio("gray", 1))
})

test_that(".ekio resolves palettes by name and by position", {
  expect_identical(.ekio("basic", "white"), "#FFFFFF")
  expect_identical(.ekio("basic", "offwhite"), "#FEFEFE")
  expect_identical(.ekio("basic", 1), .ekio("basic", "white"))
  expect_identical(.ekio("contrast", 2), as.character(ekio_pal("contrast"))[2])
})

test_that(".ekio prefers the scale when a palette shares its name", {
  # Sequential palettes are their scale, so both routes must agree
  expect_identical(.ekio("blue", 700), unname(ekio_pal("blue")[["700"]]))
})

test_that(".ekio rejects unknown tokens and non-scalar input", {
  expect_error(.ekio("bogus", 1), "Unknown color scale or palette")
  expect_error(.ekio("blue", 750), "Unknown shade")
  expect_error(.ekio("basic", "beige"), "Unknown color")
  expect_error(.ekio("contrast", 99), "Unknown color")
  expect_error(.ekio("contrast", 0), "Unknown color")
  expect_error(.ekio(c("blue", "gray"), 700), "single scale or palette name")
  expect_error(.ekio("blue", c(100, 200)), "single shade")
})

test_that("basic is a token group, not a user-facing palette", {
  # white/offwhite/black are surfaces, not something data maps onto
  expect_false("basic" %in% unlist(list_ekio_palettes()))
  expect_false("basic" %in% names(list_ekio_palettes()))
  expect_error(ekio_pal("basic"), "not found")
  expect_error(list_ekio_palettes("basic"), "Unknown palette type")

  # ...but it stays reachable as a brand token
  expect_identical(.ekio("basic", "offwhite"), "#FEFEFE")
})

# ---- Palette swatch ----

test_that("the swatch plot builds for named and unnamed palettes", {
  skip_if_not_installed("ggplot2")

  # Brand scales carry shade names; categorical palettes label with hex
  scale_swatch <- .palette_plot(ekio_pal("blue"))
  expect_no_error(ggplot2::ggplot_build(scale_swatch))
  expect_equal(ggplot2::layer_data(scale_swatch, 2)$label, names(ekio_pal("blue")))

  flat_swatch <- .palette_plot(ekio_pal("contrast"))
  expect_no_error(ggplot2::ggplot_build(flat_swatch))
  expect_equal(
    ggplot2::layer_data(flat_swatch, 2)$label,
    as.character(ekio_pal("contrast"))
  )
})

test_that("the swatch fills with the palette and titles with its name", {
  skip_if_not_installed("ggplot2")
  p <- .palette_plot(ekio_pal("okabe_ito"))

  expect_equal(ggplot2::layer_data(p, 1)$fill, as.character(ekio_pal("okabe_ito")))
  expect_equal(p$labels$title, "Palette: okabe_ito")
  # Labels are picked for contrast against their own tile
  expect_true(all(
    ggplot2::layer_data(p, 2)$colour %in% c("white", .ekio("gray", 900))
  ))
})
