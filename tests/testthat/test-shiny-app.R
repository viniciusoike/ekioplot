# ---- Tests for the Palette Lab Shiny app ----
# The app lives in inst/shiny-app and depends on several Suggests packages,
# so these tests skip cleanly when any are unavailable.

app_deps <- c(
  "shiny", "bslib", "dplyr", "forcats", "stringr", "tibble",
  "ggbump", "patchwork", "colorspace", "colourpicker"
)

skip_app_unless_ready <- function() {
  skip_on_cran()
  for (pkg in app_deps) skip_if_not_installed(pkg)
  app_dir <- system.file("shiny-app", package = "ekioplot")
  if (app_dir == "" || !file.exists(file.path(app_dir, "app_data.rds"))) {
    skip("Shiny app directory or precomputed data not available")
  }
  app_dir
}

test_that("app server reacts to palette edits, presets, and CVD", {
  app_dir <- skip_app_unless_ready()
  app <- shiny::shinyAppDir(app_dir)

  shiny::testServer(app, {
    session$setInputs(
      n_colors = 6, palette_name = "contrast",
      cvd_view = "none", dark_mode = "light"
    )
    expect_length(current_colors(), 6)

    # Preset load swaps the palette and records history
    session$setInputs(preset = "viridis")
    expect_length(current_colors(), 9)
    expect_gte(length(history()), 1)

    # Debounced pipeline reflects the loaded palette
    session$elapse(300)
    expect_identical(
      plot_colors(),
      toupper(unname(ekioplot::ekio_pal("viridis")))
    )

    # A single valid edit propagates; an invalid hex is rejected
    session$setInputs(color_1 = "#123456")
    session$elapse(300)
    expect_identical(plot_colors()[1], "#123456")
    before <- current_colors()[2]
    session$setInputs(color_2 = "#12")
    expect_identical(current_colors()[2], before)

    # CVD transform changes the colors handed to the plots
    session$setInputs(cvd_view = "deutan")
    session$elapse(300)
    expect_false(identical(plot_colors(), current_colors()))
    session$setInputs(cvd_view = "none")
  })
})

test_that("pin enables in-place A/B swap and history restores", {
  app_dir <- skip_app_unless_ready()
  app <- shiny::shinyAppDir(app_dir)

  shiny::testServer(app, {
    session$setInputs(
      n_colors = 6, palette_name = "contrast",
      cvd_view = "none", dark_mode = "light", preset = "contrast"
    )
    session$elapse(300)

    session$setInputs(pin_btn = 1)
    pinned_snapshot <- pinned_colors()
    expect_false(is.null(pinned_snapshot))

    session$setInputs(preset = "viridis")
    session$setInputs(ab_view = "pinned")
    session$elapse(300)
    expect_identical(active_colors(), pinned_snapshot)

    session$setInputs(ab_view = "current")
    session$elapse(300)
    expect_identical(
      active_colors(),
      toupper(unname(ekioplot::ekio_pal("viridis")))
    )

    # Restore an earlier palette from history
    expect_gte(length(history()), 1)
    session$setInputs(history_select = 1)
    expect_identical(current_colors(), history()[[1]]$colors)
  })
})

test_that("diagnostics and plot outputs render without error", {
  app_dir <- skip_app_unless_ready()
  app <- shiny::shinyAppDir(app_dir)

  shiny::testServer(app, {
    session$setInputs(
      n_colors = 6, palette_name = "contrast",
      cvd_view = "none", dark_mode = "light"
    )
    session$elapse(300)

    # Benign rendering warnings (patchwork theme merge, dropped NA labels) are
    # silenced — these checks only assert the outputs build without error.
    expect_no_error(invisible(output$distance_summary))
    expect_no_error(invisible(output$contrast_check))
    expect_no_error(invisible(output$cvd_strips))
    suppressWarnings({
      expect_no_error(invisible(output$area_stacked))
      expect_no_error(invisible(output$bar_highlighted))
      expect_no_error(invisible(output$bubble_plot))
      expect_no_error(invisible(output$bump_plot))
      expect_no_error(invisible(output$pyramid_plot))
    })

    # Dark mode swaps in a non-empty theme
    session$setInputs(dark_mode = "dark")
    expect_gt(length(plot_theme_extra()), 0)
  })
})
