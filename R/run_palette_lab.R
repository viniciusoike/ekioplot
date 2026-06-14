#' Launch the EKIO Palette Lab Shiny app
#'
#' Opens the Palette Lab, an interactive explorer for building and comparing
#' EKIO colour palettes across a wide range of chart types. Edit palettes
#' live, simulate colour-vision deficiencies, check contrast, and copy ready
#' to-paste scale code.
#'
#' A hosted version that runs entirely in the browser (no installation needed)
#' is available at <https://viniciusoike.github.io/ekioplot/palette-lab/>.
#'
#' @details
#' The app relies on a number of packages listed under `Suggests` rather than
#' `Imports`, so they are not installed with `ekioplot` by default. If any are
#' missing you will be prompted to install them.
#'
#' @param ... Additional arguments passed to [shiny::runApp()], such as
#'   `launch.browser` or `port`.
#'
#' @return Called for its side effect of launching the app. Invisibly returns
#'   the app directory path.
#' @export
#'
#' @examplesIf rlang::is_interactive()
#' run_palette_lab()
run_palette_lab <- function(...) {
  app_pkgs <- c(
    "shiny", "bslib", "ggplot2", "dplyr", "forcats", "stringr", "tibble",
    "ggbump", "patchwork", "colorspace", "colourpicker"
  )
  rlang::check_installed(app_pkgs, "to run the EKIO Palette Lab.")

  app_dir <- system.file("shiny-app", package = "ekioplot")
  if (!nzchar(app_dir)) {
    cli::cli_abort(
      "Could not find the Palette Lab app directory in the installed package."
    )
  }

  shiny::runApp(app_dir, ...)
  invisible(app_dir)
}
