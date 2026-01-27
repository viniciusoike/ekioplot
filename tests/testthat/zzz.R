# =============================================================================
# Package Load Hooks
# =============================================================================

.onLoad <- function(libname, pkgname) {
  # Silent loading
}

.onAttach <- function(libname, pkgname) {
  if (interactive() && !getOption("ekioplot.welcome.shown", FALSE)) {
    packageStartupMessage(
      "ekioplot v2.0.0 - EKIO Visual Identity System\n",
      "Explore: show_all_ekio_palettes() | show_ekio_palette(\"contrast\")"
    )
    options(ekioplot.welcome.shown = TRUE)
  }
}
