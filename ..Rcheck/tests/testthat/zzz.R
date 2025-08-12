.onLoad <- function(libname, pkgname) {
  # Check if this is the first time loading
  if (interactive() && !getOption("ekioplot.welcome.shown", FALSE)) {
    packageStartupMessage(
      "Welcome to ekioplot v0.1.0 - EKIO Visual Identity System\n",
      "• Enhanced with 35+ professional color palettes\n",
      "• Smart font management with Google Fonts integration\n",
      "• Custom color indexing for advanced control\n\n",
      "Quick start: load_ekio_fonts() for best typography\n",
      "Explore palettes: list_ekio_palettes() | show_ekio_palette()\n",
      "Documentation: vignette('ekio-visual-identity')\n"
    )
    options(ekioplot.welcome.shown = TRUE)
  }
}

.onAttach <- function(libname, pkgname) {
  # Silent loading, message only on first .onLoad
}
