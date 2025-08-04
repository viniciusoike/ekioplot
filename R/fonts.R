# Enhanced Font System for EKIO Package
# Based on hrbrthemes approach but adapted for EKIO needs

#' Load EKIO Fonts
#'
#' Loads and registers EKIO fonts for use in plots. This function should be called
#' once per session before using EKIO themes.
#'
#' @param install_missing Logical, whether to attempt installing missing fonts from Google Fonts
#' @param verbose Logical, whether to print font loading messages
#' @return Invisibly returns a list of successfully loaded fonts
#' @examples
#' \dontrun{
#' # Load fonts at start of session
#' load_ekio_fonts()
#'
#' # Install missing fonts and load
#' load_ekio_fonts(install_missing = TRUE)
#' }
#' @export
load_ekio_fonts <- function(install_missing = FALSE, verbose = TRUE) {

  # Required packages for font management
  if (!requireNamespace("extrafont", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg extrafont} needed for font management. Please install it.")
  }

  if (install_missing && !requireNamespace("showtext", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg showtext} needed for Google Fonts. Please install it.")
  }

  # EKIO font hierarchy
  ekio_fonts <- list(
    primary = list(
      name = "Avenir",
      fallback = "Helvetica Neue",
      source = "system"  # Available on macOS by default
    ),
    secondary = list(
      name = "Lato",
      fallback = "Helvetica Neue",
      source = "google"
    ),
    mono = list(
      name = "Fira Code",
      fallback = "Monaco",
      source = "google"
    ),
    condensed = list(
      name = "Roboto Condensed",
      fallback = "Arial Narrow",
      source = "google"
    )
  )

  loaded_fonts <- list()

  if (verbose) cli::cli_alert_info("Loading EKIO fonts...")

  # Check and load system fonts (macOS)
  if (.Platform$OS.type == "unix" && Sys.info()["sysname"] == "Darwin") {
    # macOS - Avenir and Helvetica Neue should be available
    system_fonts <- c("Avenir", "Helvetica Neue")

    for (font in system_fonts) {
      if (font %in% extrafont::fonts()) {
        loaded_fonts[[font]] <- font
        if (verbose) cli::cli_alert_success("Loaded system font:", font, "\n")
      } else {
        if (verbose) cli::cli_alert_warning("System font not found:", font, "\n")
      }
    }
  }

  # Load Google Fonts if requested
  if (install_missing) {
    if (verbose) cli::cli_alert_info("Installing Google Fonts...")

    google_fonts <- c("Lato", "Fira Code", "Roboto Condensed")

    if (requireNamespace("showtext", quietly = TRUE)) {
      showtext::showtext_auto()

      for (font in google_fonts) {
        tryCatch({
          if (font == "Fira Code") {
            sysfonts::font_add_google("Fira Code", "firacoded")
            loaded_fonts[["Fira Code"]] <- "firacoded"
          } else if (font == "Roboto Condensed") {
            sysfonts::font_add_google("Roboto Condensed", "robotocondensed")
            loaded_fonts[["Roboto Condensed"]] <- "robotocondensed"
          } else {
            sysfonts::font_add_google(font, tolower(font))
            loaded_fonts[[font]] <- tolower(font)
          }
          if (verbose) cli::cli_alert_success("Installed Google Font:", font, "\n")
        }, error = function(e) {
          if (verbose) cli::cli_alert_warning("Failed to install:", font, "-", e$message, "\n")
        })
      }
    }
  }

  # Fallback to system defaults if needed
  if (length(loaded_fonts) == 0) {
    loaded_fonts[["system"]] <- "sans"
    if (verbose) cli::cli_alert_info("Using system default fonts")
  }

  if (verbose) {
    cli::cli_alert_info("Font loading complete. Available fonts: {names(loaded_fonts)}.")
  }
  invisible(loaded_fonts)
}

#' Import Lato Font Family
#'
#' Downloads and imports Lato font family from Google Fonts
#'
#' @param install Logical, whether to install the font
#' @return Invisibly returns font family name if successful
#' @examples
#' \dontrun{
#' import_lato()
#' }
#' @export
import_lato <- function(install = TRUE) {
  if (!install) return(invisible(NULL))

  if (!requireNamespace("showtext", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg showtext} needed. Please install it.")
  }

  tryCatch({
    sysfonts::font_add_google("Lato", "lato")
    showtext::showtext_auto()
    cli::cli_alert_success("Lato font family imported successfully")
    invisible("lato")
  }, error = function(e) {
    cli::cli_warn("Failed to import Lato: ", e$message)
    invisible(NULL)
  })
}

#' Import Fira Code Font Family
#'
#' Downloads and imports Fira Code font family from Google Fonts
#'
#' @param install Logical, whether to install the font
#' @return Invisibly returns font family name if successful
#' @examples
#' \dontrun{
#' import_fira_code()
#' }
#' @export
import_fira_code <- function(install = TRUE) {
  if (!install) return(invisible(NULL))

  if (!requireNamespace("showtext", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg showtext} needed. Please install it.")
  }

  tryCatch({
    sysfonts::font_add_google("Fira Code", "firacoded")
    showtext::showtext_auto()
    cli::cli_alert_success("Fira Code font family imported successfully")
    invisible("firacode")
  }, error = function(e) {
    cli::cli_warn("Failed to import Fira Code: ", e$message)
    invisible(NULL)
  })
}

#' Import Roboto Condensed Font Family
#'
#' Downloads and imports Roboto Condensed font family from Google Fonts
#'
#' @param install Logical, whether to install the font
#' @return Invisibly returns font family name if successful
#' @examples
#' \dontrun{
#' import_roboto_condensed()
#' }
#' @export
import_roboto_condensed <- function(install = TRUE) {
  if (!install) return(invisible(NULL))

  if (!requireNamespace("showtext", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg showtext} needed. Please install it.")
  }

  tryCatch({
    sysfonts::font_add_google("Roboto Condensed", "robotocondensed")
    showtext::showtext_auto()
    cli::cli_alert_success("Roboto Condensed font family imported successfully")
    invisible("robotocondensed")
  }, error = function(e) {
    cli::cli_warn("Failed to import Roboto Condensed: ", e$message)
    invisible(NULL)
  })
}

#' Get Best Available EKIO Font
#'
#' Returns the best available font from EKIO hierarchy
#'
#' @param type Font type: "primary", "secondary", "mono", "condensed"
#' @param loaded_fonts List of loaded fonts (from load_ekio_fonts)
#' @return Font family name
#' @examples
#' fonts <- load_ekio_fonts()
#' get_ekio_font("primary", fonts)
#'
#' @export
get_ekio_font <- function(type = "primary", loaded_fonts = NULL) {

  if (is.null(loaded_fonts)) {
    loaded_fonts <- load_ekio_fonts(verbose = FALSE)
  }

  # Font preferences by type
  preferences <- list(
    primary = c("Avenir", "Helvetica Neue", "sans"),
    secondary = c("lato", "Lato", "Helvetica Neue", "sans"),
    mono = c("firacoded", "Fira Code", "Monaco", "Courier", "mono"),
    condensed = c("robotocondensed", "Roboto Condensed", "Arial Narrow", "sans")
  )

  fonts_to_try <- preferences[[type]]
  if (is.null(fonts_to_try)) fonts_to_try <- preferences[["primary"]]

  # Find first available font
  for (font in fonts_to_try) {
    if (font %in% names(loaded_fonts) || font %in% c("sans", "serif", "mono")) {
      return(font)
    }
  }

  # Ultimate fallback
  return("sans")
}

#' Check Font Availability
#'
#' Checks which EKIO fonts are available on the system
#'
#' @param detailed Logical, whether to return detailed information
#' @return List or data frame of font availability
#' @examples
#' check_ekio_fonts()
#' check_ekio_fonts(detailed = TRUE)
#'
#' @export
check_ekio_fonts <- function(detailed = FALSE) {

  # Check system fonts
  system_fonts <- c("Avenir", "Helvetica Neue", "Arial", "Times", "Courier")
  available_system <- system_fonts[system_fonts %in% extrafont::fonts()]

  # Check Google Fonts (if showtext is available)
  google_status <- list()
  if (requireNamespace("showtext", quietly = TRUE)) {
    google_fonts <- c("Lato", "Fira Code", "Roboto Condensed")
    for (font in google_fonts) {
      google_status[[font]] <- "Available for download"
    }
  }

  if (!detailed) {
    return(list(
      system_available = available_system,
      google_available = names(google_status)
    ))
  }

  # Detailed status
  all_fonts <- data.frame(
    Font = c("Avenir", "Helvetica Neue", "Lato", "Fira Code", "Roboto Condensed"),
    Type = c("Primary", "Primary", "Secondary", "Monospace", "Condensed"),
    Source = c("System (macOS)", "System (macOS)", "Google Fonts", "Google Fonts", "Google Fonts"),
    Available = c(
      "Avenir" %in% available_system,
      "Helvetica Neue" %in% available_system,
      !is.null(google_status[["Lato"]]),
      !is.null(google_status[["Fira Code"]]),
      !is.null(google_status[["Roboto Condensed"]])
    ),
    stringsAsFactors = FALSE
  )

  return(all_fonts)
}
