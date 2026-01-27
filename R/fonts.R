# =============================================================================
# EKIO Font System (Simplified)
# =============================================================================
# Simplified font management - uses system fonts with graceful fallbacks.
# No external dependencies (extrafont, showtext, sysfonts removed).

#' Get EKIO Font
#'
#' Returns the best available font for the specified type.
#' Uses simple system font detection with sensible fallbacks.
#'
#' @param type Character. Font type to retrieve:
#'   \itemize{
#'     \item "primary": Main text font (default)
#'     \item "mono": Monospace font for code
#'   }
#'
#' @return Character. Font family name to use.
#'
#' @export
#'
#' @examples
#' get_ekio_font()
#' get_ekio_font("mono")
get_ekio_font <- function(type = "primary") {

  # Font preferences by type
  preferences <- list(
    primary = c("Helvetica Neue", "Helvetica", "Arial", "sans"),
    mono = c("Fira Code", "Monaco", "Consolas", "mono")
  )

  fonts_to_try <- preferences[[type]]
  if (is.null(fonts_to_try)) {
    fonts_to_try <- preferences[["primary"]]
  }

  # On macOS, Helvetica Neue is available by default
  if (.Platform$OS.type == "unix" && Sys.info()["sysname"] == "Darwin") {
    if (type == "primary") {
      return("Helvetica Neue")
    } else if (type == "mono") {
      return("Monaco")
    }
  }

  # On Windows, Arial is available
  if (.Platform$OS.type == "windows") {
    if (type == "primary") {
      return("Arial")
    } else if (type == "mono") {
      return("Consolas")
    }
  }

  # Ultimate fallback
  if (type == "mono") {
    return("mono")
  }
  "sans"
}

#' Check Font Availability
#'
#' Reports which fonts are likely available on the current system.
#'
#' @return Named list with font information by platform.
#'
#' @export
#'
#' @examples
#' check_ekio_fonts()
check_ekio_fonts <- function() {

  platform <- Sys.info()["sysname"]

  result <- list(
    platform = platform,
    primary = get_ekio_font("primary"),
    mono = get_ekio_font("mono")
  )

  if (platform == "Darwin") {
    cli::cli_alert_info("macOS detected - using Helvetica Neue and Monaco")
  } else if (platform == "Windows") {
    cli::cli_alert_info("Windows detected - using Arial and Consolas")
  } else {
    cli::cli_alert_info("Linux/other detected - using system defaults")
  }

  cli::cli_bullets(c(
    "*" = "Primary font: {.val {result$primary}}",
    "*" = "Monospace font: {.val {result$mono}}"
  ))

  invisible(result)
}
