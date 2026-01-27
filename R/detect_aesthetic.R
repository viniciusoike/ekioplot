# =============================================================================
# Aesthetic Detection Utilities
# =============================================================================
# Enables intuitive API where users can pass either:
# - A color string: fill = "blue" or fill = "#FF0000"
# - A variable name: fill = category or fill = factor(cyl)

#' Detect if Aesthetic Parameter is Static Color or Variable Mapping
#'
#' Intelligently determines whether a user-provided aesthetic parameter (color/fill)
#' is a static color string ("blue", "#FF0000") or a variable mapping (column name
#' or expression). This enables intuitive API where both use cases work naturally.
#'
#' @param quo Quosure from \code{rlang::enquo()}
#' @param param_name Character. Parameter name for error messages (e.g., "color", "fill")
#' @param data Data frame to evaluate variable in (optional). If provided, enables
#'   detection of continuous vs discrete variables.
#'
#' @return List with:
#'   \itemize{
#'     \item type: "missing", "static_color", or "variable_mapping"
#'     \item value: The static color value (if type = "static_color")
#'     \item is_continuous: Logical (if type = "variable_mapping" and data provided)
#'   }
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # In a function context:
#' my_plot <- function(data, x, y, color = NULL) {
#'   color_quo <- rlang::enquo(color)
#'   color_type <- detect_aesthetic_type(color_quo, "color", data)
#'
#'   if (color_type$type == "static_color") {
#'     geom_point(color = color_type$value)
#'   } else if (color_type$type == "variable_mapping") {
#'     geom_point(aes(color = {{ color }}))
#'   }
#' }
#' }
detect_aesthetic_type <- function(quo, param_name = "parameter", data = NULL) {
  # Check if parameter was not provided
  if (rlang::quo_is_null(quo)) {
    return(list(type = "missing"))
  }

  expr <- rlang::quo_get_expr(quo)

  # Check if it's a string literal (static color)
  if (is.character(expr) && length(expr) == 1) {
    if (is_valid_color(expr)) {
      return(list(type = "static_color", value = expr))
    } else {
      cli::cli_abort(
        "{.val {expr}} is not a valid color. Use a column name or valid color string."
      )
    }
  }

  # It's a variable mapping (symbol or expression)
  is_continuous <- FALSE

  # Detect if continuous or discrete when data is provided
  if (!is.null(data)) {
    tryCatch(
      {
        var_vals <- rlang::eval_tidy(quo, rlang::as_data_mask(data))
        is_continuous <- is.numeric(var_vals) && !is.factor(var_vals)
      },
      error = function(e) {
        # If evaluation fails, default to discrete
        is_continuous <- FALSE
      }
    )
  }

  list(
    type = "variable_mapping",
    is_continuous = is_continuous
  )
}

#' Check if String is a Valid Color
#'
#' Validates whether a string represents a valid color (hex code or R color name).
#'
#' @param x Character. String to validate.
#'
#' @return Logical. TRUE if valid color, FALSE otherwise.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' is_valid_color("blue")      # TRUE
#' is_valid_color("#FF0000")   # TRUE
#' is_valid_color("notacolor") # FALSE
#' }
is_valid_color <- function(x) {
  if (!is.character(x) || length(x) != 1) {
    return(FALSE)
  }

 # Check hex color format
  if (grepl("^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{8}|[A-Fa-f0-9]{3})$", x)) {
    return(TRUE)
  }

  # Check R color names
  x %in% grDevices::colors()
}

#' Warn if Palette Specified with Static Aesthetic
#'
#' Educates users when they specify a palette parameter but use a static color
#' instead of a variable mapping. The palette parameter only applies to variable
#' mappings (discrete or continuous scales), not static colors.
#'
#' @param aesthetic_type List returned from \code{detect_aesthetic_type()}
#' @param palette Character or NULL. The palette argument value
#' @param param_name Character. Name of the aesthetic parameter ("color" or "fill")
#'
#' @return NULL (called for side effect of warning)
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # This will warn
#' warn_palette_ignored(
#'   list(type = "static_color", value = "blue"),
#'   palette = "contrast",
#'   param_name = "fill"
#' )
#'
#' # This will NOT warn (palette is used)
#' warn_palette_ignored(
#'   list(type = "variable_mapping"),
#'   palette = "contrast",
#'   param_name = "fill"
#' )
#' }
warn_palette_ignored <- function(aesthetic_type, palette, param_name) {
  if (!is.null(palette) && aesthetic_type$type == "static_color") {
    cli::cli_warn(c(
      "{.arg palette} argument ignored when {.arg {param_name}} is a static color",
      "i" = "Remove {.code palette = \"{palette}\"} or use a variable for {.arg {param_name}}"
    ))
  }
  invisible(NULL)
}
