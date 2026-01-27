#' Detect if aesthetic parameter is static color or variable mapping
#'
#' Intelligently determines whether a user-provided aesthetic parameter (color/fill)
#' is a static color string ("blue", "#FF0000") or a variable mapping (column name
#' or expression). This enables intuitive API where both use cases work naturally.
#'
#' @param quo Quosure from rlang::enquo()
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
#' @examples
#' \dontrun{
#' # In a function context:
#' my_plot <- function(data, x, y, color = NULL) {
#'   color_quo <- rlang::enquo(color)
#'   color_type <- detect_aesthetic_type(color_quo, "color", data)
#'
#'   if (color_type$type == "static_color") {
#'     # Use static color
#'     geom_point(color = color_type$value)
#'   } else if (color_type$type == "variable_mapping") {
#'     # Use aes() mapping
#'     geom_point(aes(color = {{color}}))
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
      cli::cli_abort(c(
        "{.arg {param_name}} = {.val {expr}} is not a valid color",
        "i" = "Use a bare column name for variable mapping: {.code {param_name} = column_name}",
        "i" = "Or use a valid color name/hex code: {.code {param_name} = \"blue\"}",
        "i" = "See {.code colors()} for valid color names"
      ))
    }
  }

  # It's a variable mapping (symbol or expression)
  # Detect if continuous or discrete
  if (!is.null(data)) {
    # Try to evaluate in data context to detect variable type
    tryCatch(
      {
        var_vals <- rlang::eval_tidy(quo, rlang::as_data_mask(data))
        is_continuous <- is.numeric(var_vals) && !is.factor(var_vals)
      },
      error = function(e) {
        # If evaluation fails, default to discrete
        # This can happen with complex expressions
        is_continuous <- FALSE
      }
    )
  } else {
    is_continuous <- FALSE # Can't determine without data
  }

  return(list(
    type = "variable_mapping",
    is_continuous = is_continuous
  ))
}

#' Warn if palette specified with static aesthetic
#'
#' Educates users when they specify a palette parameter but use a static color
#' instead of a variable mapping. The palette parameter only applies to variable
#' mappings (discrete or continuous scales), not static colors.
#'
#' @param aesthetic_type List returned from detect_aesthetic_type()
#' @param palette Character or NULL. The palette argument value
#' @param param_name Character. Name of the aesthetic parameter ("color" or "fill")
#'
#' @return NULL (called for side effect of warning)
#' @keywords internal
#' @examples
#' \dontrun{
#' # This will warn
#' warn_palette_ignored(
#'   list(type = "static_color", value = "blue"),
#'   palette = "bright",
#'   param_name = "fill"
#' )
#' # Warning: `palette` argument ignored when `fill` is a static color
#'
#' # This will NOT warn (palette is used)
#' warn_palette_ignored(
#'   list(type = "variable_mapping"),
#'   palette = "bright",
#'   param_name = "fill"
#' )
#' }
warn_palette_ignored <- function(aesthetic_type, palette, param_name) {
  if (!is.null(palette) && aesthetic_type$type == "static_color") {
    cli::cli_warn(c(
      "{.arg palette} argument ignored when {.arg {param_name}} is a static color",
      "i" = "The {.arg palette} parameter only applies when {.arg {param_name}} is a variable mapping",
      "i" = "Remove {.code palette = {.val {palette}}} or use a variable for {.arg {param_name}}"
    ))
  }
}


#' Insper Histogram
#'
#' Create histograms with formal bin selection methods using Insper's visual identity.
#' Implements Sturges, Freedman-Diaconis, and Scott algorithms for optimal bin width.
#'
#' @param data A data frame containing the data to plot
#' @param x Variable for x-axis (numeric)
#' @param fill Fill aesthetic.
#'   Can be:
#'   \itemize{
#'     \item A quoted color name/hex (e.g., `"blue"`, `"#FF0000"`) for static color
#'     \item A bare column name (e.g., `factor(cyl)`) for discrete grouping
#'     \item A continuous variable (e.g., `hp`) for gradient coloring (rare for histograms)
#'   }
#'   If `NULL` (default), uses Insper red.
#' @param palette Character. Color palette name for variable mappings.
#'   Options: "categorical", "main", "bright", "reds", "teals", etc.
#'   If NULL (default), uses "categorical". Only applies to variable mappings.
#' @param bins Numeric. Number of bins. Only used when bin_method = "manual"
#' @param bin_method Character. Bin selection method: "sturges", "fd" (Freedman-Diaconis),
#'   "scott", or "manual". Default is "sturges"
#' @param border_color Character. Color for bar borders. Default is "white"
#' @param zero Logical. If TRUE, adds a horizontal line at y = 0. Default is TRUE
#' @param ... Additional arguments passed to \code{ggplot2::geom_histogram()}
#' @return A ggplot2 object
#'
#' @details
#' Bin selection methods:
#' \itemize{
#'   \item **Sturges**: \eqn{k = \lceil \log_2(n) + 1 \rceil}. Works well for normal distributions.
#'   \item **Freedman-Diaconis**: Uses IQR to determine bin width. Robust to outliers.
#'   \item **Scott**: Uses standard deviation. Optimal for normal distributions.
#'   \item **Manual**: Specify exact number of bins with the `bins` parameter.
#' }
#'
#' @examplesIf has_insper_fonts()
#' # Simple histogram with Sturges method
#' insper_histogram(mtcars, x = mpg)
#'
#' # Using Freedman-Diaconis method
#' insper_histogram(mtcars, x = mpg, bin_method = "fd")
#'
#' # Manual bin specification
#' insper_histogram(mtcars, x = mpg, bin_method = "manual", bins = 15)
#' @family plots
#' @seealso \code{\link{theme_insper}}, \code{\link{insper_density}}
#' @importFrom grDevices nclass.Sturges nclass.FD nclass.scott
#' @export
insper_histogram <- function(
  data,
  x,
  fill = NULL,
  palette = NULL,
  bins = NULL,
  bin_method = c("sturges", "fd", "scott", "manual"),
  border_color = "white",
  zero = TRUE,
  ...
) {
  # Input validation with cli
  if (!is.data.frame(data)) {
    cli::cli_abort(c(
      "{.arg data} must be a data frame",
      "x" = "You supplied an object of class {.cls {class(data)}}"
    ))
  }

  bin_method <- match.arg(bin_method)

  if (bin_method == "manual" && is.null(bins)) {
    cli::cli_abort(c(
      "{.arg bins} must be specified when bin_method = 'manual'",
      "i" = "Set a number of bins, e.g., bins = 30"
    ))
  }

  # Smart detection for fill aesthetic
  fill_quo <- rlang::enquo(fill)
  fill_type <- detect_aesthetic_type(fill_quo, "fill", data)
  warn_palette_ignored(fill_type, palette, "fill")

  # Use default palette if not specified
  if (is.null(palette)) {
    palette <- "categorical"
  }

  # Extract x values for bin calculation
  x_quo <- rlang::enquo(x)
  x_vals <- rlang::eval_tidy(x_quo, data)

  # Calculate number of bins based on method
  if (bin_method == "sturges") {
    n_bins <- nclass.Sturges(x_vals)
  } else if (bin_method == "fd") {
    n_bins <- nclass.FD(x_vals)
  } else if (bin_method == "scott") {
    n_bins <- nclass.scott(x_vals)
  } else {
    # manual
    n_bins <- bins
  }

  # Build plot based on fill type
  if (fill_type$type == "missing") {
    # No fill specified - use default Insper red
    p <- ggplot2::ggplot(data, ggplot2::aes(x = {{ x }})) +
      ggplot2::geom_histogram(
        fill = get_insper_colors("reds1"),
        color = border_color,
        bins = n_bins,
        ...
      )
  } else if (fill_type$type == "static_color") {
    # Static color specified
    p <- ggplot2::ggplot(data, ggplot2::aes(x = {{ x }})) +
      ggplot2::geom_histogram(
        fill = fill_type$value,
        color = border_color,
        bins = n_bins,
        ...
      )
  } else {
    # Variable mapping (discrete or continuous)
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(x = {{ x }}, fill = {{ fill }})
    ) +
      ggplot2::geom_histogram(
        color = border_color,
        bins = n_bins,
        position = "identity",
        alpha = 0.7,
        ...
      )

    # Apply appropriate scale
    if (fill_type$is_continuous) {
      p <- p + scale_fill_insper_c(palette = palette)
    } else {
      p <- p + scale_fill_insper_d(palette = palette)
    }
  }

  # Add line at zero if requested
  if (zero) {
    p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 1)
  }

  # Apply theme and scale
  p <- p +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    theme_insper()

  return(p)
}
