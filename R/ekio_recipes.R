# =============================================================================
# EKIO Plot Recipes - High-level plotting functions
# =============================================================================

#' EKIO Histogram
#'
#' Creates a professionally styled histogram following EKIO design principles.
#' Supports smart aesthetic detection: pass either a color string or a variable.
#'
#' @param data A data frame
#' @param x Variable to plot (supports data-masking)
#' @param fill Fill aesthetic. Can be:
#'   \itemize{
#'     \item A color string: \code{fill = "blue"} or \code{fill = "#1E3A5F"}
#'     \item A variable name: \code{fill = category} or \code{fill = factor(cyl)}
#'   }
#'   If NULL (default), uses EKIO blue.
#' @param palette Character. Palette name for variable mappings.
#'   Options: "contrast", "cool", "full", etc. See \code{\link{ekio_pal}}.
#'   Ignored when fill is a static color.
#' @param bins Method for determining number of bins: "sturges", "FD", "scott",
#'   or a numeric value.
#' @param binwidth Width of bins (overrides bins if specified)
#' @param add_zero Logical. Add horizontal line at y=0 (default: TRUE)
#' @param border_color Color for histogram outline (default: "white")
#' @param ... Additional arguments passed to \code{ggplot2::geom_histogram()}
#'
#' @return ggplot2 object
#' @export
#'
#' @examplesIf rlang::is_interactive()
#' # Basic histogram
#' ekio_histogram(mtcars, mpg)
#'
#' # With static color
#' ekio_histogram(mtcars, mpg, fill = "steelblue")
#'
#' # With variable mapping
#' ekio_histogram(mtcars, mpg, fill = factor(cyl), palette = "cool")
#'
#' # With custom binning
#' ekio_histogram(mtcars, mpg, bins = "FD")
ekio_histogram <- function(
  data,
  x,
  fill = NULL,
  palette = NULL,
  bins = "sturges",
  binwidth = NULL,
  add_zero = TRUE,
  border_color = "white",
  ...
) {
  # Input validation
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame")
  }

  # Capture variables
  x_var <- rlang::enquo(x)
  fill_quo <- rlang::enquo(fill)

  # Detect fill type (static color vs variable)
  fill_type <- detect_aesthetic_type(fill_quo, "fill", data)
  warn_palette_ignored(fill_type, palette, "fill")

  # Default palette
  if (is.null(palette)) {
    palette <- "contrast"
  }

  # Extract x values for bin calculation
  x_values <- data[[rlang::as_name(x_var)]]
  x_values <- stats::na.omit(x_values)

  # Determine number of bins
  if (is.null(binwidth)) {
    n_bins <- switch(
      as.character(bins),
      "sturges" = grDevices::nclass.Sturges(x_values),
      "FD" = grDevices::nclass.FD(x_values),
      "scott" = grDevices::nclass.scott(x_values),
      if (is.numeric(bins)) bins else grDevices::nclass.Sturges(x_values)
    )
  } else {
    n_bins <- NULL
  }

  # Build plot based on fill type
  if (fill_type$type == "missing") {
    # No fill specified - use default EKIO blue
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var)) +
      ggplot2::geom_histogram(
        fill = ekio_blue["700"],
        color = border_color,
        bins = n_bins,
        binwidth = binwidth,
        ...
      )
  } else if (fill_type$type == "static_color") {
    # Static color specified
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var)) +
      ggplot2::geom_histogram(
        fill = fill_type$value,
        color = border_color,
        bins = n_bins,
        binwidth = binwidth,
        ...
      )
  } else {
    # Variable mapping
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, fill = {{ fill }})) +
      ggplot2::geom_histogram(
        color = border_color,
        bins = n_bins,
        binwidth = binwidth,
        position = "identity",
        alpha = 0.7,
        ...
      )

    # Apply appropriate scale
    if (fill_type$is_continuous) {
      p <- p + scale_fill_ekio_c(palette = palette)
    } else {
      p <- p + scale_fill_ekio_d(palette = palette)
    }
  }

  # Add zero line if requested
  if (add_zero) {
    p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.8)
  }

  # Add scales and theme
  p <- p +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    theme_ekio(grid = "y")

  p
}

#' EKIO Line Plot
#'
#' Creates a professionally styled line plot following EKIO design principles.
#'
#' @param data A data frame
#' @param x X-axis variable (supports data-masking)
#' @param y Y-axis variable (supports data-masking)
#' @param color Color aesthetic. Can be a color string or variable name.
#' @param palette Character. Palette name for variable mappings.
#' @param add_zero Logical. Add horizontal line at y=0 (default: TRUE)
#' @param line_width Line thickness (default: 0.8)
#' @param ... Additional arguments passed to \code{ggplot2::geom_line()}
#'
#' @return ggplot2 object
#' @export
#'
#' @examplesIf rlang::is_interactive()
#' # Single line plot
#' ekio_lineplot(economics, date, unemploy)
#'
#' # Multiple lines with grouping
#' ekio_lineplot(economics_long, date, value, color = variable)
ekio_lineplot <- function(
  data,
  x,
  y,
  color = NULL,
  palette = NULL,
  add_zero = TRUE,
  line_width = 0.8,
  ...
) {
  # Capture variables
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  color_quo <- rlang::enquo(color)

  # Detect color type
  color_type <- detect_aesthetic_type(color_quo, "color", data)
  warn_palette_ignored(color_type, palette, "color")

  if (is.null(palette)) {
    palette <- "contrast"
  }

  # Build plot
  if (color_type$type == "missing") {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var)) +
      ggplot2::geom_line(color = ekio_blue["700"], linewidth = line_width, ...)
  } else if (color_type$type == "static_color") {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var)) +
      ggplot2::geom_line(color = color_type$value, linewidth = line_width, ...)
  } else {
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(x = !!x_var, y = !!y_var, color = {{ color }})
    ) +
      ggplot2::geom_line(linewidth = line_width, ...)

    if (color_type$is_continuous) {
      p <- p + scale_color_ekio_c(palette = palette)
    } else {
      p <- p + scale_color_ekio_d(palette = palette)
    }
  }

  if (add_zero) {
    p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.8)
  }

  p <- p +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    theme_ekio()

  p
}

#' EKIO Scatter Plot
#'
#' Creates a professionally styled scatter plot following EKIO design principles.
#'
#' @param data A data frame
#' @param x X-axis variable (supports data-masking)
#' @param y Y-axis variable (supports data-masking)
#' @param color Color aesthetic. Can be a color string or variable name.
#' @param size Size aesthetic (optional variable)
#' @param palette Character. Palette name for variable mappings.
#' @param add_zero Logical. Add horizontal line at y=0 (default: TRUE)
#' @param add_smooth Logical. Add smooth trend line (default: FALSE)
#' @param smooth_method Smoothing method: "lm", "gam", "loess" (default: "lm")
#' @param point_size Base point size (default: 2.5)
#' @param point_alpha Point transparency (default: 0.8)
#' @param ... Additional arguments passed to \code{ggplot2::geom_point()}
#'
#' @return ggplot2 object
#' @export
#'
#' @examplesIf rlang::is_interactive()
#' # Basic scatter plot
#' ekio_scatterplot(mtcars, wt, mpg)
#'
#' # With color grouping
#' ekio_scatterplot(mtcars, wt, mpg, color = factor(cyl))
#'
#' # With smooth line
#' ekio_scatterplot(mtcars, wt, mpg, add_smooth = TRUE)
ekio_scatterplot <- function(
  data,
  x,
  y,
  color = NULL,
  size = NULL,
  palette = NULL,
  add_zero = TRUE,
  add_smooth = FALSE,
  smooth_method = "lm",
  point_size = 2.5,
  point_alpha = 0.8,
  ...
) {
  # Capture variables
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  color_quo <- rlang::enquo(color)
  size_var <- rlang::enquo(size)

  # Detect color type
  color_type <- detect_aesthetic_type(color_quo, "color", data)
  warn_palette_ignored(color_type, palette, "color")

  if (is.null(palette)) {
    palette <- "contrast"
  }

  has_size <- !rlang::quo_is_null(size_var)

  # Build plot based on aesthetics
  if (color_type$type == "missing" && !has_size) {
    # Simple scatter
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var)) +
      ggplot2::geom_point(
        color = ekio_blue["700"],
        size = point_size,
        alpha = point_alpha,
        ...
      )
  } else if (color_type$type == "static_color" && !has_size) {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var)) +
      ggplot2::geom_point(
        color = color_type$value,
        size = point_size,
        alpha = point_alpha,
        ...
      )
  } else if (color_type$type == "variable_mapping" && !has_size) {
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(x = !!x_var, y = !!y_var, color = {{ color }})
    ) +
      ggplot2::geom_point(size = point_size, alpha = point_alpha, ...)

    if (color_type$is_continuous) {
      p <- p + scale_color_ekio_c(palette = palette)
    } else {
      p <- p + scale_color_ekio_d(palette = palette)
    }
  } else {
    # Has size mapping - build appropriately
    if (color_type$type == "missing") {
      p <- ggplot2::ggplot(
        data,
        ggplot2::aes(x = !!x_var, y = !!y_var, size = !!size_var)
      ) +
        ggplot2::geom_point(color = ekio_blue["700"], alpha = point_alpha, ...)
    } else if (color_type$type == "static_color") {
      p <- ggplot2::ggplot(
        data,
        ggplot2::aes(x = !!x_var, y = !!y_var, size = !!size_var)
      ) +
        ggplot2::geom_point(color = color_type$value, alpha = point_alpha, ...)
    } else {
      p <- ggplot2::ggplot(
        data,
        ggplot2::aes(
          x = !!x_var,
          y = !!y_var,
          color = {{ color }},
          size = !!size_var
        )
      ) +
        ggplot2::geom_point(alpha = point_alpha, ...)

      if (color_type$is_continuous) {
        p <- p + scale_color_ekio_c(palette = palette)
      } else {
        p <- p + scale_color_ekio_d(palette = palette)
      }
    }
  }

  if (add_zero) {
    p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.8)
  }

  if (add_smooth) {
    p <- p +
      ggplot2::geom_smooth(
        method = smooth_method,
        se = FALSE,
        color = ekio_gray["700"],
        linewidth = 1
      )
  }

  p <- p + theme_ekio(grid = "xy")

  p
}

#' EKIO Bar Plot
#'
#' Creates a professionally styled bar plot following EKIO design principles.
#'
#' @param data A data frame
#' @param x X-axis variable (supports data-masking)
#' @param y Y-axis variable (supports data-masking)
#' @param fill Fill aesthetic. Can be a color string or variable name.
#' @param palette Character. Palette name for variable mappings.
#' @param add_zero Logical. Add horizontal line at y=0 (default: TRUE)
#' @param horizontal Logical. Create horizontal bar plot (default: FALSE)
#' @param bar_width Bar width (default: 0.8)
#' @param ... Additional arguments passed to \code{ggplot2::geom_col()}
#'
#' @return ggplot2 object
#' @export
#'
#' @examplesIf rlang::is_interactive()
#' # Prepare summary data
#' cyl_counts <- as.data.frame(table(cyl = mtcars$cyl))
#' names(cyl_counts)[2] <- "n"
#'
#' # Basic bar plot
#' ekio_barplot(cyl_counts, cyl, n)
#'
#' # Horizontal bars
#' ekio_barplot(cyl_counts, cyl, n, horizontal = TRUE)
ekio_barplot <- function(
  data,
  x,
  y,
  fill = NULL,
  palette = NULL,
  add_zero = TRUE,
  horizontal = FALSE,
  bar_width = 0.8,
  ...
) {
  # Capture variables
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  fill_quo <- rlang::enquo(fill)

  # Detect fill type
  fill_type <- detect_aesthetic_type(fill_quo, "fill", data)
  warn_palette_ignored(fill_type, palette, "fill")

  if (is.null(palette)) {
    palette <- "contrast"
  }

  # Build plot
  if (fill_type$type == "missing") {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var)) +
      ggplot2::geom_col(fill = ekio_blue["700"], width = bar_width, ...)
  } else if (fill_type$type == "static_color") {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var)) +
      ggplot2::geom_col(fill = fill_type$value, width = bar_width, ...)
  } else {
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(x = !!x_var, y = !!y_var, fill = {{ fill }})
    ) +
      ggplot2::geom_col(width = bar_width, ...) +
      scale_fill_ekio_d(palette = palette)
  }

  if (add_zero) {
    if (horizontal) {
      p <- p + ggplot2::geom_vline(xintercept = 0, linewidth = 0.8)
    } else {
      p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.8)
    }
  }

  if (horizontal) {
    p <- p + ggplot2::coord_flip()
  }

  p <- p +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    theme_ekio(grid = "y")

  p
}
