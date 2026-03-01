# ---- Internal Aesthetic Detection ----

.is_valid_color <- function(x) {
  if (!is.character(x) || length(x) != 1) return(FALSE)
  if (grepl("^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{8}|[A-Fa-f0-9]{3})$", x)) return(TRUE)
  x %in% grDevices::colors()
}

.detect_aesthetic_type <- function(quo, param_name = "parameter", data = NULL) {
  if (rlang::quo_is_null(quo)) return(list(type = "missing"))

  expr <- rlang::quo_get_expr(quo)

  if (is.character(expr) && length(expr) == 1) {
    if (.is_valid_color(expr)) {
      return(list(type = "static_color", value = expr))
    } else {
      cli::cli_abort(
        "{.val {expr}} is not a valid color. Use a column name or valid color string."
      )
    }
  }

  is_continuous <- FALSE
  if (!is.null(data)) {
    tryCatch(
      {
        var_vals <- rlang::eval_tidy(quo, rlang::as_data_mask(data))
        is_continuous <- is.numeric(var_vals) && !is.factor(var_vals)
      },
      error = function(e) is_continuous <<- FALSE
    )
  }

  list(type = "variable_mapping", is_continuous = is_continuous)
}

.warn_palette_ignored <- function(aesthetic_type, palette, param_name) {
  if (!is.null(palette) && aesthetic_type$type == "static_color") {
    cli::cli_warn(c(
      "{.arg palette} ignored when {.arg {param_name}} is a static color",
      "i" = "Remove {.code palette} or use a variable for {.arg {param_name}}"
    ))
  }
  invisible(NULL)
}

# ---- Recipe Functions ----

#' EKIO Histogram
#'
#' Professional histogram with smart aesthetic detection.
#'
#' @param data A data frame
#' @param x Variable to plot (supports data-masking)
#' @param fill Fill aesthetic. A color string or variable name. NULL uses EKIO blue.
#' @param palette Character. Palette name for variable mappings.
#' @param bins Binning method: "sturges", "FD", "scott", or numeric.
#' @param binwidth Width of bins (overrides bins if specified)
#' @param add_zero Logical. Add horizontal line at y=0 (default: TRUE)
#' @param border_color Color for histogram outline (default: "white")
#' @param ... Additional arguments passed to [ggplot2::geom_histogram()]
#'
#' @return ggplot2 object
#' @export
#'
#' @examplesIf rlang::is_interactive()
#' ekio_histogram(mtcars, mpg)
#' ekio_histogram(mtcars, mpg, fill = "steelblue")
#' ekio_histogram(mtcars, mpg, fill = factor(cyl), palette = "cool")
ekio_histogram <- function(
  data, x, fill = NULL, palette = NULL,
  bins = "sturges", binwidth = NULL,
  add_zero = TRUE, border_color = "white", ...
) {
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame")

  x_var <- rlang::enquo(x)
  fill_quo <- rlang::enquo(fill)
  fill_type <- .detect_aesthetic_type(fill_quo, "fill", data)
  .warn_palette_ignored(fill_type, palette, "fill")

  if (is.null(palette)) palette <- "contrast"

  # Bin calculation
  x_values <- stats::na.omit(data[[rlang::as_name(x_var)]])
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

  if (fill_type$type == "missing") {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var)) +
      ggplot2::geom_histogram(
        fill = ekio_blue["700"], color = border_color,
        bins = n_bins, binwidth = binwidth, ...
      )
  } else if (fill_type$type == "static_color") {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var)) +
      ggplot2::geom_histogram(
        fill = fill_type$value, color = border_color,
        bins = n_bins, binwidth = binwidth, ...
      )
  } else {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, fill = {{ fill }})) +
      ggplot2::geom_histogram(
        color = border_color, bins = n_bins, binwidth = binwidth,
        position = "identity", alpha = 0.7, ...
      )
    if (fill_type$is_continuous) {
      p <- p + scale_fill_ekio_c(palette = palette)
    } else {
      p <- p + scale_fill_ekio_d(palette = palette)
    }
  }

  if (add_zero) p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.8)

  p + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    theme_ekio(grid = "y")
}

#' EKIO Line Plot
#'
#' Professional line plot with smart aesthetic detection.
#'
#' @param data A data frame
#' @param x X-axis variable (supports data-masking)
#' @param y Y-axis variable (supports data-masking)
#' @param color Color aesthetic. A color string or variable name.
#' @param palette Character. Palette name for variable mappings.
#' @param add_zero Logical. Add horizontal line at y=0 (default: TRUE)
#' @param line_width Line thickness (default: 0.8)
#' @param ... Additional arguments passed to [ggplot2::geom_line()]
#'
#' @return ggplot2 object
#' @export
#'
#' @examplesIf rlang::is_interactive()
#' ekio_lineplot(ggplot2::economics, date, unemploy)
ekio_lineplot <- function(
  data, x, y, color = NULL, palette = NULL,
  add_zero = TRUE, line_width = 0.8, ...
) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  color_quo <- rlang::enquo(color)

  color_type <- .detect_aesthetic_type(color_quo, "color", data)
  .warn_palette_ignored(color_type, palette, "color")
  if (is.null(palette)) palette <- "contrast"

  if (color_type$type == "missing") {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var)) +
      ggplot2::geom_line(color = ekio_blue["700"], linewidth = line_width, ...)
  } else if (color_type$type == "static_color") {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var)) +
      ggplot2::geom_line(color = color_type$value, linewidth = line_width, ...)
  } else {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var, color = {{ color }})) +
      ggplot2::geom_line(linewidth = line_width, ...)
    if (color_type$is_continuous) {
      p <- p + scale_color_ekio_c(palette = palette)
    } else {
      p <- p + scale_color_ekio_d(palette = palette)
    }
  }

  if (add_zero) p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.8)

  p + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    theme_ekio()
}

#' EKIO Scatter Plot
#'
#' Professional scatter plot with smart aesthetic detection.
#'
#' @param data A data frame
#' @param x X-axis variable (supports data-masking)
#' @param y Y-axis variable (supports data-masking)
#' @param color Color aesthetic. A color string or variable name.
#' @param size Size aesthetic (optional variable)
#' @param palette Character. Palette name for variable mappings.
#' @param add_zero Logical. Add horizontal line at y=0 (default: TRUE)
#' @param add_smooth Logical. Add smooth trend line (default: FALSE)
#' @param smooth_method Smoothing method: "lm", "gam", "loess" (default: "lm")
#' @param point_size Base point size (default: 2.5)
#' @param point_alpha Point transparency (default: 0.8)
#' @param ... Additional arguments passed to [ggplot2::geom_point()]
#'
#' @return ggplot2 object
#' @export
#'
#' @examplesIf rlang::is_interactive()
#' ekio_scatterplot(mtcars, wt, mpg)
#' ekio_scatterplot(mtcars, wt, mpg, color = factor(cyl))
ekio_scatterplot <- function(
  data, x, y, color = NULL, size = NULL, palette = NULL,
  add_zero = TRUE, add_smooth = FALSE, smooth_method = "lm",
  point_size = 2.5, point_alpha = 0.8, ...
) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  color_quo <- rlang::enquo(color)
  size_var <- rlang::enquo(size)

  color_type <- .detect_aesthetic_type(color_quo, "color", data)
  .warn_palette_ignored(color_type, palette, "color")
  if (is.null(palette)) palette <- "contrast"

  has_size <- !rlang::quo_is_null(size_var)

  # Build base aesthetics
  if (color_type$type == "missing" && !has_size) {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var)) +
      ggplot2::geom_point(color = ekio_blue["700"], size = point_size, alpha = point_alpha, ...)
  } else if (color_type$type == "static_color" && !has_size) {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var)) +
      ggplot2::geom_point(color = color_type$value, size = point_size, alpha = point_alpha, ...)
  } else if (color_type$type == "variable_mapping" && !has_size) {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var, color = {{ color }})) +
      ggplot2::geom_point(size = point_size, alpha = point_alpha, ...)
    if (color_type$is_continuous) {
      p <- p + scale_color_ekio_c(palette = palette)
    } else {
      p <- p + scale_color_ekio_d(palette = palette)
    }
  } else {
    # Has size mapping
    if (color_type$type == "missing") {
      p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var, size = !!size_var)) +
        ggplot2::geom_point(color = ekio_blue["700"], alpha = point_alpha, ...)
    } else if (color_type$type == "static_color") {
      p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var, size = !!size_var)) +
        ggplot2::geom_point(color = color_type$value, alpha = point_alpha, ...)
    } else {
      p <- ggplot2::ggplot(
        data,
        ggplot2::aes(x = !!x_var, y = !!y_var, color = {{ color }}, size = !!size_var)
      ) +
        ggplot2::geom_point(alpha = point_alpha, ...)
      if (color_type$is_continuous) {
        p <- p + scale_color_ekio_c(palette = palette)
      } else {
        p <- p + scale_color_ekio_d(palette = palette)
      }
    }
  }

  if (add_zero) p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.8)

  if (add_smooth) {
    p <- p + ggplot2::geom_smooth(
      method = smooth_method, se = FALSE,
      color = ekio_gray["700"], linewidth = 1
    )
  }

  p + theme_ekio(grid = "xy")
}

#' EKIO Bar Plot
#'
#' Professional bar plot with smart aesthetic detection.
#'
#' @param data A data frame
#' @param x X-axis variable (supports data-masking)
#' @param y Y-axis variable (supports data-masking)
#' @param fill Fill aesthetic. A color string or variable name.
#' @param palette Character. Palette name for variable mappings.
#' @param add_zero Logical. Add horizontal line at y=0 (default: TRUE)
#' @param horizontal Logical. Create horizontal bar plot (default: FALSE)
#' @param bar_width Bar width (default: 0.8)
#' @param ... Additional arguments passed to [ggplot2::geom_col()]
#'
#' @return ggplot2 object
#' @export
#'
#' @examplesIf rlang::is_interactive()
#' cyl_counts <- as.data.frame(table(cyl = mtcars$cyl))
#' names(cyl_counts)[2] <- "n"
#' ekio_barplot(cyl_counts, cyl, n)
ekio_barplot <- function(
  data, x, y, fill = NULL, palette = NULL,
  add_zero = TRUE, horizontal = FALSE, bar_width = 0.8, ...
) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  fill_quo <- rlang::enquo(fill)

  fill_type <- .detect_aesthetic_type(fill_quo, "fill", data)
  .warn_palette_ignored(fill_type, palette, "fill")
  if (is.null(palette)) palette <- "contrast"

  if (fill_type$type == "missing") {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var)) +
      ggplot2::geom_col(fill = ekio_blue["700"], width = bar_width, ...)
  } else if (fill_type$type == "static_color") {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var)) +
      ggplot2::geom_col(fill = fill_type$value, width = bar_width, ...)
  } else {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var, fill = {{ fill }})) +
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

  if (horizontal) p <- p + ggplot2::coord_flip()

  p + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    theme_ekio(grid = "y")
}
