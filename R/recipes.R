# ---- Internal Aesthetic Detection ----

.is_valid_color <- function(x) {
  if (!is.character(x) || length(x) != 1) {
    return(FALSE)
  }
  if (grepl("^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{8}|[A-Fa-f0-9]{3})$", x)) {
    return(TRUE)
  }
  x %in% grDevices::colors()
}

# ggplot2 resolves these at build time, so they cannot be evaluated against
# the data here. Treat them as mappings of unknown type.
.is_delayed_aes <- function(expr) {
  any(all.names(expr) %in% c("after_stat", "after_scale", "stage"))
}

.detect_aesthetic_type <- function(
  quo,
  data = NULL,
  arg_name = "colour",
  call = rlang::caller_env()
) {
  if (rlang::quo_is_null(quo)) {
    return(list(type = "missing"))
  }

  expr <- rlang::quo_get_expr(quo)

  if (is.character(expr) && length(expr) == 1) {
    if (.is_valid_color(expr)) {
      return(list(type = "static_color", value = expr))
    }
    cli::cli_abort(
      "{.val {expr}} is not a valid color. Use a column name or valid color string.",
      call = call
    )
  }

  is_continuous <- FALSE
  if (!is.null(data) && !.is_delayed_aes(expr)) {
    # Failing here means a bad column name: report it now rather than letting
    # ggplot2 raise "object not found" from deep inside the build.
    values <- tryCatch(
      rlang::eval_tidy(quo, rlang::as_data_mask(data)),
      error = function(cnd) {
        label <- rlang::as_label(expr)
        cli::cli_abort(
          c(
            "Can't evaluate {.arg {arg_name}} = {.code {label}}.",
            "i" = "Use a column of {.arg data} or a valid color string."
          ),
          parent = cnd,
          call = call
        )
      }
    )
    is_continuous <- is.numeric(values) && !is.factor(values)
  }

  list(type = "variable_mapping", is_continuous = is_continuous)
}

# A continuous mapping needs a ramp, so the categorical default only applies
# to discrete mappings.
.default_palette <- function(palette, aesthetic_type) {
  if (!is.null(palette)) {
    return(palette)
  }
  if (isTRUE(aesthetic_type$is_continuous)) "blue" else "full"
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

# A continuous color or fill reads well on a scatter plot and poorly on the
# charts that group data into bins, bars or bands. Each recipe declares its
# policy here instead of branching ad hoc.
.check_continuous <- function(aesthetic_type, arg_name, continuous, call) {
  if (continuous == "allow" || !isTRUE(aesthetic_type$is_continuous)) {
    return(invisible(NULL))
  }

  if (continuous == "reject") {
    cli::cli_abort(
      c(
        "{.arg {arg_name}} must map a discrete variable, not a continuous one.",
        "i" = "Bin the variable or wrap it in {.fn factor}."
      ),
      call = call
    )
  }

  cli::cli_warn(c(
    "{.arg {arg_name}} maps a continuous variable.",
    "i" = "A discrete mapping is usually easier to read: bin the variable or
           wrap it in {.fn factor}."
  ))
  invisible(NULL)
}

# Single entry point for the color/fill argument every recipe shares: detect
# the type, apply the recipe's continuous policy, and settle the palette.
.resolve_aes <- function(
  quo,
  data,
  palette,
  arg_name,
  continuous = c("allow", "warn", "reject"),
  call = rlang::caller_env()
) {
  continuous <- match.arg(continuous)

  aesthetic_type <- .detect_aesthetic_type(quo, data, arg_name, call)
  .check_continuous(aesthetic_type, arg_name, continuous, call)
  .warn_palette_ignored(aesthetic_type, palette, arg_name)

  aesthetic_type$quo <- quo
  aesthetic_type$palette <- .default_palette(palette, aesthetic_type)
  aesthetic_type
}

# ---- Internal Layer Builder ----

# Later entries win, so a user's `...` overrides a recipe default of the same
# name instead of tripping geom_*()'s duplicate-argument error.
.merge_args <- function(...) {
  args <- c(...)
  arg_names <- rlang::names2(args)
  arg_names[arg_names == "color"] <- "colour"
  names(args) <- arg_names
  args[arg_names == "" | !duplicated(arg_names, fromLast = TRUE)]
}

.ekio_scale <- function(aes_name, is_continuous, palette) {
  scale <- switch(
    aes_name,
    fill = if (is_continuous) scale_fill_ekio_c else scale_fill_ekio_d,
    if (is_continuous) scale_color_ekio_c else scale_color_ekio_d
  )
  scale(palette = palette)
}

# Builds the ggplot + geom for one color/fill-aware recipe and attaches the
# scale the detected aesthetic type calls for. `base_aes` is the mapping every
# arm shares (x, y, size); `aes_name` is "colour" or "fill"; `aesthetic` is
# the list returned by .resolve_aes(). Geom arguments are layered: `geom_args`
# apply to every arm, `mapped_args` only to the variable-mapping arm, and
# `user_args` (the recipe's `...`) override both.
.recipe_layer <- function(
  data,
  base_aes,
  aes_name,
  aesthetic,
  geom,
  geom_args = list(),
  mapped_args = list(),
  user_args = list()
) {
  mapped <- aesthetic$type == "variable_mapping"

  if (mapped) {
    base_aes[[aes_name]] <- aesthetic$quo
    args <- .merge_args(geom_args, mapped_args, user_args)
  } else {
    defaults <- .merge_args(geom_args)
    defaults[[aes_name]] <- if (aesthetic$type == "static_color") {
      aesthetic$value
    } else {
      .ekio("blue", 700)
    }
    args <- .merge_args(defaults, user_args)
  }

  p <- ggplot2::ggplot(data, ggplot2::aes(!!!base_aes)) +
    rlang::exec(geom, !!!args)

  if (mapped) {
    p <- p + .ekio_scale(aes_name, aesthetic$is_continuous, aesthetic$palette)
  }

  p
}

# ---- Recipe Functions ----

#' EKIO Histogram
#'
#' Professional histogram with smart aesthetic detection.
#'
#' @param data A data frame
#' @param x Variable to plot (supports data-masking)
#' @param fill Fill aesthetic. A color string or a discrete variable. NULL uses
#'   EKIO blue. A continuous variable is an error: bin it or wrap it in
#'   `factor()`.
#' @param palette Character. Palette name for variable mappings.
#' @param bins Binning method: "sturges", "FD", "scott", or numeric.
#' @param binwidth Width of bins (overrides bins if specified)
#' @param border_color Color for histogram outline (default: "white")
#' @param title,subtitle,caption Plot labels. NULL (default) draws none.
#' @param ... Additional arguments passed to [ggplot2::geom_histogram()].
#'   These override the recipe's own geom defaults.
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' \dontshow{.op <- options(ekioplot.font_title = "serif", ekioplot.font_text = "sans")}
#' ekio_histogram(mtcars, mpg)
#' ekio_histogram(mtcars, mpg, fill = "steelblue")
#' ekio_histogram(mtcars, mpg, fill = factor(cyl), palette = "full")
#' \dontshow{options(.op)}
ekio_histogram <- function(
  data,
  x,
  fill = NULL,
  palette = NULL,
  bins = "sturges",
  binwidth = NULL,
  border_color = "white",
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ...
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame")
  }

  x_var <- rlang::enquo(x)
  fill_aes <- .resolve_aes(
    rlang::enquo(fill),
    data,
    palette,
    "fill",
    continuous = "reject"
  )

  # Bin calculation
  x_values <- stats::na.omit(rlang::eval_tidy(x_var, data))
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

  p <- .recipe_layer(
    data = data,
    base_aes = list(x = x_var),
    aes_name = "fill",
    aesthetic = fill_aes,
    geom = ggplot2::geom_histogram,
    geom_args = list(colour = border_color, bins = n_bins, binwidth = binwidth),
    # Groups overlap in a histogram, so the mapped arm keeps them all visible
    mapped_args = list(position = "identity", alpha = 0.7),
    user_args = rlang::list2(...)
  )

  p +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    theme_ekio(grid = "y")
}

#' EKIO Line Plot
#'
#' Professional line plot with smart aesthetic detection.
#'
#' @param data A data frame
#' @param x X-axis variable (supports data-masking)
#' @param y Y-axis variable (supports data-masking)
#' @param color Color aesthetic. A color string or a discrete variable. A
#'   continuous variable is an error: bin it or wrap it in `factor()`.
#' @param palette Character. Palette name for variable mappings.
#' @param add_zero Logical. Add horizontal line at y=0 (default: FALSE)
#' @param line_width Line thickness (default: 0.8)
#' @param title,subtitle,caption Plot labels. NULL (default) draws none.
#' @param ... Additional arguments passed to [ggplot2::geom_line()].
#'   These override the recipe's own geom defaults.
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' \dontshow{.op <- options(ekioplot.font_title = "serif", ekioplot.font_text = "sans")}
#' ekio_lineplot(ggplot2::economics, date, unemploy)
#' \dontshow{options(.op)}
ekio_lineplot <- function(
  data,
  x,
  y,
  color = NULL,
  palette = NULL,
  add_zero = FALSE,
  line_width = 0.8,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ...
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame")
  }

  color_aes <- .resolve_aes(
    rlang::enquo(color),
    data,
    palette,
    "color",
    continuous = "reject"
  )

  p <- .recipe_layer(
    data = data,
    base_aes = list(x = rlang::enquo(x), y = rlang::enquo(y)),
    aes_name = "colour",
    aesthetic = color_aes,
    geom = ggplot2::geom_line,
    geom_args = list(linewidth = line_width),
    user_args = rlang::list2(...)
  )

  if (add_zero) {
    p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.8)
  }

  p +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    theme_ekio(grid = "y", ticks = "x")
}

#' EKIO Scatter Plot
#'
#' Professional scatter plot with smart aesthetic detection.
#'
#' @param data A data frame
#' @param x X-axis variable (supports data-masking)
#' @param y Y-axis variable (supports data-masking)
#' @param color Color aesthetic. A color string or a variable. A continuous
#'   variable warns and uses a sequential ramp.
#' @param size Size aesthetic (optional variable)
#' @param palette Character. Palette name for variable mappings.
#' @param add_zero Logical. Add horizontal line at y=0 (default: FALSE)
#' @param add_smooth Logical. Add smooth trend line (default: FALSE)
#' @param smooth_method Smoothing method: "lm", "gam", "loess" (default: "lm")
#' @param point_size Base point size (default: 2.5)
#' @param point_alpha Point transparency (default: 0.8)
#' @param title,subtitle,caption Plot labels. NULL (default) draws none.
#' @param ... Additional arguments passed to [ggplot2::geom_point()].
#'   These override the recipe's own geom defaults.
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' \dontshow{.op <- options(ekioplot.font_title = "serif", ekioplot.font_text = "sans")}
#' ekio_scatterplot(mtcars, wt, mpg)
#' ekio_scatterplot(mtcars, wt, mpg, color = factor(cyl))
#' \dontshow{options(.op)}
ekio_scatterplot <- function(
  data,
  x,
  y,
  color = NULL,
  size = NULL,
  palette = NULL,
  add_zero = FALSE,
  add_smooth = FALSE,
  smooth_method = "lm",
  point_size = 2.5,
  point_alpha = 0.8,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ...
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame")
  }

  size_var <- rlang::enquo(size)
  color_aes <- .resolve_aes(
    rlang::enquo(color),
    data,
    palette,
    "color",
    continuous = "warn"
  )

  base_aes <- list(x = rlang::enquo(x), y = rlang::enquo(y))
  if (rlang::quo_is_null(size_var)) {
    # Only a constant size when size is not mapped
    point_args <- list(size = point_size, alpha = point_alpha)
  } else {
    base_aes$size <- size_var
    point_args <- list(alpha = point_alpha)
  }

  p <- .recipe_layer(
    data = data,
    base_aes = base_aes,
    aes_name = "colour",
    aesthetic = color_aes,
    geom = ggplot2::geom_point,
    geom_args = point_args,
    user_args = rlang::list2(...)
  )

  if (add_zero) {
    p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.8)
  }

  if (add_smooth) {
    p <- p +
      ggplot2::geom_smooth(
        method = smooth_method,
        se = FALSE,
        color = .ekio("gray", 700),
        linewidth = 1
      )
  }

  p +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    theme_ekio(grid = "xy", ticks = "xy")
}

#' EKIO Bar Plot
#'
#' Professional bar plot with smart aesthetic detection.
#'
#' @param data A data frame
#' @param x X-axis variable (supports data-masking)
#' @param y Y-axis variable (supports data-masking)
#' @param fill Fill aesthetic. A color string or a discrete variable. A
#'   continuous variable is an error: bin it or wrap it in `factor()`.
#' @param palette Character. Palette name for variable mappings.
#' @param horizontal Logical. Create horizontal bar plot (default: FALSE)
#' @param bar_width Bar width (default: 0.8)
#' @param title,subtitle,caption Plot labels. NULL (default) draws none.
#' @param ... Additional arguments passed to [ggplot2::geom_col()].
#'   These override the recipe's own geom defaults.
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' \dontshow{.op <- options(ekioplot.font_title = "serif", ekioplot.font_text = "sans")}
#' cyl_counts <- as.data.frame(table(cyl = mtcars$cyl))
#' names(cyl_counts)[2] <- "n"
#' ekio_barplot(cyl_counts, cyl, n)
#' \dontshow{options(.op)}
ekio_barplot <- function(
  data,
  x,
  y,
  fill = NULL,
  palette = NULL,
  horizontal = FALSE,
  bar_width = 0.8,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ...
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame")
  }

  fill_aes <- .resolve_aes(
    rlang::enquo(fill),
    data,
    palette,
    "fill",
    continuous = "reject"
  )

  p <- .recipe_layer(
    data = data,
    base_aes = list(x = rlang::enquo(x), y = rlang::enquo(y)),
    aes_name = "fill",
    aesthetic = fill_aes,
    geom = ggplot2::geom_col,
    geom_args = list(width = bar_width),
    user_args = rlang::list2(...)
  )

  if (horizontal) {
    p <- p + ggplot2::coord_flip()
  }

  p +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    theme_ekio(grid = "y", ticks = "x")
}

#' EKIO Area Plot
#'
#' Professional area plot with smart aesthetic detection. Supports stacked
#' and filled (proportional) area charts.
#'
#' @param data A data frame
#' @param x X-axis variable (supports data-masking)
#' @param y Y-axis variable (supports data-masking)
#' @param fill Fill aesthetic. A color string or a discrete variable. A
#'   continuous variable is an error: bin it or wrap it in `factor()`.
#' @param palette Character. Palette name for variable mappings.
#' @param position Character. Stacking method: `"stack"` (default) or
#'   `"fill"` for proportional areas.
#' @param alpha Numeric. Fill transparency (default: 0.8).
#' @param title,subtitle,caption Plot labels. NULL (default) draws none.
#' @param ... Additional arguments passed to [ggplot2::geom_area()].
#'   These override the recipe's own geom defaults.
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' \dontshow{.op <- options(ekioplot.font_title = "serif", ekioplot.font_text = "sans")}
#' ekio_areaplot(ggplot2::economics, date, unemploy)
#'
#' # Stacked area with groups
#' data(fuels)
#' world_fuels <- fuels[fuels$entity == "World" & fuels$year >= 1950, ]
#' ekio_areaplot(world_fuels, year, consumption_gwh, fill = fuel)
#' \dontshow{options(.op)}
ekio_areaplot <- function(
  data,
  x,
  y,
  fill = NULL,
  palette = NULL,
  position = "stack",
  alpha = 1,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ...
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame")
  }

  fill_aes <- .resolve_aes(
    rlang::enquo(fill),
    data,
    palette,
    "fill",
    continuous = "reject"
  )

  p <- .recipe_layer(
    data = data,
    base_aes = list(x = rlang::enquo(x), y = rlang::enquo(y)),
    aes_name = "fill",
    aesthetic = fill_aes,
    geom = ggplot2::geom_area,
    geom_args = list(position = position, alpha = alpha),
    user_args = rlang::list2(...)
  )

  p +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    theme_ekio(grid = "y", ticks = "x")
}
