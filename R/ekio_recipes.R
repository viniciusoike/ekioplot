#' EKIO Histogram with Professional Styling
#'
#' Creates a professionally styled histogram following EKIO design principles
#'
#' @param data A data frame
#' @param x Variable to plot (supports data-masking)
#' @param bins Method for determining number of bins. Options: "sturges", "FD", "scott", or numeric value
#' @param binwidth Width of bins (overrides bins if specified)
#' @param add_zero Logical, add horizontal line at y=0 (default: TRUE)
#' @param fill_palette EKIO palette name for fill color
#' @param fill_index Index of color to use from palette (default: 7)
#' @param outline_color Color for histogram outline (default: "white")
#' @param theme_style EKIO theme style (default: "modern_premium")
#' @return ggplot2 object
#' @examples
#' library(ggplot2)
#'
#' # Basic histogram
#' ekio_histogram(mtcars, mpg)
#'
#' # With custom binning
#' ekio_histogram(mtcars, mpg, bins = "FD")
#' ekio_histogram(mtcars, mpg, binwidth = 2)
#'
#' # Custom styling
#' ekio_histogram(mtcars, mpg,
#'                fill_palette = "academic_authority",
#'                fill_index = 6)
#'
#' @export
ekio_histogram <- function(
  data,
  x,
  bins = "sturges",
  binwidth = NULL,
  add_zero = TRUE,
  fill_palette = "institutional_oxford",
  fill_index = 7,
  outline_color = "white",
  theme_style = "modern_premium"
) {
  # Capture the x variable using data-masking
  x_var <- rlang::enquo(x)

  # Extract x values for bin calculation
  x_values <- dplyr::pull(data, !!x_var)
  x_values <- stats::na.omit(x_values)

  # Determine number of bins if not using binwidth
  if (is.null(binwidth)) {
    n_bins <- switch(
      bins,
      "sturges" = nclass.Sturges(x_values),
      "FD" = nclass.FD(x_values),
      "scott" = nclass.scott(x_values),
      # If numeric value provided
      if (is.numeric(bins)) bins else nclass.Sturges(x_values)
    )
  } else {
    n_bins <- NULL
  }

  # Get fill color
  fill_color <- ekio_palette(fill_palette)[fill_index]

  # Start building the plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var))

  # Add histogram layer
  if (!is.null(binwidth)) {
    p <- p +
      ggplot2::geom_histogram(
        fill = fill_color,
        color = outline_color,
        binwidth = binwidth
      )
  } else {
    p <- p +
      ggplot2::geom_histogram(
        fill = fill_color,
        color = outline_color,
        bins = n_bins
      )
  }

  # Add zero line if requested
  if (add_zero) {
    p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.8)
  }

  # Add scales with Brazilian number formatting
  p <- p +
    ggplot2::scale_x_continuous(
      labels = scales::label_number(decimal.mark = ",", big.mark = ".")
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(decimal.mark = ",", big.mark = "."),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    )

  # Apply EKIO theme
  p <- p +
    theme_ekio(theme_style) +
    theme(
      panel.grid.major.x = element_blank()
    )

  return(p)
}

#' EKIO Line Plot with Professional Styling
#'
#' Creates a professionally styled line plot following EKIO design principles
#'
#' @param data A data frame
#' @param x X-axis variable (supports data-masking)
#' @param y Y-axis variable (supports data-masking)
#' @param color Optional grouping variable for multiple lines (supports data-masking)
#' @param add_zero Logical, add horizontal line at y=0 (default: TRUE)
#' @param palette EKIO palette name for line colors
#' @param single_color_index If no color grouping, which palette index to use (default: 9)
#' @param theme_style EKIO theme style (default: "modern_premium")
#' @param line_size Line thickness (default: 1)
#' @return ggplot2 object
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' # Single line plot
#' ekio_lineplot(economics, date, unemploy)
#'
#' # Multiple lines with grouping
#' ekio_lineplot(mtcars, wt, mpg, color = factor(cyl))
#'
#' # Custom styling
#' ekio_lineplot(economics, date, unemploy,
#'               palette = "academic_authority",
#'               single_color_index = 6)
#'
#' @export
ekio_lineplot <- function(
  data,
  x,
  y,
  color = NULL,
  add_zero = TRUE,
  palette = "categorical_extended",
  single_color_index = 9,
  theme_style = "modern_premium",
  line_size = 1
) {
  # Capture variables using data-masking
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  color_var <- rlang::enquo(color)

  # Start building the plot
  if (rlang::quo_is_null(color_var)) {
    # Single line plot
    single_color <- ekio_palette(palette)[single_color_index]
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var)) +
      ggplot2::geom_line(color = single_color, linewidth = line_size)
  } else {
    # Multiple lines with color grouping
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(x = !!x_var, y = !!y_var, color = !!color_var)
    ) +
      ggplot2::geom_line(linewidth = line_size) +
      scale_color_ekio_d(palette)
  }

  # Add zero line if requested
  if (add_zero) {
    p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.8)
  }

  # Add y-axis scale with Brazilian number formatting
  p <- p +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(decimal.mark = ",", big.mark = "."),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    )

  # Apply EKIO theme
  p <- p + theme_ekio(theme_style)

  return(p)
}

#' EKIO Scatter Plot with Professional Styling
#'
#' Creates a professionally styled scatter plot following EKIO design principles
#'
#' @param data A data frame
#' @param x X-axis variable (supports data-masking)
#' @param y Y-axis variable (supports data-masking)
#' @param color Optional grouping variable for point colors (supports data-masking)
#' @param size Optional variable for point sizes (supports data-masking)
#' @param add_zero Logical, add horizontal line at y=0 (default: TRUE)
#' @param add_smooth Logical, add smooth trend line (default: FALSE)
#' @param smooth_method Smoothing method: "lm", "gam", "loess" (default: "lm")
#' @param smooth_se Show confidence interval for smooth line (default: FALSE)
#' @param palette EKIO palette name for point colors
#' @param single_color_index If no color grouping, which palette index to use (default: 6)
#' @param point_shape Point shape (default: 21 for filled circles)
#' @param point_size Base point size (default: 2.5)
#' @param point_alpha Point transparency (default: 0.8)
#' @param outline_color Point outline color when using shape 21 (default: "gray20")
#' @param theme_style EKIO theme style (default: "modern_premium")
#' @return ggplot2 object
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' # Basic scatter plot
#' ekio_scatterplot(mtcars, wt, mpg)
#'
#' # With color grouping
#' ekio_scatterplot(mtcars, wt, mpg, color = factor(cyl))
#'
#' # With size and smooth line
#' ekio_scatterplot(mtcars, wt, mpg,
#'                  color = factor(cyl),
#'                  size = hp,
#'                  add_smooth = TRUE)
#'
#' # Custom styling
#' ekio_scatterplot(mtcars, wt, mpg,
#'                  palette = "categorical_warm",
#'                  point_shape = 19,
#'                  add_smooth = TRUE,
#'                  smooth_method = "gam")
#'
#' @export
ekio_scatterplot <- function(
  data,
  x,
  y,
  color = NULL,
  size = NULL,
  add_zero = TRUE,
  add_smooth = FALSE,
  smooth_method = "lm",
  smooth_se = FALSE,
  palette = "categorical_extended",
  single_color_index = 6,
  point_shape = 21,
  point_size = 2.5,
  point_alpha = 0.8,
  outline_color = "gray20",
  theme_style = "modern_premium"
) {
  # Capture variables using data-masking
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  color_var <- rlang::enquo(color)
  size_var <- rlang::enquo(size)

  # Start building the plot
  if (rlang::quo_is_null(color_var) && rlang::quo_is_null(size_var)) {
    # Simple scatter plot - single color
    single_color <- ekio_palette(palette)[single_color_index]
    if (point_shape == 21) {
      # Filled circles with outline
      p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var)) +
        ggplot2::geom_point(
          fill = single_color,
          color = outline_color,
          shape = point_shape,
          size = point_size,
          alpha = point_alpha
        )
    } else {
      # Solid points
      p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var)) +
        ggplot2::geom_point(
          color = single_color,
          shape = point_shape,
          size = point_size,
          alpha = point_alpha
        )
    }
  } else if (!rlang::quo_is_null(color_var) && rlang::quo_is_null(size_var)) {
    # Color grouping only
    if (point_shape == 21) {
      # Filled circles with color mapping to fill
      p <- ggplot2::ggplot(
        data,
        ggplot2::aes(x = !!x_var, y = !!y_var, fill = !!color_var)
      ) +
        ggplot2::geom_point(
          color = outline_color,
          shape = point_shape,
          size = point_size,
          alpha = point_alpha
        ) +
        scale_fill_ekio_d(palette)
    } else {
      # Color mapping to color aesthetic
      p <- ggplot2::ggplot(
        data,
        ggplot2::aes(x = !!x_var, y = !!y_var, color = !!color_var)
      ) +
        ggplot2::geom_point(
          shape = point_shape,
          size = point_size,
          alpha = point_alpha
        ) +
        scale_color_ekio_d(palette)
    }
  } else if (rlang::quo_is_null(color_var) && !rlang::quo_is_null(size_var)) {
    # Size mapping only
    single_color <- ekio_palette(palette)[single_color_index]
    if (point_shape == 21) {
      p <- ggplot2::ggplot(
        data,
        ggplot2::aes(x = !!x_var, y = !!y_var, size = !!size_var)
      ) +
        ggplot2::geom_point(
          fill = single_color,
          color = outline_color,
          shape = point_shape,
          alpha = point_alpha
        )
    } else {
      p <- ggplot2::ggplot(
        data,
        ggplot2::aes(x = !!x_var, y = !!y_var, size = !!size_var)
      ) +
        ggplot2::geom_point(
          color = single_color,
          shape = point_shape,
          alpha = point_alpha
        )
    }
  } else {
    # Both color and size mapping
    if (point_shape == 21) {
      p <- ggplot2::ggplot(
        data,
        ggplot2::aes(
          x = !!x_var,
          y = !!y_var,
          fill = !!color_var,
          size = !!size_var
        )
      ) +
        ggplot2::geom_point(
          color = outline_color,
          shape = point_shape,
          alpha = point_alpha
        ) +
        scale_fill_ekio_d(palette)
    } else {
      p <- ggplot2::ggplot(
        data,
        ggplot2::aes(
          x = !!x_var,
          y = !!y_var,
          color = !!color_var,
          size = !!size_var
        )
      ) +
        ggplot2::geom_point(
          shape = point_shape,
          alpha = point_alpha
        ) +
        scale_color_ekio_d(palette)
    }
  }

  # Add zero line if requested
  if (add_zero) {
    p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.8)
  }

  # Add smooth line if requested
  if (add_smooth) {
    smooth_color <- ekio_palette("modern_premium")[9] # Consistent smooth line color
    p <- p +
      ggplot2::geom_smooth(
        method = smooth_method,
        se = smooth_se,
        color = smooth_color,
        linewidth = 1
      )
  }

  # Add scales with Brazilian number formatting
  p <- p +
    ggplot2::scale_x_continuous(
      labels = scales::label_number(decimal.mark = ",", big.mark = ".")
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(decimal.mark = ",", big.mark = "."),
      expand = ggplot2::expansion(mult = c(0.05, 0.05))
    )

  # Apply EKIO theme
  p <- p + theme_ekio(theme_style)

  return(p)
}

#' EKIO Bar Plot with Professional Styling
#'
#' Creates a professionally styled bar plot following EKIO design principles
#'
#' @param data A data frame
#' @param x X-axis variable (supports data-masking)
#' @param y Y-axis variable (supports data-masking)
#' @param fill Optional grouping variable for bar colors (supports data-masking)
#' @param add_zero Logical, add horizontal line at y=0 (default: TRUE)
#' @param add_labels Logical, add data labels on bars (default: FALSE)
#' @param label_accuracy Number accuracy for labels (default: 1)
#' @param horizontal Logical, create horizontal bar plot (default: FALSE)
#' @param palette EKIO palette name for bar colors
#' @param single_color_index If no fill grouping, which palette index to use (default: 7)
#' @param bar_width Bar width (default: 0.8)
#' @param theme_style EKIO theme style (default: "modern_premium")
#' @return ggplot2 object
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' # Basic bar plot
#' ekio_barplot(mtcars |> count(cyl), cyl, n)
#'
#' # With fill grouping
#' ekio_barplot(mtcars |> count(cyl, gear), cyl, n, fill = factor(gear))
#'
#' # Horizontal with labels
#' ekio_barplot(mtcars |> count(cyl), cyl, n,
#'              horizontal = TRUE,
#'              add_labels = TRUE)
#'
#' # Custom styling
#' ekio_barplot(mtcars |> count(cyl), cyl, n,
#'              palette = "sophisticated_unique",
#'              single_color_index = 6,
#'              add_labels = TRUE)
#'
#' @export
ekio_barplot <- function(
  data,
  x,
  y,
  fill = NULL,
  add_zero = TRUE,
  add_labels = FALSE,
  label_accuracy = 1,
  horizontal = FALSE,
  palette = "categorical_extended",
  single_color_index = 7,
  bar_width = 0.8,
  theme_style = "modern_premium"
) {
  # Capture variables using data-masking
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  fill_var <- rlang::enquo(fill)

  # Start building the plot
  if (rlang::quo_is_null(fill_var)) {
    # Single color bar plot
    single_color <- ekio_palette(palette)[single_color_index]
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var)) +
      ggplot2::geom_col(
        fill = single_color,
        width = bar_width
      )
  } else {
    # Grouped bar plot with fill
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(x = !!x_var, y = !!y_var, fill = !!fill_var)
    ) +
      ggplot2::geom_col(width = bar_width) +
      scale_fill_ekio_d(palette)
  }

  # Add zero line if requested
  if (add_zero) {
    if (horizontal) {
      p <- p + ggplot2::geom_vline(xintercept = 0, linewidth = 0.8)
    } else {
      p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.8)
    }
  }

  # Add data labels if requested
  if (add_labels) {
    if (horizontal) {
      p <- p +
        ggplot2::geom_text(
          ggplot2::aes(
            label = scales::number(
              !!y_var,
              accuracy = label_accuracy,
              decimal.mark = ",",
              big.mark = "."
            )
          ),
          hjust = -0.1,
          size = 3,
          family = "Avenir"
        )
    } else {
      p <- p +
        ggplot2::geom_text(
          ggplot2::aes(
            label = scales::number(
              !!y_var,
              accuracy = label_accuracy,
              decimal.mark = ",",
              big.mark = "."
            )
          ),
          vjust = -0.5,
          size = 3,
          family = "Avenir"
        )
    }
  }

  # Apply horizontal transformation if requested
  if (horizontal) {
    p <- p + ggplot2::coord_flip()
  }

  # Add scales with Brazilian number formatting
  if (horizontal) {
    p <- p +
      ggplot2::scale_y_continuous(
        labels = scales::label_number(decimal.mark = ",", big.mark = "."),
        expand = ggplot2::expansion(mult = c(0, 0.1))
      )
  } else {
    p <- p +
      ggplot2::scale_y_continuous(
        labels = scales::label_number(decimal.mark = ",", big.mark = "."),
        expand = ggplot2::expansion(mult = c(0, 0.05))
      )
  }

  # Apply EKIO theme with grid adjustments
  p <- p + theme_ekio(theme_style)

  if (horizontal) {
    p <- p +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank()
      )
  } else {
    p <- p +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank()
      )
  }

  return(p)
}
