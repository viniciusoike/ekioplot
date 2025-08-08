# EKIO Advanced Recipe Functions (Testing - Not Exported)
# Additional visualization functions in development

#' EKIO Hexbin Map with Professional Styling
#'
#' Creates a professionally styled hexagonal binned map following EKIO design principles
#'
#' @param data A data frame with x, y coordinates and optional values
#' @param x X-coordinate variable (supports data-masking)
#' @param y Y-coordinate variable (supports data-masking)
#' @param z Optional value variable for coloring hexbins (supports data-masking)
#' @param bins Number of hexagonal bins (default: 30)
#' @param palette EKIO palette name for colors (default: "modern_premium")
#' @param show_points Logical, overlay original points (default: FALSE)
#' @param point_alpha Point transparency when shown (default: 0.3)
#' @param theme_style EKIO theme style (default: "modern_premium")
#' @return ggplot2 object
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' 
#' # Basic hexbin map
#' ekio_hexmap(faithful, waiting, eruptions)
#' 
#' # With point overlay
#' ekio_hexmap(faithful, waiting, eruptions, show_points = TRUE)
#'
ekio_hexmap <- function(data, x, y, z = NULL,
                        bins = 30,
                        palette = "modern_premium",
                        show_points = FALSE,
                        point_alpha = 0.3,
                        theme_style = "modern_premium") {
  
  # Check for hexbin package
  if (!requireNamespace("hexbin", quietly = TRUE)) {
    stop("hexbin package required for hexagonal binning. Install with install.packages('hexbin')")
  }
  
  # Capture variables using data-masking
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  z_var <- rlang::enquo(z)
  
  # Create base plot
  if (rlang::quo_is_null(z_var)) {
    # Count-based hexbins
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var)) +
      ggplot2::stat_binhex(bins = bins) +
      ggplot2::scale_fill_gradient(
        low = ekio_palette(palette)[1],
        high = ekio_palette(palette)[6],
        name = "Count",
        labels = scales::label_number(decimal.mark = ",", big.mark = ".")
      )
  } else {
    # Value-based hexbins
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var, z = !!z_var)) +
      ggplot2::stat_summary_hex(bins = bins, fun = mean) +
      ggplot2::scale_fill_gradient(
        low = ekio_palette(palette)[1],
        high = ekio_palette(palette)[6],
        name = "Mean",
        labels = scales::label_number(decimal.mark = ",", big.mark = ".")
      )
  }
  
  # Add original points if requested
  if (show_points) {
    p <- p + ggplot2::geom_point(
      alpha = point_alpha,
      size = 0.8,
      color = "gray30"
    )
  }
  
  # Add scales with Brazilian number formatting
  p <- p + 
    ggplot2::scale_x_continuous(
      labels = scales::label_number(decimal.mark = ",", big.mark = ".")
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(decimal.mark = ",", big.mark = ".")
    )
  
  # Apply EKIO theme
  p <- p + theme_ekio(theme_style)
  
  return(p)
}

#' EKIO Parallel Coordinates Plot with Professional Styling
#'
#' Creates parallel coordinates plot with straight lines or smooth bumps
#'
#' @param data A data frame
#' @param variables Vector of column names for parallel coordinates
#' @param group Optional grouping variable (supports data-masking)
#' @param use_bumps Logical, use ggbump for smooth curves (default: FALSE)
#' @param palette EKIO palette name for line colors
#' @param line_alpha Line transparency (default: 0.6)
#' @param line_size Line thickness (default: 0.8)
#' @param theme_style EKIO theme style (default: "modern_premium")
#' @return ggplot2 object
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' 
#' # Basic parallel coordinates
#' ekio_parallel(iris, c("Sepal.Length", "Sepal.Width", "Petal.Length"))
#' 
#' # Grouped with bumps
#' ekio_parallel(iris, c("Sepal.Length", "Sepal.Width", "Petal.Length"),
#'               group = Species, use_bumps = TRUE)
#'
ekio_parallel <- function(data, variables, group = NULL,
                          use_bumps = FALSE,
                          palette = "categorical_extended",
                          line_alpha = 0.6,
                          line_size = 0.8,
                          theme_style = "modern_premium") {
  
  # Check for ggbump if needed
  if (use_bumps && !requireNamespace("ggbump", quietly = TRUE)) {
    warning("ggbump package required for smooth bumps. Install with install.packages('ggbump'). Using straight lines instead.")
    use_bumps <- FALSE
  }
  
  # Check for tidyr
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("tidyr package required for data reshaping. Install with install.packages('tidyr')")
  }
  
  # Capture group variable
  group_var <- rlang::enquo(group)
  
  # Prepare data for parallel coordinates
  if (rlang::quo_is_null(group_var)) {
    plot_data <- data |>
      dplyr::select(dplyr::all_of(variables)) |>
      dplyr::mutate(id = dplyr::row_number()) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(variables),
        names_to = "variable",
        values_to = "value"
      ) |>
      dplyr::mutate(
        variable = factor(variable, levels = variables),
        variable_num = as.numeric(variable)
      )
  } else {
    group_name <- rlang::as_name(group_var)
    plot_data <- data |>
      dplyr::select(dplyr::all_of(variables), !!group_var) |>
      dplyr::mutate(id = dplyr::row_number()) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(variables),
        names_to = "variable",
        values_to = "value"
      ) |>
      dplyr::mutate(
        variable = factor(variable, levels = variables),
        variable_num = as.numeric(variable)
      ) |>
      dplyr::rename(group = dplyr::all_of(group_name))
  }
  
  # Create base plot
  if (rlang::quo_is_null(group_var)) {
    # No grouping
    if (use_bumps) {
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = variable_num, y = value, group = id)) +
        ggbump::geom_bump(
          alpha = line_alpha,
          size = line_size,
          color = ekio_palette(palette)[6]
        )
    } else {
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = variable_num, y = value, group = id)) +
        ggplot2::geom_line(
          alpha = line_alpha,
          linewidth = line_size,
          color = ekio_palette(palette)[6]
        )
    }
  } else {
    # With grouping
    if (use_bumps) {
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = variable_num, y = value, group = id, color = group)) +
        ggbump::geom_bump(
          alpha = line_alpha,
          size = line_size
        ) +
        scale_color_ekio_d(palette)
    } else {
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = variable_num, y = value, group = id, color = group)) +
        ggplot2::geom_line(
          alpha = line_alpha,
          linewidth = line_size
        ) +
        scale_color_ekio_d(palette)
    }
  }
  
  # Add scales and formatting
  p <- p + 
    ggplot2::scale_x_continuous(
      breaks = 1:length(variables),
      labels = variables,
      expand = ggplot2::expansion(mult = 0.05)
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(decimal.mark = ",", big.mark = ".")
    )
  
  # Apply EKIO theme
  p <- p + theme_ekio(theme_style) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(color = "gray90", linewidth = 0.5),
      panel.grid.minor.x = ggplot2::element_blank()
    )
  
  return(p)
}

#' EKIO Lollipop Plot with Professional Styling
#'
#' Creates professionally styled lollipop plots using base ggplot2
#'
#' @param data A data frame
#' @param x X-axis variable (supports data-masking)
#' @param y Y-axis variable (supports data-masking)
#' @param color Optional grouping variable for colors (supports data-masking)
#' @param horizontal Logical, create horizontal lollipops (default: FALSE)
#' @param add_zero Logical, add reference line at zero (default: TRUE)
#' @param show_labels Logical, show value labels (default: FALSE)
#' @param palette EKIO palette name for colors
#' @param single_color_index If no color grouping, which palette index to use (default: 6)
#' @param point_size Lollipop head size (default: 3)
#' @param line_size Lollipop stem thickness (default: 1)
#' @param label_accuracy Accuracy for value labels (default: 1)
#' @param theme_style EKIO theme style (default: "modern_premium")
#' @return ggplot2 object
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' 
#' # Basic lollipop plot
#' mtcars |> slice_head(n = 10) |> 
#'   ekio_lolli(reorder(rownames(.), mpg), mpg)
#' 
#' # Horizontal with colors
#' mtcars |> slice_head(n = 8) |> 
#'   ekio_lolli(reorder(rownames(.), mpg), mpg, 
#'              color = factor(cyl), horizontal = TRUE)
#'
ekio_lolli <- function(data, x, y, 
                       color = NULL,
                       horizontal = FALSE,
                       add_zero = TRUE,
                       show_labels = FALSE,
                       palette = "categorical_extended",
                       single_color_index = 6,
                       point_size = 3,
                       line_size = 1,
                       label_accuracy = 1,
                       theme_style = "modern_premium") {
  
  # Capture variables using data-masking
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  color_var <- rlang::enquo(color)
  
  # Create base plot
  if (rlang::quo_is_null(color_var)) {
    # Single color
    single_color <- ekio_palette(palette)[single_color_index]
    
    if (horizontal) {
      p <- ggplot2::ggplot(data, ggplot2::aes(x = !!y_var, y = !!x_var)) +
        ggplot2::geom_segment(
          ggplot2::aes(x = 0, xend = !!y_var, yend = !!x_var),
          color = single_color,
          linewidth = line_size
        ) +
        ggplot2::geom_point(
          color = single_color,
          size = point_size
        )
    } else {
      p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var)) +
        ggplot2::geom_segment(
          ggplot2::aes(xend = !!x_var, y = 0, yend = !!y_var),
          color = single_color,
          linewidth = line_size
        ) +
        ggplot2::geom_point(
          color = single_color,
          size = point_size
        )
    }
  } else {
    # With color grouping
    if (horizontal) {
      p <- ggplot2::ggplot(data, ggplot2::aes(x = !!y_var, y = !!x_var, color = !!color_var)) +
        ggplot2::geom_segment(
          ggplot2::aes(x = 0, xend = !!y_var, yend = !!x_var),
          linewidth = line_size
        ) +
        ggplot2::geom_point(size = point_size) +
        scale_color_ekio_d(palette)
    } else {
      p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var, color = !!color_var)) +
        ggplot2::geom_segment(
          ggplot2::aes(xend = !!x_var, y = 0, yend = !!y_var),
          linewidth = line_size
        ) +
        ggplot2::geom_point(size = point_size) +
        scale_color_ekio_d(palette)
    }
  }
  
  # Add zero reference line if requested
  if (add_zero) {
    if (horizontal) {
      p <- p + ggplot2::geom_vline(xintercept = 0, linewidth = 0.8, color = "gray50")
    } else {
      p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.8, color = "gray50")
    }
  }
  
  # Add value labels if requested
  if (show_labels) {
    if (horizontal) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = scales::number(!!y_var, accuracy = label_accuracy, decimal.mark = ",")),
        hjust = -0.2,
        size = 3,
        fontface = "bold"
      )
    } else {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = scales::number(!!y_var, accuracy = label_accuracy, decimal.mark = ",")),
        vjust = -0.5,
        size = 3,
        fontface = "bold"
      )
    }
  }
  
  # Add scales with Brazilian number formatting
  if (horizontal) {
    p <- p + 
      ggplot2::scale_x_continuous(
        labels = scales::label_number(decimal.mark = ",", big.mark = ".")
      )
  } else {
    p <- p + 
      ggplot2::scale_y_continuous(
        labels = scales::label_number(decimal.mark = ",", big.mark = ".")
      )
  }
  
  # Apply EKIO theme
  p <- p + theme_ekio(theme_style)
  
  if (horizontal) {
    p <- p + ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank()
    )
  } else {
    p <- p + ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank()
    )
  }
  
  return(p)
}

#' EKIO Radar Plot with Professional Styling
#'
#' Creates professionally styled radar plots using ggradar
#'
#' @param data A data frame with first column as group labels and numeric columns for axes
#' @param palette EKIO palette name for colors
#' @param grid_color Color for grid lines (default: "gray80")
#' @param axis_label_size Size of axis labels (default: 3)
#' @param grid_line_width Grid line thickness (default: 0.5)
#' @param show_legend Logical, show legend (default: TRUE)
#' @return ggplot2 object
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' 
#' # Create sample radar data
#' radar_data <- mtcars |>
#'   tibble::rownames_to_column("car") |>
#'   select(car, mpg, hp, wt, qsec) |>
#'   slice_head(n = 3) |>
#'   mutate(across(-car, ~ scales::rescale(.x, to = c(0, 1))))
#' 
#' ekio_radar(radar_data)
#'
ekio_radar <- function(data, 
                       palette = "categorical_extended",
                       grid_color = "gray80",
                       axis_label_size = 3,
                       grid_line_width = 0.5,
                       show_legend = TRUE) {
  
  # Check for ggradar package
  if (!requireNamespace("ggradar", quietly = TRUE)) {
    stop("ggradar package required for radar plots. Install with remotes::install_github('ricardo-bion/ggradar')")
  }
  
  # Get colors from EKIO palette
  colors <- ekio_palette(palette)[1:min(nrow(data), length(ekio_palette(palette)))]
  
  # Create radar plot using ggradar
  p <- ggradar::ggradar(
    data,
    values.radar = c("0%", "25%", "50%", "75%", "100%"),
    grid.min = 0,
    grid.mid = 0.5,
    grid.max = 1,
    group.line.width = 1.2,
    group.point.size = 2,
    group.colours = colors,
    background.circle.colour = "white",
    gridline.min.colour = grid_color,
    gridline.mid.colour = grid_color,
    gridline.max.colour = grid_color,
    gridline.min.linetype = "solid",
    gridline.mid.linetype = "dashed", 
    gridline.max.linetype = "solid",
    grid.line.width = grid_line_width,
    axis.label.size = axis_label_size,
    axis.line.colour = "gray60",
    plot.extent.x.sf = 1,
    plot.extent.y.sf = 1,
    legend.position = if(show_legend) "bottom" else "none"
  )
  
  # Apply additional EKIO styling
  p <- p +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.key = ggplot2::element_rect(fill = "white", color = NA),
      text = ggplot2::element_text(family = "Avenir", color = "#2c3e50"),
      legend.text = ggplot2::element_text(size = 10),
      legend.title = ggplot2::element_blank()
    )
  
  return(p)
}