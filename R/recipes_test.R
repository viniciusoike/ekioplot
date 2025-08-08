# EKIO Recipe Functions (Testing - Not Exported)
# These functions are in development and not yet ready for export

#' EKIO Pie Chart with Professional Styling
#'
#' Creates a professionally styled pie chart following EKIO design principles
#'
#' @param data A data frame
#' @param category Category variable (supports data-masking)
#' @param value Value variable (supports data-masking)
#' @param palette EKIO palette name for slice colors
#' @param show_labels Logical, show percentage labels (default: TRUE)
#' @param show_legend Logical, show legend (default: TRUE)
#' @param label_accuracy Accuracy for percentage labels (default: 0.1)
#' @param theme_style EKIO theme style (default: "modern_premium")
#' @return ggplot2 object
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' 
#' # Basic pie chart
#' mtcars |> count(cyl) |> ekio_pie(cyl, n)
#' 
#' # Custom styling
#' mtcars |> count(gear) |> ekio_pie(gear, n, palette = "accent_blue")
#'
ekio_pie <- function(data, category, value, 
                     palette = "categorical_extended",
                     show_labels = TRUE,
                     show_legend = TRUE,
                     label_accuracy = 0.1,
                     theme_style = "modern_premium") {
  
  # Capture variables using data-masking
  cat_var <- rlang::enquo(category)
  val_var <- rlang::enquo(value)
  
  # Calculate percentages
  plot_data <- data |>
    dplyr::mutate(
      category = !!cat_var,
      value = !!val_var,
      percentage = value / sum(value) * 100,
      ypos = cumsum(percentage) - 0.5 * percentage
    )
  
  # Create pie chart
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = "", y = percentage, fill = category)) +
    ggplot2::geom_bar(stat = "identity", width = 1) +
    ggplot2::coord_polar("y", start = 0) +
    scale_fill_ekio_d(palette) +
    theme_ekio(theme_style) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank()
    )
  
  # Add labels if requested
  if (show_labels) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(y = ypos, label = scales::percent(percentage/100, accuracy = label_accuracy, decimal.mark = ",")),
      color = "white",
      size = 3.5,
      fontface = "bold"
    )
  }
  
  # Control legend
  if (!show_legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  }
  
  return(p)
}

#' EKIO Box Plot with Professional Styling
#'
#' Creates a professionally styled box plot with optional jitter and ridges
#'
#' @param data A data frame
#' @param x X-axis variable (supports data-masking)
#' @param y Y-axis variable (supports data-masking)
#' @param fill Optional fill grouping variable (supports data-masking)
#' @param add_jitter Logical, add jittered points (default: FALSE)
#' @param add_ridges Logical, add density ridges (requires ggridges package, default: FALSE)
#' @param palette EKIO palette name for colors
#' @param single_color_index If no fill grouping, which palette index to use (default: 6)
#' @param jitter_alpha Point transparency for jitter (default: 0.4)
#' @param jitter_size Point size for jitter (default: 1)
#' @param theme_style EKIO theme style (default: "modern_premium")
#' @return ggplot2 object
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' 
#' # Basic box plot
#' ekio_boxplot(mtcars, factor(cyl), mpg)
#' 
#' # With jitter points
#' ekio_boxplot(mtcars, factor(cyl), mpg, add_jitter = TRUE)
#' 
#' # With fill grouping
#' ekio_boxplot(mtcars, factor(cyl), mpg, fill = factor(gear))
#'
ekio_boxplot <- function(data, x, y, 
                         fill = NULL,
                         add_jitter = FALSE,
                         add_ridges = FALSE,
                         palette = "categorical_extended",
                         single_color_index = 6,
                         jitter_alpha = 0.4,
                         jitter_size = 1,
                         theme_style = "modern_premium") {
  
  # Capture variables using data-masking
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  fill_var <- rlang::enquo(fill)
  
  # Check for ggridges if needed
  if (add_ridges && !requireNamespace("ggridges", quietly = TRUE)) {
    warning("ggridges package required for ridges. Install with install.packages('ggridges')")
    add_ridges <- FALSE
  }
  
  # Create base plot
  if (rlang::quo_is_null(fill_var)) {
    # Single color
    single_color <- ekio_palette(palette)[single_color_index]
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var))
    
    if (add_ridges) {
      p <- p + ggridges::geom_density_ridges(
        fill = single_color,
        alpha = 0.7,
        scale = 0.9
      )
    } else {
      p <- p + ggplot2::geom_boxplot(
        fill = single_color,
        alpha = 0.7,
        outlier.alpha = 0.6
      )
    }
  } else {
    # Grouped with fill
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var, fill = !!fill_var))
    
    if (add_ridges) {
      p <- p + ggridges::geom_density_ridges(
        alpha = 0.7,
        scale = 0.9
      ) +
      scale_fill_ekio_d(palette)
    } else {
      p <- p + ggplot2::geom_boxplot(
        alpha = 0.7,
        outlier.alpha = 0.6
      ) +
      scale_fill_ekio_d(palette)
    }
  }
  
  # Add jitter if requested
  if (add_jitter && !add_ridges) {
    p <- p + ggplot2::geom_jitter(
      width = 0.2,
      alpha = jitter_alpha,
      size = jitter_size,
      color = "gray30"
    )
  }
  
  # Add scales with Brazilian number formatting
  p <- p + 
    ggplot2::scale_y_continuous(
      labels = scales::label_number(decimal.mark = ",", big.mark = ".")
    )
  
  # Apply EKIO theme
  p <- p + theme_ekio(theme_style)
  
  return(p)
}

#' EKIO Area Plot with Professional Styling
#'
#' Creates a professionally styled area chart following EKIO design principles
#'
#' @param data A data frame
#' @param x X-axis variable (supports data-masking)
#' @param y Y-axis variable (supports data-masking)
#' @param fill Optional grouping variable for stacked areas (supports data-masking)
#' @param stacked Logical, create stacked area chart (default: TRUE)
#' @param palette EKIO palette name for area colors
#' @param single_color_index If no fill grouping, which palette index to use (default: 6)
#' @param area_alpha Area transparency (default: 0.7)
#' @param add_zero Logical, add horizontal line at y=0 (default: TRUE)
#' @param theme_style EKIO theme style (default: "modern_premium")
#' @return ggplot2 object
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' 
#' # Basic area plot
#' economics |> ekio_areaplot(date, unemploy)
#' 
#' # Stacked area plot
#' economics_long |> ekio_areaplot(date, value, fill = variable)
#'
ekio_areaplot <- function(data, x, y, 
                          fill = NULL,
                          stacked = TRUE,
                          palette = "categorical_extended",
                          single_color_index = 6,
                          area_alpha = 0.7,
                          add_zero = TRUE,
                          theme_style = "modern_premium") {
  
  # Capture variables using data-masking
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  fill_var <- rlang::enquo(fill)
  
  # Create base plot
  if (rlang::quo_is_null(fill_var)) {
    # Single area
    single_color <- ekio_palette(palette)[single_color_index]
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var)) +
      ggplot2::geom_area(
        fill = single_color,
        alpha = area_alpha
      )
  } else {
    # Multiple areas
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var, fill = !!fill_var))
    
    if (stacked) {
      p <- p + ggplot2::geom_area(
        alpha = area_alpha,
        position = "stack"
      )
    } else {
      p <- p + ggplot2::geom_area(
        alpha = area_alpha,
        position = "identity"
      )
    }
    
    p <- p + scale_fill_ekio_d(palette)
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

#' EKIO Heatmap with Professional Styling
#'
#' Creates a professionally styled heatmap following EKIO design principles
#'
#' @param data A data frame in long format
#' @param x X-axis variable (supports data-masking)
#' @param y Y-axis variable (supports data-masking)  
#' @param fill Fill variable for heatmap colors (supports data-masking)
#' @param palette EKIO palette name for colors (default: "modern_premium")
#' @param show_values Logical, show values as text (default: FALSE)
#' @param value_accuracy Accuracy for value labels (default: 0.1)
#' @param theme_style EKIO theme style (default: "modern_premium")
#' @return ggplot2 object
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' 
#' # Create sample data
#' heatmap_data <- expand.grid(
#'   x = LETTERS[1:5],
#'   y = 1:5
#' ) |> 
#' mutate(value = rnorm(25, 50, 15))
#' 
#' # Basic heatmap
#' ekio_heatmap(heatmap_data, x, y, value)
#' 
#' # With values displayed
#' ekio_heatmap(heatmap_data, x, y, value, show_values = TRUE)
#'
ekio_heatmap <- function(data, x, y, fill,
                         palette = "modern_premium",
                         show_values = FALSE,
                         value_accuracy = 0.1,
                         theme_style = "modern_premium") {
  
  # Capture variables using data-masking
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  fill_var <- rlang::enquo(fill)
  
  # Create heatmap
  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_var, y = !!y_var, fill = !!fill_var)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_ekio_c(palette) +
    theme_ekio(theme_style) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
  
  # Add value labels if requested
  if (show_values) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = scales::number(!!fill_var, accuracy = value_accuracy, decimal.mark = ",")),
      color = "white",
      size = 3,
      fontface = "bold"
    )
  }
  
  return(p)
}

#' EKIO Slope Chart for Before-After Comparisons
#'
#' Creates a professionally styled slope chart for comparing two time points
#'
#' @param data A data frame
#' @param group Grouping variable (supports data-masking)
#' @param time Time variable with exactly 2 levels (supports data-masking)
#' @param value Value variable (supports data-masking)
#' @param palette EKIO palette name for line colors
#' @param show_labels Logical, show value labels (default: TRUE)
#' @param show_change Logical, show change indicators (default: TRUE)
#' @param label_accuracy Accuracy for value labels (default: 1)
#' @param line_size Line thickness (default: 1.2)
#' @param point_size Point size (default: 3)
#' @param theme_style EKIO theme style (default: "modern_premium")
#' @return ggplot2 object
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' 
#' # Create sample before-after data
#' slope_data <- data.frame(
#'   country = rep(c("Brazil", "Argentina", "Chile"), 2),
#'   period = rep(c("Before", "After"), each = 3),
#'   gdp = c(100, 90, 80, 120, 95, 85)
#' )
#' 
#' ekio_slopechart(slope_data, country, period, gdp)
#'
ekio_slopechart <- function(data, group, time, value,
                            palette = "categorical_extended",
                            show_labels = TRUE,
                            show_change = TRUE,
                            label_accuracy = 1,
                            line_size = 1.2,
                            point_size = 3,
                            theme_style = "modern_premium") {
  
  # Capture variables using data-masking
  group_var <- rlang::enquo(group)
  time_var <- rlang::enquo(time)
  value_var <- rlang::enquo(value)
  
  # Create slope chart
  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!time_var, y = !!value_var, group = !!group_var)) +
    ggplot2::geom_line(
      ggplot2::aes(color = !!group_var),
      linewidth = line_size,
      alpha = 0.8
    ) +
    ggplot2::geom_point(
      ggplot2::aes(color = !!group_var),
      size = point_size
    ) +
    scale_color_ekio_d(palette) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(decimal.mark = ",", big.mark = ".")
    ) +
    theme_ekio(theme_style) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    )
  
  # Add value labels if requested
  if (show_labels) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = scales::number(!!value_var, accuracy = label_accuracy, decimal.mark = ",")),
      hjust = -0.2,
      size = 3,
      fontface = "bold"
    )
  }
  
  return(p)
}

#' EKIO Waterfall Chart for Cumulative Changes
#'
#' Creates a professionally styled waterfall chart for showing cumulative effects
#'
#' @param data A data frame with category and value columns
#' @param category Category variable (supports data-masking)
#' @param value Value variable (supports data-masking)
#' @param start_value Starting value for waterfall (default: 0)
#' @param palette EKIO palette name for colors
#' @param positive_color_index Index for positive values color (default: 3)
#' @param negative_color_index Index for negative values color (default: 1)
#' @param total_color_index Index for total/cumulative color (default: 6)
#' @param show_labels Logical, show value labels (default: TRUE)
#' @param label_accuracy Accuracy for value labels (default: 1)
#' @param theme_style EKIO theme style (default: "modern_premium")
#' @return ggplot2 object
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' 
#' # Create sample waterfall data
#' waterfall_data <- data.frame(
#'   category = c("Start", "Q1", "Q2", "Q3", "Q4", "End"),
#'   value = c(100, 15, -8, 12, -5, 0),
#'   type = c("total", "positive", "negative", "positive", "negative", "total")
#' )
#' 
#' ekio_waterfall(waterfall_data, category, value)
#'
ekio_waterfall <- function(data, category, value,
                           start_value = 0,
                           palette = "accent_blue",
                           positive_color_index = 3,
                           negative_color_index = 1,
                           total_color_index = 6,
                           show_labels = TRUE,
                           label_accuracy = 1,
                           theme_style = "modern_premium") {
  
  # Capture variables using data-masking
  cat_var <- rlang::enquo(category)
  val_var <- rlang::enquo(value)
  
  # Calculate waterfall positions
  plot_data <- data |>
    dplyr::mutate(
      category = !!cat_var,
      value = !!val_var,
      cumulative = cumsum(dplyr::coalesce(value, 0)),
      yend = cumulative,
      ystart = dplyr::lag(cumulative, default = start_value),
      type = dplyr::case_when(
        value > 0 ~ "positive",
        value < 0 ~ "negative",
        TRUE ~ "total"
      )
    ) |>
    dplyr::mutate(
      color_index = dplyr::case_when(
        type == "positive" ~ positive_color_index,
        type == "negative" ~ negative_color_index,
        TRUE ~ total_color_index
      )
    )
  
  # Get colors
  colors <- ekio_palette(palette)
  plot_data$fill_color <- colors[plot_data$color_index]
  
  # Create waterfall chart
  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = as.numeric(as.factor(category)) - 0.4,
        xmax = as.numeric(as.factor(category)) + 0.4,
        ymin = pmin(ystart, yend),
        ymax = pmax(ystart, yend),
        fill = I(fill_color)
      ),
      color = "white",
      linewidth = 1
    ) +
    ggplot2::scale_x_continuous(
      breaks = 1:length(unique(plot_data$category)),
      labels = unique(plot_data$category)
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(decimal.mark = ",", big.mark = ".")
    ) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.8) +
    theme_ekio(theme_style) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank()
    )
  
  # Add value labels if requested
  if (show_labels) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(
        x = as.numeric(as.factor(category)),
        y = (ystart + yend) / 2,
        label = scales::number(value, accuracy = label_accuracy, decimal.mark = ",")
      ),
      color = "white",
      size = 3.5,
      fontface = "bold"
    )
  }
  
  return(p)
}