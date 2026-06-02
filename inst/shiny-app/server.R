function(input, output, session) {

  # ---- Reactive state ----

  current_colors <- reactiveVal(default_colors)
  pinned_colors <- reactiveVal(NULL)
  pinned_name <- reactiveVal(NULL)

  # ---- Color input UI ----

  output$color_inputs <- renderUI({
    n <- input$n_colors
    cols <- current_colors()
    if (length(cols) < n) cols <- c(cols, rep("#718096", n - length(cols)))
    if (length(cols) > n) cols <- cols[seq_len(n)]

    lapply(seq_len(n), function(i) {
      fluidRow(
        column(9, textInput(
          paste0("color_", i), paste("Color", i), value = cols[i]
        )),
        column(3, tags$div(
          style = "padding-top: 25px;",
          tags$input(
            type = "color", value = cols[i],
            id = paste0("picker_", i),
            style = "width: 100%; height: 34px; border: none; cursor: pointer;",
            onchange = sprintf(
              "Shiny.setInputValue('color_%d', this.value, {priority: 'event'})", i
            )
          )
        ))
      )
    })
  })

  observe({
    n <- input$n_colors
    cols <- vapply(seq_len(n), function(i) {
      val <- input[[paste0("color_", i)]]
      if (is.null(val) || val == "") "#718096" else val
    }, character(1))
    if (length(cols) > 0) current_colors(cols)
  })

  # ---- Preset loading ----

  observeEvent(input$preset, {
    req(input$preset != "")
    pal <- ekio_pal(input$preset)
    current_colors(pal)
    updateNumericInput(session, "n_colors", value = length(pal))
    updateTextInput(session, "palette_name", value = input$preset)
  })

  # ---- Pin / unpin ----

  observeEvent(input$pin_btn, {
    if (is.null(pinned_colors())) {
      pinned_colors(current_colors())
      pinned_name(input$palette_name)
      updateActionButton(session, "pin_btn", label = "Unpin")
    } else {
      pinned_colors(NULL)
      pinned_name(NULL)
      updateActionButton(session, "pin_btn", label = "Pin palette")
    }
  })

  # ---- Highlight colors ----

  highlight_color <- reactive({
    val <- input$highlight_color
    if (is.null(val) || val == "") current_colors()[1] else val
  })

  non_highlight_color <- reactive({
    val <- input$non_highlight_color
    cols <- current_colors()
    if (is.null(val) || val == "") cols[length(cols)] else val
  })

  observeEvent(current_colors(), {
    cols <- current_colors()
    updateTextInput(session, "highlight_color", value = cols[1])
    updateTextInput(session, "non_highlight_color", value = cols[length(cols)])
  })

  # ---- Dark theme ----

  plot_theme_extra <- reactive({
    if (isTRUE(input$dark_mode)) dark_plot_theme() else theme()
  })

  # ---- Palette strip ----

  output$palette_strip <- renderUI({
    cols <- current_colors()
    swatches <- lapply(cols, function(col) {
      tags$div(
        class = "color-swatch",
        `data-color` = col,
        style = paste0("background-color: ", col, ";"),
        title = col,
        tags$span(class = "hex-tip", col)
      )
    })
    tags$div(
      id = "palette_swatches",
      style = "margin-bottom: 8px; padding-bottom: 16px; display: flex; flex-wrap: wrap; gap: 2px;",
      do.call(tagList, swatches)
    )
  })

  # ---- Reorder from drag-and-drop ----

  observeEvent(input$reordered_colors, {
    current_colors(input$reordered_colors)
  })

  output$pinned_strip <- renderUI({
    pinned <- pinned_colors()
    if (is.null(pinned)) return(NULL)
    swatches <- lapply(pinned, function(col) {
      tags$div(
        class = "color-swatch",
        style = paste0("background-color: ", col, "; opacity: 0.7;"),
        title = col,
        tags$span(class = "hex-tip", col)
      )
    })
    tagList(
      tags$div(class = "pinned-label", paste0("Pinned: ", pinned_name())),
      tags$div(style = "margin-bottom: 8px;", do.call(tagList, swatches))
    )
  })

  # ---- CVD simulation strips ----

  output$cvd_strips <- renderUI({
    cols <- current_colors()
    sims <- simulate_cvd(cols)

    rows <- lapply(names(sims), function(nm) {
      swatches <- lapply(sims[[nm]], function(col) {
        tags$div(
          class = "cvd-swatch",
          style = paste0("background-color: ", col, ";")
        )
      })
      tags$div(
        class = "cvd-row",
        tags$span(class = "cvd-label", nm),
        do.call(tagList, swatches)
      )
    })

    tags$div(style = "margin-top: 8px;", do.call(tagList, rows))
  })

  # ---- Perceptual distance bar ----

  output$distance_bar <- renderUI({
    cols <- current_colors()
    dists <- color_distances(cols)
    if (length(dists) == 0) return(NULL)

    max_dist <- max(dists, 1)
    segments <- lapply(seq_along(cols), function(i) {
      width <- if (i < length(cols)) {
        max(30, round(dists[i] / max_dist * 80))
      } else {
        30
      }
      tagList(
        tags$div(
          class = "distance-segment",
          style = paste0(
            "background-color: ", cols[i], "; width: ", width, "px;"
          )
        ),
        if (i < length(cols)) {
          tags$span(
            class = "distance-label",
            style = paste0(
              "display: inline-block; width: ",
              max(15, round(dists[i] / max_dist * 30)), "px;"
            ),
            round(dists[i])
          )
        }
      )
    })

    tags$div(
      style = "margin-top: 8px;",
      tags$small(
        style = "color: #718096; display: block; margin-bottom: 4px;",
        "Perceptual distance (CIE ΔE)"
      ),
      tags$div(
        style = "white-space: nowrap; overflow-x: auto;",
        do.call(tagList, segments)
      )
    )
  })

  # ---- Export code ----

  output$export_code <- renderText({
    cols <- current_colors()
    hex_str <- paste0('"', cols, '"', collapse = ", ")
    paste0(input$palette_name, " = c(", hex_str, ")")
  })

  observeEvent(input$copy_btn, {
    cols <- current_colors()
    hex_str <- paste0('"', cols, '"', collapse = ", ")
    code <- paste0(input$palette_name, " = c(", hex_str, ")")
    session$sendCustomMessage("copy_to_clipboard", code)
  })

  # ---- Data reactives ----

  fuel_data <- reactive({
    n <- min(length(current_colors()), 5)
    fuel_levels <- levels(subfuels$fuel)
    keep <- fuel_levels[seq_len(n)]

    subfuels |>
      filter(fuel %in% keep) |>
      mutate(
        fuel = factor(fuel, levels = keep),
        share = consumption_gwh / sum(consumption_gwh) * 100,
        .by = year
      )
  })

  bar_data <- reactive({
    n <- min(length(current_colors()), nrow(diamond_cuts))
    diamond_cuts |> tail(n)
  })

  # ---- Comparison helper ----

  render_with_compare <- function(p_current, make_pinned_fn) {
    extra <- plot_theme_extra()
    p <- p_current + extra
    pinned <- pinned_colors()
    if (is.null(pinned)) return(p)
    p_pin <- make_pinned_fn(pinned) + extra
    wrap_plots(
      p_pin + labs(subtitle = "Pinned"),
      p + labs(subtitle = "Current"),
      ncol = 2
    )
  }

  # ---- Area plots ----

  output$area_stacked <- renderPlot({
    fd <- fuel_data()
    n <- length(levels(fd$fuel))
    render_with_compare(
      plot_area_stacked(fd, current_colors()),
      \(pin) plot_area_stacked(fd, match_palette(pin, n))
    )
  })

  output$area_share <- renderPlot({
    fd <- fuel_data()
    n <- length(levels(fd$fuel))
    render_with_compare(
      plot_area_share(fd, current_colors()),
      \(pin) plot_area_share(fd, match_palette(pin, n))
    )
  })

  # ---- Line plots ----

  output$line_labeled <- renderPlot({
    fd <- fuel_data()
    n <- length(levels(fd$fuel))
    render_with_compare(
      plot_line_labeled(fd, current_colors()),
      \(pin) plot_line_labeled(fd, match_palette(pin, n))
    )
  })

  output$line_faceted <- renderPlot({
    render_with_compare(
      plot_line_faceted(fuel_data(), current_colors()),
      \(pin) plot_line_faceted(fuel_data(), pin)
    )
  })

  output$line_single <- renderPlot({
    render_with_compare(
      plot_line_single(total_fuel, current_colors()),
      \(pin) plot_line_single(total_fuel, pin)
    )
  })

  output$line_trend <- renderPlot({
    render_with_compare(
      plot_line_single_trend(co2_data, current_colors()),
      \(pin) plot_line_single_trend(co2_data, pin)
    )
  })

  output$line_trend_dots <- renderPlot({
    render_with_compare(
      plot_line_single_trend_dots(co2_data, current_colors()),
      \(pin) plot_line_single_trend_dots(co2_data, pin)
    )
  })

  # ---- Bar plots ----

  output$bar_vertical <- renderPlot({
    render_with_compare(
      plot_bar_vertical(bar_data(), current_colors()),
      \(pin) plot_bar_vertical(bar_data(), pin)
    )
  })

  output$bar_horizontal <- renderPlot({
    render_with_compare(
      plot_bar_horizontal(bar_data(), current_colors()),
      \(pin) plot_bar_horizontal(bar_data(), pin)
    )
  })

  output$bar_labels_inside <- renderPlot({
    render_with_compare(
      plot_bar_labels_inside(bar_data(), current_colors()),
      \(pin) plot_bar_labels_inside(bar_data(), pin)
    )
  })

  output$bar_highlighted <- renderPlot({
    plot_bar_highlighted(
      bar_data(), highlight_color(), non_highlight_color()
    ) + plot_theme_extra()
  })

  output$bar_highlighted_top <- renderPlot({
    plot_bar_highlighted_top(
      bar_data(), highlight_color(), non_highlight_color()
    ) + plot_theme_extra()
  })

  # ---- Scatter plots ----

  output$scatter_single <- renderPlot({
    render_with_compare(
      plot_scatter_single(scatter_df, current_colors()),
      \(pin) plot_scatter_single(scatter_df, pin)
    )
  })

  output$scatter_grouped <- renderPlot({
    render_with_compare(
      plot_scatter_grouped(scatter_df, current_colors()),
      \(pin) plot_scatter_grouped(scatter_df, pin)
    )
  })

  # ---- Histograms ----

  output$hist_plot <- renderPlot({
    render_with_compare(
      plot_histogram(current_colors()),
      \(pin) plot_histogram(pin)
    )
  })

  output$hist_fine <- renderPlot({
    render_with_compare(
      plot_histogram_fine(current_colors()),
      \(pin) plot_histogram_fine(pin)
    )
  })

  output$hist_density <- renderPlot({
    render_with_compare(
      plot_histogram_density(current_colors()),
      \(pin) plot_histogram_density(pin)
    )
  })

  output$hist_by_cut <- renderPlot({
    render_with_compare(
      plot_histogram_by_cut(current_colors()),
      \(pin) plot_histogram_by_cut(pin)
    )
  })

  # ---- Continuous plots ----

  output$heatmap_plot <- renderPlot({
    render_with_compare(
      plot_heatmap(current_colors()),
      \(pin) plot_heatmap(pin)
    )
  })

  output$gradient_scatter <- renderPlot({
    render_with_compare(
      plot_gradient_scatter(scatter_df, current_colors()),
      \(pin) plot_gradient_scatter(scatter_df, pin)
    )
  })

  output$diverging_bar <- renderPlot({
    render_with_compare(
      plot_diverging_bar(current_colors()),
      \(pin) plot_diverging_bar(pin)
    )
  })

  output$correlation_plot <- renderPlot({
    render_with_compare(
      plot_correlation(current_colors()),
      \(pin) plot_correlation(pin)
    )
  })

  # ---- Complex plots ----

  output$bubble_plot <- renderPlot({
    render_with_compare(
      plot_bubble(bubble_data, current_colors()),
      \(pin) plot_bubble(bubble_data, pin)
    )
  })

  output$bump_plot <- renderPlot({
    render_with_compare(
      plot_bump(
        bump_list$ranking, bump_list$df_gdp,
        bump_list$measures, bump_list$countries_sel,
        current_colors()
      ),
      \(pin) plot_bump(
        bump_list$ranking, bump_list$df_gdp,
        bump_list$measures, bump_list$countries_sel,
        pin
      )
    )
  })

  output$pyramid_plot <- renderPlot({
    render_with_compare(
      plot_pyramid(pyramid_data, current_colors()),
      \(pin) plot_pyramid(pyramid_data, pin)
    )
  })

  # ---- Overview (no comparison, keeps it fast) ----

  output$all_area <- renderPlot({
    plot_mini_area(fuel_data(), current_colors()) + plot_theme_extra()
  })

  output$all_line <- renderPlot({
    plot_mini_line(fuel_data(), current_colors()) + plot_theme_extra()
  })

  output$all_bar <- renderPlot({
    plot_mini_bar(bar_data(), current_colors()) + plot_theme_extra()
  })

  output$all_scatter <- renderPlot({
    plot_mini_scatter(scatter_df, current_colors()) + plot_theme_extra()
  })
}
