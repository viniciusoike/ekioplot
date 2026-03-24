function(input, output, session) {
  # ---- Reactive palette ----

  current_colors <- reactiveVal(default_palettes$contrast)

  output$color_inputs <- renderUI({
    n <- input$n_colors
    cols <- current_colors()
    if (length(cols) < n) {
      cols <- c(cols, rep("#718096", n - length(cols)))
    }
    if (length(cols) > n) {
      cols <- cols[seq_len(n)]
    }

    lapply(seq_len(n), function(i) {
      fluidRow(
        column(
          9,
          textInput(
            paste0("color_", i),
            paste("Color", i),
            value = cols[i]
          )
        ),
        column(
          3,
          div(
            style = "padding-top: 25px;",
            tags$input(
              type = "color",
              value = cols[i],
              id = paste0("picker_", i),
              style = "width: 100%; height: 34px; border: none; cursor: pointer;",
              onchange = sprintf(
                "Shiny.setInputValue('color_%d', this.value, {priority: 'event'})",
                i
              )
            )
          )
        )
      )
    })
  })

  # Watch color inputs
  observe({
    n <- input$n_colors
    cols <- vapply(
      seq_len(n),
      function(i) {
        val <- input[[paste0("color_", i)]]
        if (is.null(val) || val == "") "#718096" else val
      },
      character(1)
    )
    if (length(cols) > 0) current_colors(cols)
  })

  # ---- Preset loading ----

  observeEvent(input$preset, {
    if (input$preset != "") {
      pal <- default_palettes[[input$preset]]
      current_colors(pal)
      updateNumericInput(session, "n_colors", value = length(pal))
      updateTextInput(session, "palette_name", value = input$preset)
    }
  })

  observeEvent(input$preset_2, {
    current_colors(default_palettes$binary)
    updateNumericInput(session, "n_colors", value = 2)
  })

  observeEvent(input$preset_3, {
    current_colors(default_palettes$trio)
    updateNumericInput(session, "n_colors", value = 3)
  })

  observeEvent(input$preset_4, {
    current_colors(default_palettes$quad)
    updateNumericInput(session, "n_colors", value = 4)
  })

  observeEvent(input$preset_5, {
    current_colors(default_palettes$five)
    updateNumericInput(session, "n_colors", value = 5)
  })

  observeEvent(input$preset_6, {
    current_colors(default_palettes$six)
    updateNumericInput(session, "n_colors", value = 6)
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

  # ---- Palette strip ----

  output$palette_strip <- renderUI({
    cols <- current_colors()
    swatches <- lapply(cols, function(col) {
      tags$div(
        class = "color-swatch",
        style = paste0("background-color: ", col, ";"),
        title = col
      )
    })
    do.call(tagList, swatches)
  })

  # ---- Luminance info ----

  output$luminance_info <- renderUI({
    cols <- current_colors()
    rgb_vals <- col2rgb(cols)
    lum <- round(
      rgb_vals[1, ] * 0.299 + rgb_vals[2, ] * 0.587 + rgb_vals[3, ] * 0.114
    )
    info <- paste0(cols, " (L:", lum, ")")
    tags$small(
      style = "color: #718096;",
      paste("Luminance:", paste(info, collapse = "  \u2022  "))
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

  # ---- Fuel data reactive (subset to palette size) ----

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

  # ---- Bar data reactive (subset to palette size) ----

  bar_data <- reactive({
    n <- min(length(current_colors()), nrow(diamond_cuts))
    diamond_cuts |> tail(n)
  })

  # ---- Area plots ----

  output$area_stacked <- renderPlot({
    plot_area_stacked(fuel_data(), current_colors())
  })

  output$area_share <- renderPlot({
    plot_area_share(fuel_data(), current_colors())
  })

  # ---- Line plots ----

  output$line_labeled <- renderPlot({
    plot_line_labeled(fuel_data(), current_colors())
  })

  output$line_faceted <- renderPlot({
    plot_line_faceted(fuel_data(), current_colors())
  })

  output$line_single <- renderPlot({
    plot_line_single(total_fuel, current_colors())
  })

  output$line_trend <- renderPlot({
    plot_line_single_trend(co2_data, current_colors())
  })

  output$line_trend_dots <- renderPlot({
    plot_line_single_trend_dots(co2_data, current_colors())
  })

  # ---- Bar plots ----

  output$bar_vertical <- renderPlot({
    plot_bar_vertical(bar_data(), current_colors())
  })

  output$bar_horizontal <- renderPlot({
    plot_bar_horizontal(bar_data(), current_colors())
  })

  output$bar_labels_inside <- renderPlot({
    plot_bar_labels_inside(bar_data(), current_colors())
  })

  output$bar_highlighted <- renderPlot({
    plot_bar_highlighted(bar_data(), highlight_color(), non_highlight_color())
  })

  output$bar_highlighted_top <- renderPlot({
    plot_bar_highlighted_top(bar_data(), highlight_color(), non_highlight_color())
  })

  # ---- Scatter plots ----

  output$scatter_single <- renderPlot({
    plot_scatter_single(scatter_df, current_colors())
  })

  output$scatter_grouped <- renderPlot({
    plot_scatter_grouped(scatter_df, current_colors())
  })

  # ---- Histogram ----

  output$hist_plot <- renderPlot({
    plot_histogram(current_colors())
  })

  output$hist_fine <- renderPlot({
    plot_histogram_fine(current_colors())
  })

  output$hist_density <- renderPlot({
    plot_histogram_density(current_colors())
  })

  output$hist_by_cut <- renderPlot({
    plot_histogram_by_cut(current_colors())
  })

  # ---- Complex plots ----

  output$bubble_plot <- renderPlot({
    plot_bubble(bubble_data, current_colors())
  })

  output$bump_plot <- renderPlot({
    plot_bump(
      bump_list$ranking, bump_list$df_gdp,
      bump_list$measures, bump_list$countries_sel,
      current_colors()
    )
  })

  output$pyramid_plot <- renderPlot({
    plot_pyramid(pyramid_data, current_colors())
  })

  # ---- All Charts (overview) ----

  output$all_area <- renderPlot({
    plot_mini_area(fuel_data(), current_colors())
  })

  output$all_line <- renderPlot({
    plot_mini_line(fuel_data(), current_colors())
  })

  output$all_bar <- renderPlot({
    plot_mini_bar(bar_data(), current_colors())
  })

  output$all_scatter <- renderPlot({
    plot_mini_scatter(scatter_df, current_colors())
  })
}
