page_sidebar(
  theme = bs_theme(
    preset = "shiny",
    primary = "#1E3A5F",
    "font-family-base" = "-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif"
  ),

  title = tags$div(
    class = "d-flex align-items-center justify-content-between w-100",
    tags$div(
      tags$span("EKIO Palette Lab", style = "font-weight: 700; font-size: 20px;"),
      tags$span(
        " — compare palettes across chart types",
        style = "color: #718096; font-size: 13px;"
      )
    )
  ),

  sidebar = sidebar(
    width = 320,
    tags$head(
      tags$style(HTML(app_css)),
      tags$script(src = "https://cdn.jsdelivr.net/npm/sortablejs@1.15.6/Sortable.min.js")
    ),
    tags$script(HTML(app_js)),

    # ---- Palette editor ----
    selectInput(
      "preset", "Load palette",
      choices = c("(custom)" = "", palette_choices)
    ),
    numericInput("n_colors", "Number of colors", value = 6, min = 2, max = 12),
    textInput("palette_name", "Name", value = "contrast"),
    uiOutput("color_inputs"),

    hr(),

    # ---- Highlight ----
    tags$h6("Highlight colors", style = "color: #4A5568; font-weight: 600;"),
    fluidRow(
      column(9, textInput(
        "highlight_color", "Highlight", value = default_colors[1]
      )),
      column(3, tags$div(
        style = "padding-top: 25px;",
        tags$input(
          type = "color", value = default_colors[1],
          id = "picker_highlight",
          style = "width: 100%; height: 34px; border: none; cursor: pointer;",
          onchange = "Shiny.setInputValue('highlight_color', this.value, {priority: 'event'})"
        )
      ))
    ),
    fluidRow(
      column(9, textInput(
        "non_highlight_color", "Non-highlight",
        value = default_colors[length(default_colors)]
      )),
      column(3, tags$div(
        style = "padding-top: 25px;",
        tags$input(
          type = "color",
          value = default_colors[length(default_colors)],
          id = "picker_non_highlight",
          style = "width: 100%; height: 34px; border: none; cursor: pointer;",
          onchange = "Shiny.setInputValue('non_highlight_color', this.value, {priority: 'event'})"
        )
      ))
    ),

    hr(),

    # ---- Export ----
    tags$h6("Export", style = "color: #4A5568; font-weight: 600;"),
    verbatimTextOutput("export_code"),
    actionButton(
      "copy_btn", "Copy to clipboard",
      class = "btn-primary btn-sm", style = "width: 100%;"
    )
  ),

  # ---- Main content ----

  # Palette preview card
  card(
    card_header(
      class = "d-flex justify-content-between align-items-center",
      "Palette preview",
      tags$div(
        class = "d-flex gap-3 align-items-center",
        input_switch("dark_mode", "Dark plots", value = FALSE),
        actionButton(
          "pin_btn", "Pin palette",
          class = "btn-sm btn-outline-primary"
        )
      )
    ),
    card_body(
      uiOutput("palette_strip"),
      uiOutput("pinned_strip"),
      uiOutput("cvd_strips"),
      uiOutput("distance_bar")
    )
  ),

  # Plot tabs
  navset_card_tab(
    id = "plot_tabs",

    nav_panel(
      "Overview",
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("all_area", height = "300px"),
        plotOutput("all_line", height = "300px"),
        plotOutput("all_bar", height = "300px"),
        plotOutput("all_scatter", height = "300px")
      )
    ),

    nav_panel(
      "Area",
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("area_stacked", height = "400px"),
        plotOutput("area_share", height = "400px")
      )
    ),

    nav_panel(
      "Line",
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("line_labeled", height = "400px"),
        plotOutput("line_faceted", height = "400px")
      ),
      plotOutput("line_single", height = "400px"),
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("line_trend", height = "400px"),
        plotOutput("line_trend_dots", height = "400px")
      )
    ),

    nav_panel(
      "Bar",
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("bar_vertical", height = "400px"),
        plotOutput("bar_horizontal", height = "400px")
      ),
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("bar_labels_inside", height = "400px"),
        plotOutput("bar_highlighted", height = "400px")
      ),
      plotOutput("bar_highlighted_top", height = "400px")
    ),

    nav_panel(
      "Scatter",
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("scatter_single", height = "400px"),
        plotOutput("scatter_grouped", height = "400px")
      )
    ),

    nav_panel(
      "Histogram",
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("hist_plot", height = "400px"),
        plotOutput("hist_fine", height = "400px")
      ),
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("hist_density", height = "400px"),
        plotOutput("hist_by_cut", height = "400px")
      )
    ),

    nav_panel(
      "Continuous",
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("heatmap_plot", height = "400px"),
        plotOutput("gradient_scatter", height = "400px")
      ),
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("diverging_bar", height = "500px"),
        plotOutput("correlation_plot", height = "500px")
      )
    ),

    nav_panel(
      "Complex",
      plotOutput("bubble_plot", height = "600px"),
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("bump_plot", height = "600px"),
        plotOutput("pyramid_plot", height = "500px")
      )
    )
  )
)
