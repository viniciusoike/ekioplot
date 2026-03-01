library(shiny)
library(ggplot2)

# ---- Sample Data ----

sample_bar <- data.frame(
  group = LETTERS[1:6],
  value = c(42, 35, 28, 22, 18, 15)
)

sample_lines <- data.frame(
  x = rep(1:20, 3),
  y = c(
    cumsum(rnorm(20, 0.5, 1)),
    cumsum(rnorm(20, 0.3, 1.2)),
    cumsum(rnorm(20, 0.1, 0.8))
  ),
  group = rep(c("Series A", "Series B", "Series C"), each = 20)
)

sample_scatter <- data.frame(
  x = rnorm(100),
  y = rnorm(100),
  group = sample(LETTERS[1:4], 100, replace = TRUE)
)

sample_hist <- data.frame(value = c(rnorm(200, 50, 10), rnorm(150, 65, 8)))

# ---- Theme (inline minimal version for the app) ----

app_theme <- function(grid = "y") {
  grid_y <- if (grid %in% c("y", "xy")) {
    element_line(color = "#E2E8F0", linewidth = 0.4)
  } else {
    element_blank()
  }
  grid_x <- if (grid %in% c("x", "xy")) {
    element_line(color = "#E2E8F0", linewidth = 0.4)
  } else {
    element_blank()
  }

  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(size = rel(1.3), face = "bold", color = "#1A202C", hjust = 0),
      plot.subtitle = element_text(size = rel(1), color = "#718096"),
      axis.title = element_text(size = rel(0.9), color = "#4A5568"),
      axis.text = element_text(size = rel(0.85), color = "#718096"),
      panel.grid.major.y = grid_y,
      panel.grid.major.x = grid_x,
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#FFFFFF", color = NA),
      plot.background = element_rect(fill = "#FAFBFC", color = NA),
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_text(size = rel(0.9), color = "#4A5568"),
      legend.text = element_text(size = rel(0.85), color = "#718096"),
      legend.key = element_blank(),
      strip.text = element_text(face = "bold", hjust = 0),
      strip.background = element_blank(),
      plot.margin = margin(15, 15, 15, 15)
    )
}

# ---- Default Palettes ----

default_palettes <- list(
  contrast = c("#1E3A5F", "#DD6B20", "#2C7A7B", "#D69E2E", "#805AD5", "#C53030"),
  binary   = c("#1E3A5F", "#DD6B20"),
  cool     = c("#1E3A5F", "#4A90C2", "#2C7A7B"),
  trio_bold = c("#1E3A5F", "#DD6B20", "#2C7A7B"),
  quad_earth = c("#1E3A5F", "#DD6B20", "#2C7A7B", "#38A169")
)

# ---- UI ----

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background-color: #f0f2f5; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; }
    .well { background-color: #ffffff; border: 1px solid #E2E8F0; border-radius: 8px; }
    .nav-tabs > li > a { color: #4A5568; }
    .nav-tabs > li.active > a { color: #1E3A5F; font-weight: 600; }
    h3 { color: #1A202C; font-weight: 600; }
    .color-swatch { display: inline-block; width: 40px; height: 40px; border-radius: 6px;
                    border: 2px solid #E2E8F0; margin: 2px; }
    #export_code { font-family: 'Fira Code', 'Monaco', monospace; font-size: 12px;
                   background: #1A202C; color: #A8D0E8; padding: 16px; border-radius: 8px;
                   white-space: pre-wrap; }
    .btn-primary { background-color: #1E3A5F; border-color: #1E3A5F; }
    .btn-primary:hover { background-color: #2B4C7E; border-color: #2B4C7E; }
  "))),

  titlePanel(div(
    style = "padding: 10px 0;",
    span("EKIO Palette Lab", style = "font-weight: 700; color: #1A202C;"),
    span(" \u2014 test palettes on real charts", style = "color: #718096; font-size: 14px;")
  )),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Palette Editor"),

      textInput("palette_name", "Palette name", value = "my_palette"),

      numericInput("n_colors", "Number of colors", value = 4, min = 2, max = 8),

      uiOutput("color_inputs"),

      hr(),

      selectInput("preset", "Load preset",
        choices = c("(none)" = "", names(default_palettes))
      ),

      hr(),

      h4("Quick Presets"),
      fluidRow(
        column(6, actionButton("preset_2", "2 groups", class = "btn-sm btn-outline-secondary", style = "width:100%; margin-bottom:4px;")),
        column(6, actionButton("preset_3", "3 groups", class = "btn-sm btn-outline-secondary", style = "width:100%; margin-bottom:4px;"))
      ),
      fluidRow(
        column(6, actionButton("preset_4", "4 groups", class = "btn-sm btn-outline-secondary", style = "width:100%; margin-bottom:4px;")),
        column(6, actionButton("preset_6", "6 groups", class = "btn-sm btn-outline-secondary", style = "width:100%; margin-bottom:4px;"))
      ),

      hr(),

      h4("Export"),
      verbatimTextOutput("export_code"),
      actionButton("copy_btn", "Copy to clipboard", class = "btn-primary btn-sm", style = "width: 100%;")
    ),

    mainPanel(
      width = 9,

      # Palette strip preview
      fluidRow(
        column(12,
          div(style = "background: white; padding: 12px 16px; border-radius: 8px; border: 1px solid #E2E8F0; margin-bottom: 16px;",
            h5("Current Palette", style = "margin: 0 0 8px 0; color: #4A5568;"),
            uiOutput("palette_strip")
          )
        )
      ),

      # Luminance info
      fluidRow(
        column(12,
          div(style = "background: white; padding: 8px 16px; border-radius: 8px; border: 1px solid #E2E8F0; margin-bottom: 16px;",
            uiOutput("luminance_info")
          )
        )
      ),

      # Test plots
      tabsetPanel(
        tabPanel("Bar Chart",
          fluidRow(
            column(6, plotOutput("bar_plot", height = "350px")),
            column(6, plotOutput("bar_plot_horiz", height = "350px"))
          )
        ),
        tabPanel("Line Chart",
          fluidRow(
            column(6, plotOutput("line_plot_2", height = "350px")),
            column(6, plotOutput("line_plot_3", height = "350px"))
          )
        ),
        tabPanel("Scatter",
          plotOutput("scatter_plot", height = "400px")
        ),
        tabPanel("Histogram",
          plotOutput("hist_plot", height = "400px")
        ),
        tabPanel("All Charts",
          fluidRow(
            column(6, plotOutput("all_bar", height = "280px")),
            column(6, plotOutput("all_line", height = "280px"))
          ),
          fluidRow(
            column(6, plotOutput("all_scatter", height = "280px")),
            column(6, plotOutput("all_hist", height = "280px"))
          )
        )
      )
    )
  )
)

# ---- Server ----

server <- function(input, output, session) {

  # ---- Reactive palette ----
  current_colors <- reactiveVal(default_palettes$quad_earth)

  # Generate color inputs dynamically
  output$color_inputs <- renderUI({
    n <- input$n_colors
    cols <- current_colors()
    # Pad or trim colors
    if (length(cols) < n) cols <- c(cols, rep("#718096", n - length(cols)))
    if (length(cols) > n) cols <- cols[seq_len(n)]

    lapply(seq_len(n), function(i) {
      fluidRow(
        column(9,
          textInput(paste0("color_", i), paste("Color", i), value = cols[i])
        ),
        column(3,
          div(style = "padding-top: 25px;",
            tags$input(
              type = "color", value = cols[i],
              id = paste0("picker_", i),
              style = "width: 100%; height: 34px; border: none; cursor: pointer;",
              onchange = sprintf(
                "Shiny.setInputValue('color_%d', this.value, {priority: 'event'})", i
              )
            )
          )
        )
      )
    })
  })

  # Watch color inputs and update reactiveVal
  observe({
    n <- input$n_colors
    cols <- sapply(seq_len(n), function(i) {
      val <- input[[paste0("color_", i)]]
      if (is.null(val) || val == "") "#718096" else val
    })
    if (length(cols) > 0 && !any(is.null(cols))) {
      current_colors(cols)
    }
  })

  # Load preset
  observeEvent(input$preset, {
    if (input$preset != "") {
      pal <- default_palettes[[input$preset]]
      current_colors(pal)
      updateNumericInput(session, "n_colors", value = length(pal))
      updateTextInput(session, "palette_name", value = input$preset)
    }
  })

  # Quick preset buttons
  observeEvent(input$preset_2, {
    current_colors(default_palettes$binary)
    updateNumericInput(session, "n_colors", value = 2)
  })
  observeEvent(input$preset_3, {
    current_colors(default_palettes$trio_bold)
    updateNumericInput(session, "n_colors", value = 3)
  })
  observeEvent(input$preset_4, {
    current_colors(default_palettes$quad_earth)
    updateNumericInput(session, "n_colors", value = 4)
  })
  observeEvent(input$preset_6, {
    current_colors(default_palettes$contrast)
    updateNumericInput(session, "n_colors", value = 6)
  })

  # ---- Palette strip preview ----
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
    lum <- round(rgb_vals[1, ] * 0.299 + rgb_vals[2, ] * 0.587 + rgb_vals[3, ] * 0.114)
    info <- paste0(cols, " (L:", lum, ")")
    tags$small(style = "color: #718096;", paste("Luminance:", paste(info, collapse = "  \u2022  ")))
  })

  # ---- Export code ----
  output$export_code <- renderText({
    cols <- current_colors()
    name <- input$palette_name
    hex_str <- paste0('"', cols, '"', collapse = ", ")
    paste0(name, " = c(", hex_str, ")")
  })

  # Copy to clipboard via JS
  observeEvent(input$copy_btn, {
    cols <- current_colors()
    name <- input$palette_name
    hex_str <- paste0('"', cols, '"', collapse = ", ")
    code <- paste0(name, " = c(", hex_str, ")")
    session$sendCustomMessage("copy_to_clipboard", code)
  })

  # ---- Helper: make ggplot scale from current colors ----
  pal_scale_fill <- function() {
    cols <- current_colors()
    scale_fill_manual(values = cols)
  }
  pal_scale_color <- function() {
    cols <- current_colors()
    scale_color_manual(values = cols)
  }

  # ---- Bar charts ----
  output$bar_plot <- renderPlot({
    n <- min(length(current_colors()), 6)
    df <- sample_bar[seq_len(n), ]
    ggplot(df, aes(x = reorder(group, -value), y = value, fill = group)) +
      geom_col(width = 0.7) +
      pal_scale_fill() +
      labs(title = paste("Bar Chart \u2014", n, "groups"), x = NULL, y = "Value") +
      guides(fill = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      geom_hline(yintercept = 0, linewidth = 0.8) +
      app_theme()
  })

  output$bar_plot_horiz <- renderPlot({
    n <- min(length(current_colors()), 6)
    df <- sample_bar[seq_len(n), ]
    ggplot(df, aes(x = reorder(group, value), y = value, fill = group)) +
      geom_col(width = 0.7) +
      pal_scale_fill() +
      coord_flip() +
      labs(title = paste("Horizontal \u2014", n, "groups"), x = NULL, y = "Value") +
      guides(fill = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      app_theme(grid = "x")
  })

  # ---- Line charts ----
  output$line_plot_2 <- renderPlot({
    cols <- current_colors()
    df <- sample_lines[sample_lines$group %in% unique(sample_lines$group)[1:2], ]
    ggplot(df, aes(x = x, y = y, color = group)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = cols[seq_len(min(2, length(cols)))]) +
      labs(title = "Line Chart \u2014 2 series", x = "Time", y = "Value", color = NULL) +
      geom_hline(yintercept = 0, linewidth = 0.8) +
      app_theme()
  })

  output$line_plot_3 <- renderPlot({
    cols <- current_colors()
    ggplot(sample_lines, aes(x = x, y = y, color = group)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = cols[seq_len(min(3, length(cols)))]) +
      labs(title = "Line Chart \u2014 3 series", x = "Time", y = "Value", color = NULL) +
      geom_hline(yintercept = 0, linewidth = 0.8) +
      app_theme()
  })

  # ---- Scatter ----
  output$scatter_plot <- renderPlot({
    n <- min(length(current_colors()), 4)
    df <- sample_scatter[sample_scatter$group %in% LETTERS[seq_len(n)], ]
    ggplot(df, aes(x = x, y = y, color = group)) +
      geom_point(size = 3, alpha = 0.8) +
      pal_scale_color() +
      labs(title = paste("Scatter \u2014", n, "groups"), color = NULL) +
      app_theme(grid = "xy")
  })

  # ---- Histogram ----
  output$hist_plot <- renderPlot({
    cols <- current_colors()
    ggplot(sample_hist, aes(x = value)) +
      geom_histogram(fill = cols[1], color = "white", bins = 25) +
      labs(title = paste("Histogram \u2014 primary color:", cols[1]), x = "Value", y = "Count") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      geom_hline(yintercept = 0, linewidth = 0.8) +
      app_theme()
  })

  # ---- All-in-one tab ----
  output$all_bar <- renderPlot({
    n <- min(length(current_colors()), 6)
    df <- sample_bar[seq_len(n), ]
    ggplot(df, aes(x = reorder(group, -value), y = value, fill = group)) +
      geom_col(width = 0.7) +
      pal_scale_fill() +
      labs(title = paste("Bar \u2014", n, "groups"), x = NULL, y = NULL) +
      guides(fill = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      geom_hline(yintercept = 0, linewidth = 0.8) +
      app_theme()
  })

  output$all_line <- renderPlot({
    cols <- current_colors()
    ggplot(sample_lines, aes(x = x, y = y, color = group)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = cols[seq_len(min(3, length(cols)))]) +
      labs(title = "Lines", x = NULL, y = NULL, color = NULL) +
      geom_hline(yintercept = 0, linewidth = 0.8) +
      app_theme()
  })

  output$all_scatter <- renderPlot({
    n <- min(length(current_colors()), 4)
    df <- sample_scatter[sample_scatter$group %in% LETTERS[seq_len(n)], ]
    ggplot(df, aes(x = x, y = y, color = group)) +
      geom_point(size = 2.5, alpha = 0.8) +
      pal_scale_color() +
      labs(title = "Scatter", x = NULL, y = NULL, color = NULL) +
      app_theme(grid = "xy")
  })

  output$all_hist <- renderPlot({
    cols <- current_colors()
    ggplot(sample_hist, aes(x = value)) +
      geom_histogram(fill = cols[1], color = "white", bins = 25) +
      labs(title = "Histogram", x = NULL, y = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      geom_hline(yintercept = 0, linewidth = 0.8) +
      app_theme()
  })
}

# ---- Clipboard JS ----
# Inject JS for copy functionality
shinyApp(
  ui = tagList(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('copy_to_clipboard', function(text) {
        navigator.clipboard.writeText(text).then(function() {
          // Visual feedback
          var btn = document.getElementById('copy_btn');
          var orig = btn.innerHTML;
          btn.innerHTML = 'Copied!';
          setTimeout(function() { btn.innerHTML = orig; }, 1500);
        });
      });
    ")),
    ui
  ),
  server = server
)
