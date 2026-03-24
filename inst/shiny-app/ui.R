tagList(
  tags$head(tags$style(HTML(app_css))),

  tags$script(HTML(
    "
    Shiny.addCustomMessageHandler('copy_to_clipboard', function(text) {
      navigator.clipboard.writeText(text).then(function() {
        var btn = document.getElementById('copy_btn');
        var orig = btn.innerHTML;
        btn.innerHTML = 'Copied!';
        setTimeout(function() { btn.innerHTML = orig; }, 1500);
      });
    });
    "
  )),

  fluidPage(
    titlePanel(div(
      style = "padding: 10px 0;",
      span("EKIO Palette Lab", style = "font-weight: 700; color: #1A202C;"),
      span(
        " \u2014 test palettes on real charts",
        style = "color: #718096; font-size: 14px;"
      )
    )),

    sidebarLayout(
      sidebarPanel(
        width = 3,

        h4("Palette Editor"),
        textInput("palette_name", "Palette name", value = "my_palette"),
        numericInput("n_colors", "Number of colors", value = 8, min = 2, max = 8),
        uiOutput("color_inputs"),

        hr(),
        selectInput(
          "preset", "Load preset",
          choices = c("(none)" = "", names(default_palettes))
        ),

        hr(),
        h4("Quick Presets"),
        fluidRow(
          column(6, actionButton(
            "preset_2", "2 groups",
            class = "btn-sm btn-outline-secondary",
            style = "width:100%; margin-bottom:4px;"
          )),
          column(6, actionButton(
            "preset_3", "3 groups",
            class = "btn-sm btn-outline-secondary",
            style = "width:100%; margin-bottom:4px;"
          ))
        ),
        fluidRow(
          column(4, actionButton(
            "preset_4", "4 groups",
            class = "btn-sm btn-outline-secondary",
            style = "width:100%; margin-bottom:4px;"
          )),
          column(4, actionButton(
            "preset_5", "5 groups",
            class = "btn-sm btn-outline-secondary",
            style = "width:100%; margin-bottom:4px;"
          )),
          column(4, actionButton(
            "preset_6", "6 groups",
            class = "btn-sm btn-outline-secondary",
            style = "width:100%; margin-bottom:4px;"
          ))
        ),

        hr(),
        h4("Highlight Colors"),
        fluidRow(
          column(9, textInput(
            "highlight_color", "Highlight",
            value = default_palettes$contrast[1]
          )),
          column(3, div(
            style = "padding-top: 25px;",
            tags$input(
              type = "color",
              value = default_palettes$contrast[1],
              id = "picker_highlight",
              style = "width: 100%; height: 34px; border: none; cursor: pointer;",
              onchange = "Shiny.setInputValue('highlight_color', this.value, {priority: 'event'})"
            )
          ))
        ),
        fluidRow(
          column(9, textInput(
            "non_highlight_color", "Non-highlight",
            value = default_palettes$contrast[length(default_palettes$contrast)]
          )),
          column(3, div(
            style = "padding-top: 25px;",
            tags$input(
              type = "color",
              value = default_palettes$contrast[length(default_palettes$contrast)],
              id = "picker_non_highlight",
              style = "width: 100%; height: 34px; border: none; cursor: pointer;",
              onchange = "Shiny.setInputValue('non_highlight_color', this.value, {priority: 'event'})"
            )
          ))
        ),

        hr(),
        h4("Export"),
        verbatimTextOutput("export_code"),
        actionButton(
          "copy_btn", "Copy to clipboard",
          class = "btn-primary btn-sm", style = "width: 100%;"
        )
      ),

      mainPanel(
        width = 9,

        # Palette strip preview
        fluidRow(column(12, div(
          style = "background: white; padding: 12px 16px; border-radius: 8px; border: 1px solid #E2E8F0; margin-bottom: 16px;",
          h5("Current Palette", style = "margin: 0 0 8px 0; color: #4A5568;"),
          uiOutput("palette_strip")
        ))),

        # Luminance info
        fluidRow(column(12, div(
          style = "background: white; padding: 8px 16px; border-radius: 8px; border: 1px solid #E2E8F0; margin-bottom: 16px;",
          uiOutput("luminance_info")
        ))),

        # Plot tabs
        tabsetPanel(
          tabPanel(
            "Area",
            fluidRow(
              column(6, plotOutput("area_stacked", height = "380px")),
              column(6, plotOutput("area_share", height = "380px"))
            )
          ),
          tabPanel(
            "Line",
            fluidRow(
              column(6, plotOutput("line_labeled", height = "380px")),
              column(6, plotOutput("line_faceted", height = "380px"))
            ),
            fluidRow(
              column(12, plotOutput("line_single", height = "380px"))
            ),
            fluidRow(
              column(6, plotOutput("line_trend", height = "380px")),
              column(6, plotOutput("line_trend_dots", height = "380px"))
            )
          ),
          tabPanel(
            "Bar",
            fluidRow(
              column(6, plotOutput("bar_vertical", height = "380px")),
              column(6, plotOutput("bar_horizontal", height = "380px"))
            ),
            fluidRow(
              column(6, plotOutput("bar_labels_inside", height = "380px")),
              column(6, plotOutput("bar_highlighted", height = "380px"))
            ),
            fluidRow(
              column(12, plotOutput("bar_highlighted_top", height = "380px"))
            )
          ),
          tabPanel(
            "Scatter",
            fluidRow(
              column(6, plotOutput("scatter_single", height = "380px")),
              column(6, plotOutput("scatter_grouped", height = "380px"))
            )
          ),
          tabPanel(
            "Histogram",
            fluidRow(
              column(6, plotOutput("hist_plot", height = "380px")),
              column(6, plotOutput("hist_fine", height = "380px"))
            ),
            fluidRow(
              column(6, plotOutput("hist_density", height = "380px")),
              column(6, plotOutput("hist_by_cut", height = "380px"))
            )
          ),
          tabPanel(
            "Bubble",
            plotOutput("bubble_plot", height = "600px")
          ),
          tabPanel(
            "Bump",
            plotOutput("bump_plot", height = "600px")
          ),
          tabPanel(
            "Pyramid",
            plotOutput("pyramid_plot", height = "500px")
          ),
          tabPanel(
            "All Charts",
            fluidRow(
              column(6, plotOutput("all_area", height = "280px")),
              column(6, plotOutput("all_line", height = "280px"))
            ),
            fluidRow(
              column(6, plotOutput("all_bar", height = "280px")),
              column(6, plotOutput("all_scatter", height = "280px"))
            )
          )
        )
      )
    )
  )
)
