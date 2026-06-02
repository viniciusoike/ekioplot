source("inst/shiny-app/utils.R")
source("inst/shiny-app/plot_funs.R")
library(shiny)

ui <- source("inst/shiny-app/ui.R", local = TRUE)$value
server <- source("inst/shiny-app/server.R", local = TRUE)$value

shinyApp(ui = ui, server = server)
