source("inst/shiny-app/utils.R")
source("inst/shiny-app/plot_funs.R")
library(shiny)

ui <- source("inst/shiny-app/ui.R", local = TRUE)$value
server <- source("inst/shiny-app/server.R", local = TRUE)$value

shinyApp(ui = ui, server = server)

ekioplot::show_ekio_palette("contrast", n = 8)

ekioplot::ekio_pal("contrast", n = 8)
