library("shiny")
library("shinyBS")
library("shinyjs")
library("dygraphs")
library("magrittr")

app_id = "test_app_1"

ui <- fluidPage(
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      write_delim_sidebar(app_id)
    ),
    mainPanel(
      write_delim_main(app_id)
    )
  )
)

server <- function(input, output, session) {
  dyg <- callModule(write_delim_server, app_id, data = wx_ames)
}

shinyApp(ui, server)