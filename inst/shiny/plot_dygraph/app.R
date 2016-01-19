library("shiny")
library("shinyBS")
library("shinyjs")
library("shinychord")

app_id = "test_app_1"

ui <- fluidPage(
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      h4("Dygraph"),
      dygraph_input(app_id)
    ),
    mainPanel(
      h3("Dygraph"),
      dygraph_output(app_id)
    )
  )
)

server <- function(input, output, session) {
  data = virtualSensorSE::sensor_training
  datafile <- callModule(dygraph_server, app_id, 
    plot_df = data)
}

shinyApp(ui, server)
