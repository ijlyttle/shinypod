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
      h4("Dygraph"),
      read_delim_sidebar_side("test_app_2"),
      dygraph_input(app_id)
    ),
    mainPanel(
      h3("Dygraph"),
      dygraph_output(app_id),
      dygraphs::dygraphOutput("view_dygraph2")
    )
  )
)

server <- function(input, output, session) {
  rct_data <- callModule(read_delim_server, "test_app_2")
  dyg <- callModule(dygraph_server, app_id, 
    plot_df = rct_data())


  output$view_dygraph2 <-  dygraphs::renderDygraph({
    #dyg()
    dyg() %>% dygraphs::dyOptions(useDataTimezone = TRUE)
  })


}

shinyApp(ui, server)
