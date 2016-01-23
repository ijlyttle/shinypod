library("shiny")
library("shinyBS")
library("shinyjs")
library("dygraphs")
library("dplyr")

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Read csv and dygraph"),
  sidebarLayout(
    sidebarPanel(
      read_delim_sidebar_side("csv"),
      tags$hr(),
      dygraph_sidebar_side("dyg")
    ),
    mainPanel(
      read_delim_sidebar_main("csv"),
      dygraphOutput("csv_dyg")
    )
  )
)

server <- function(input, output, session) {

  rct_data <- callModule(read_delim_server, "csv")

  rct_dyg <- callModule(dygraph_server, "dyg", data = rct_data)

  output$csv_dyg <- renderDygraph({
    rct_dyg() %>%
    dyOptions(useDataTimezone = TRUE)
  })

}

shinyApp(ui, server)
