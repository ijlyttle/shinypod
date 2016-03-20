library("shiny")
library("shinyBS")
library("shinyjs")
library("shinypod")
library("dygraphs")
library("dplyr")

function(input, output, session) {

  list_rct <- callModule(module = read_delim_sidebar_server, id = "csv")
  rct_data <- list_rct$rct_data

  rct_dyg <- callModule(dygraph_server, "dyg", data = rct_data)

  output$csv_dyg <- renderDygraph({
    rct_dyg() %>%
      dyOptions(useDataTimezone = TRUE)
  })

}
