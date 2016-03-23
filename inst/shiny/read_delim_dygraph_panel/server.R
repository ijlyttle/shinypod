library("shiny")
library("shinyjs")
library("shinypod")
library("dygraphs")

shinyServer(function(input, output, session) {

  rct_data <- callModule(module = read_delim_sidebar_server, id = "csv")

  rct_dyg <- callModule(dygraph_sidebar_server, "dyg", data = rct_data)

  observe({
    shinyjs::toggle(id = "csv_dyg", condition = isValidy(rct_dyg()))
  })

  output$csv_dyg <- renderDygraph({
    rct_dyg() %>%
      dyOptions(useDataTimezone = TRUE)
  })

})
