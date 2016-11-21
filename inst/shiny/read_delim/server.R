library("shiny")
library("tibble")
library("shinypod")
library("dygraphs")

shinyServer(function(input, output, session) {

  rct_text <- callModule(module = upload_text_sidebar_server, id = "text")

  rct_data <- callModule(module = read_delim_sidebar_server, id = "data", text = rct_text)

  rct_dyg <- callModule(module = dygraph_sidebar_server, id = "dygraph", data = rct_data)

  observe({
    toggle("dyg", condition = isValidy(rct_dyg()))
  })

  output$dyg <- renderDygraph(dyOptions(rct_dyg(), useDataTimezone = TRUE))

})
