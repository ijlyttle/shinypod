library("shiny")
library("tibble")
library("shinypod")
library("dygraphs")

shinyServer(function(input, output, session) {

  rct_text <- callModule(module = upload_text_sidebar_server, id = "text")

#   rct_text <- reactive(system.file("extdata", "wx_ames.csv", package = "shinypod"))

  rct_data <- callModule(module = read_delim_sidebar_server, id = "data", text = rct_text)

  rct_dyg <- callModule(module = dygraph_sidebar_server, id = "dygraph", data = rct_data)

  output$dyg <- renderDygraph(rct_dyg())

})
