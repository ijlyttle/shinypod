library("shiny")
library("shinyjs")
library("dplyr")
library("shinypod")

shinyServer(function(input, output, session) {

  rct_data <- callModule(module = read_delim_sidebar_server, id = "csv")

})
