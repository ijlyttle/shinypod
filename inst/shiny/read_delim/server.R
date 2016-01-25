library("shiny")
library("shinyjs")
library("shinyBS")
library("dplyr")
library("shinypod")

shinyServer(function(input, output, session) {

  rct_data <- callModule(module = read_delim_server, id = "csv")

})
