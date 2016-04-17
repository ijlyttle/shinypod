library("shiny")
library("shinyjs")
library("tibble")
library("shinypod")

shinyServer(function(input, output, session) {

  list_rct <- callModule(module = read_delim_sidebar_server, id = "csv")

})
