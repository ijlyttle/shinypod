library("shiny")
library("shinyBS")
library("shinyjs")
library("shinypod")

function(input, output, session) {
  dyg <- callModule(write_delim_server, "example", data = wx_ames)
  
}
