library("shiny")
library("shinyBS")
library("shinyjs")
library("shinypod")

function(input, output, session) {
  callModule(write_delim_sidebar_server, "example", data = wx_ames, delim = ",")
}
