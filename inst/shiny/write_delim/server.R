library("shiny")
library("shinyjs")
library("shinypod")
library("tibble")

function(input, output, session) {
  callModule(write_delim_sidebar_server, "example", data = wx_ames, delim = ",", filename = "test.csv")
}
