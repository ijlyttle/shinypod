library("shiny")
library("shinyjs")
library("shinypod")
library("tibble")

function(input, output, session) {
  callModule(write_delim_sidebar_server, "example", data = wx_ames, delim = ",")
  callModule(foo_sidebar_server, "foo", data = wx_ames)
}
