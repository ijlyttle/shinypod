library("shiny")
library("shinyjs")
library("shinyBS")
library("shinypod")

shinyServer(function(input, output, session) {

  rctval_temp <- reactiveValues(
    a = NULL
  )

  read_delim <- callModule(
    module = read_delim_server,
    id = "foo"
  )

  observeEvent(
    eventExpr = read_delim$data(),
    handlerExpr = {rctval_temp$a <- read_delim$data()}
  )

  observe(print(rctval_temp$a))
})
