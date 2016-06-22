library("shiny")
library("shinyjs")
library("shinypod")

shinyServer(function(input, output, session) {

  callModule(module = upload_text_sidebar_server, id = "upload")

})
