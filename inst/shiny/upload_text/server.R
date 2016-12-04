library("shiny")
library("shinypod")

shinyServer(function(input, output, session) {

  rct_text <- callModule(module = upload_text_sb_server, id = "upload")

})
