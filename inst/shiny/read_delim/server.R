library("shiny")
library("shinypod")

shinyServer(function(input, output, session) {

  # get text
  rct_text <- callModule(sample_delim_server, id = "delim")

  # parse text into dataframe

})
