library("shiny")
library("shinypod")
library("readr")

shinyServer(function(input, output, session) {

  rct_text <- reactive(
    read_file(input$sample_text)
  )

  output$text_preview <-
    renderUI(
      rct_text() %>%
      shinypod::text_html()
    )

})
