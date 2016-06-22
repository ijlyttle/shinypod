library("shiny")
library("shinyjs")
library("shinypod")

shinyUI(
  fluidPage(
    useShinyjs(debug = TRUE),
    titlePanel("Upload Text"),
    sidebarLayout(
      sidebarPanel(
        upload_text_sidebar_side("upload")
      ),
      mainPanel(
        upload_text_sidebar_main("upload")
      )
    )
  )
)
