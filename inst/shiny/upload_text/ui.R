library("shiny")
library("shinyjs")
library("shinypod")

shinyUI(
  fluidPage(
    useShinyjs(),
    titlePanel("Upload Text"),
    sidebarLayout(
      sidebarPanel(
        upload_text_sb_side("upload")
      ),
      mainPanel(
        upload_text_sb_main("upload")
      )
    )
  )
)
