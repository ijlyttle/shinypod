library("shiny")
library("shinyjs")
library("shinypod")
library("dygraphs")

shinyUI(
  fluidPage(
    useShinyjs(),
    titlePanel("CSV parser"),
    sidebarLayout(
      sidebarPanel(
        upload_text_sidebar_side("text"),
        read_delim_sidebar_side("data"),
        dygraph_sidebar_side("dygraph")
      ),
      mainPanel(
        upload_text_sidebar_main("text"),
        read_delim_sidebar_main("data"),
        dygraph_sidebar_main("dygraph"),
        dygraphOutput("dyg")
      )
    )
  )
)
