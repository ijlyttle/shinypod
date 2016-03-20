library("shiny")
library("shinyjs")
library("shinypod")

shinyUI(
  fluidPage(
    useShinyjs(debug = TRUE),
    titlePanel("CSV parser"),
    sidebarLayout(
      sidebarPanel(
        read_delim_sidebar_side("csv")
      ),
      mainPanel(
        read_delim_sidebar_main("csv")
      )
    )
  )
)
