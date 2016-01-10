library("shiny")
library("shinyjs")
library("shinyBS")
library("shinypod")

shinyUI(
  fluidPage(
    useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        read_delim_side("foo")
      ),
      mainPanel(
        read_delim_main("foo")
      )
    )
  )
)
