library("shiny")
library("shinyjs")
library("shinyBS")
library("shinypod")

shinyUI(
  fluidPage(
    useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        read_delim_sidebar_side("foo")
      ),
      mainPanel(
        read_delim_sidebar_main("foo")
      )
    )
  )
)
