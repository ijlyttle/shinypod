library("shiny")
library("shinyBS")
library("shinyjs")
library("shinypod")

fluidPage(
  useShinyjs(),
  titlePanel("Write CSV example"),
  sidebarLayout(
    sidebarPanel(
      write_delim_sidebar("example")
    ),
    mainPanel(
      write_delim_main("example")
    )
  )
)