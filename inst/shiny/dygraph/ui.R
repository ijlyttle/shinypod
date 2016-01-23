library("shiny")
library("shinyBS")
library("shinyjs")
library("shinypod")
library("dygraphs")
library("dplyr")

fluidPage(
  useShinyjs(),
  titlePanel("Read csv and dygraph"),
  sidebarLayout(
    sidebarPanel(
      read_delim_sidebar_side("csv"),
      tags$hr(),
      dygraph_sidebar_side("dyg")
    ),
    mainPanel(
      read_delim_sidebar_main("csv"),
      dygraphOutput("csv_dyg")
    )
  )
)
