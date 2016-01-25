library("shiny")
library("shinyjs")
library("shinyBS")
library("dplyr")
library("shinypod")

shinyUI(
  fluidPage(
    useShinyjs(),
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
