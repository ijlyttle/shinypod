library("shiny")
library("shinyjs")
library("shinypod")

fluidPage(
  useShinyjs(),
  titlePanel("Write CSV example"),
  sidebarLayout(
    sidebarPanel(
      write_delim_sidebar_side("example")
    ),
    mainPanel(
      write_delim_sidebar_main("example")
    )
  )
)
