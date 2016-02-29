library("shiny")

fluidPage(
  titlePanel("Test Update Select Function"),
  sidebarLayout(
    sidebarPanel(
      shiny::selectizeInput(
        inputId = "first",
        label = "first",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
        #multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = "second",
        label = "second",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
        #multiple = FALSE
      ),
      shiny::tags$hr(),
      shiny::selectizeInput(
        inputId = "third",
        label = "third",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      shiny::selectizeInput(
        inputId = "fourth",
        label = "fourth",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      )
    ),
    mainPanel(
      verbatimTextOutput("txt")
    )
  )
)