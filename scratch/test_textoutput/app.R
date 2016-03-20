#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
library("shinyjs")
library("shinypod")

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(

   useShinyjs(),

   titlePanel("extend verbatim output"),

   sidebarLayout(
      sidebarPanel(
        textInput(
          inputId = "text",
          label = "type something"
        ),
        selectInput(
          inputId = "class",
          label = "class",
          choices = c(
            default = " ",
            success = "alert-success",
            info = "alert-info",
            warning = "alert-warning",
            danger = "alert-danger"
          )
        )
      ),

      mainPanel(
         shiny::htmlOutput(
           outputId = "regular",
           container = pre_scroll
         )
      )
   )
))

server <- shinyServer(function(input, output) {

  output$regular <- renderText(input$text)
  observe_class_swap(id = "regular", input$class)

})

# Run the application
shinyApp(ui = ui, server = server)

