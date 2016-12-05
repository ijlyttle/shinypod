# note that this module acts as a source of sorts
upload_text_ui <- function(id){

  ns <- shiny::NS(id)

  # returns a list with members: input, output, misc - each a shiny tagList
  ui <- sp_ui()

  ### input ###
  #############

  # file
  ui$input$file <-
    shiny::fileInput(
      inputId = ns("file"),
      label = "Upload text file",
      accept = c("text/csv", ".csv", "text/comma-separated-values", "text/plain")
    )

  ### output ###
  ##############

  # text_preview
  ui$output$text_preview <-
    shiny::htmlOutput(
      outputId = ns("text_preview"),
      container = shinypod::pre_scroll
    )

  ui
}

upload_text_server <- function(input, output, session){

  ns <- session$ns

  ## functions ##
  ###############

  ## reactive sources ##
  ######################

  rctval <- shiny::reactiveValues(
    text = NULL,
    notification = NULL
  )

  ## reactive conductors ##
  #########################

  rct_text <- shiny::reactive(rctval$text)

  rct_state <-
    shiny::reactive({
      list(
        has_file = shinypod::isValidy(input$file)
      )
    })

  rct_notification <- shiny::reactive(rctval$notification)

  ## input-update observers ##
  ############################

  ## other observers ##
  #####################

  # button
  shiny::observeEvent(
    eventExpr = input$file,
    handlerExpr = {
      # put the result in a reactive source
      rctval$text <- readr::read_file(input$file$datapath)

      str_size <-
        input$file$size %>%
        structure(class = "object_size") %>%
        format(units = "auto")

      rctval$notification <- list(
        ui = paste(
          "Uploaded", input$file$name,
          paste0("(", str_size, ")")
        ),
        type = "message"
      )
    }
  )

  ## outputs ##
  #############

  output$text_preview <-
    shiny::renderUI(
      rct_text() %>%
      shinypod::text_html()
    )

  # returns a list
  list(
    rct_result = rct_text,
    rct_state = rct_state,
    rct_notification = rct_notification
  )
}
