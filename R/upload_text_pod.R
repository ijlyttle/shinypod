
upload_text_ui_input <- function(id){

  ns <- shiny::NS(id)

  ui_input <- shiny::tagList()

  # file upload
  ui_input$file <-
    shiny::fileInput(
      inputId = ns("file"),
      label = "Upload text file",
      accept = c("text/csv", ".csv", "text/comma-separated-values", "text/plain")
    )

  ui_input
}

upload_text_ui_output <- function(id){

  ns <- shiny::NS(id)

  ui_output <- shiny::tagList()

  ui_output$status <-
    shiny::htmlOutput(
      outputId = ns("status"),
      container = shinypod::pre_scroll
    )

  ui_output$data_preview <-
    shiny::htmlOutput(
      outputId = ns("data_preview"),
      container = shinypod::pre_scroll
    )

  ui_output
}


upload_text_ui_misc <- function(id){

  # this is for elements that are neither inputs nor outputs

  ui_misc <- shiny::tagList()

  ui_misc
}


upload_text_server <- function(
  input, output, session,
  data
){

  ns <- session$ns

  ## functions ##
  ###############

  ## reactive sources ##
  ######################

  rctval_status <-
    shiny::reactiveValues(
      input = list(index = 0, is_valid = NULL, message = NULL),
      result = list(index = 0, is_valid = NULL, message = NULL)
    )

  rctval_result <- shiny::reactiveValues(text = NULL)

  ## reactive conductors ##
  #########################

  rct_data <-
    shinypod::reactive_validate(data, is.data.frame, message = "Please supply a dataset")

  rct_fn_rename <-
    shiny::reactive({

      shiny::validate(
        shiny::need(input$choice, "Please make a choice")
      )

      input$choice
    })

  rct_input_state <-
    shiny::reactive({
      list(
        has_file = shinypod::isValidy(input$file)
      )
    })

  rct_text <- shiny::reactive(rctval_result$text)

  rct_status_content <- shiny::reactive(shinypod::status_content(rctval_status))

  ## input-update observers ##
  ############################

  ## other observers ##
  #####################

  # input
  shiny::observeEvent(
    eventExpr = rct_input_state(),
    handlerExpr = {

      state <- rct_input_state()

      # default
      is_valid <- TRUE
      message <- ""

      if (!state$has_file){
        is_valid <- FALSE
        message <- "Please choose a text file"
      }

      rctval_status$input$index <- rctval_status$input$index + 1
      rctval_status$input$is_valid <- is_valid
      rctval_status$input$message <- message
    },
    ignoreNULL = FALSE, # makes sure we evaluate on initialization
    priority = 1 # always execute before others
  )

  # button
  shiny::observeEvent(
    eventExpr = input$file,
    handlerExpr = {
      # put the result in a reactive source
      rctval_result$text <- readr::read_file(input$file$datapath)
    }
  )

  # result
  shiny::observeEvent(
    eventExpr = rct_text(),
    handlerExpr = {

      # default
      is_valid = TRUE
      message = paste("File uploaded:", input$file$name)

      if (!shinypod::isValidy(rct_text())){
        is_valid <- FALSE
        message <- paste("Cannot find text in:", input$file$name)
      }

      rctval_status$result$index <- rctval_status$input$index
      rctval_status$result$is_valid <- is_valid
      rctval_status$result$message <- message
    }
  )

  ## outputs ##
  #############

  output$status <- shiny::renderText(rct_status_content()$message)

  output$data_preview <- shiny::renderUI(shinypod::text_html(rct_text()))

  # returns a list
  list(
    rct_result = rct_text,
    rct_input_state = rct_input_state,
    rct_status_content = rct_status_content
  )
}
