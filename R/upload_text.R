#' UI input elements text-file uploader.
#'
#' Used to define the UI input elements within the \code{upload_text} shiny module.
#'
#' This function returns a \code{shiny::\link[shiny]{tagList}} with members:
#'
#' \describe{
#'  \item{file}{\code{shiny::\link[shiny]{fileInput}}, used to specify file}
#' }
#'
#' The purpose is to specify the UI elements - another set of functions can be used to specify layout.
#'
#' @family upload_text module functions
#
#' @param id, character used to specify namesapce, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#'
#' @export
#
upload_text_ui_input <- function(id){

  ns <- shiny::NS(id)

  ui_input <- shiny::tagList()

  # specify file
  ui_input$file <-
    shiny::fileInput(
      inputId = ns("file"),
      label = "File",
      accept = c("text/csv", ".csv", "text/comma-separated-values", "text/plain")
    )

  ui_input
}

#' UI output elements for text-file uploader.
#'
#' Used to define the UI output elements within the \code{upload_text} shiny module.
#'
#' This function returns a \code{shiny::\link[shiny]{tagList}} with members:
#'
#' \describe{
#'  \item{status}{\code{shiny::\link[shiny]{htmlOutput}}, used to display status of the module}
#'  \item{text}{\code{shiny::\link[shiny]{htmlOutput}}, used to display first few lines of text from file}
#' }
#'
#' The purpose is to specify the UI elements - another set of functions can be used to specify layout.
#'
#' @family upload_text module functions
#
#' @param id, character used to specify namesapce, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#'
#' @export
#
upload_text_ui_output <- function(id){

  ns <- shiny::NS(id)

  ui_output <- shiny::tagList()

  ui_output$status <-
    shiny::htmlOutput(
      outputId = ns("status"),
      container = pre_scroll
    )

  ui_output$text <-
    shiny::htmlOutput(
      outputId = ns("text"),
      container = pre_scroll
    )

  ui_output
}

#' UI miscellaneous elements for text-file uploader.
#'
#' Used to define the UI miscellaneous elements within the \code{upload_text} shiny module.
#'
#' This function returns a \code{shiny::\link[shiny]{tagList}} with members:
#'
#' \describe{
#' }
#'
#' The purpose is to specify the UI elements - another set of functions can be used to specify layout.
#'
#' @family upload_text module functions
#
#' @param id, character used to specify namesapce, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#'
#' @export
#
upload_text_ui_misc <- function(id){

  ui_misc <- shiny::tagList()

  ui_misc
}

#' Server function for delimted-file reader.
#'
#' Used to define the server within the \code{upload_text} shiny module.
#'
#' @family upload_text module functions
#
#' @param input   standard \code{shiny} input
#' @param output  standard \code{shiny} output
#' @param session standard \code{shiny} session
#'
#' @return \code{shiny::\link[shiny]{reactive}} that returns raw text
#'
#' @examples
#' shinyServer(function(input, output, session) {
#' })
#'
#' @export
#
upload_text_server <- function(
  input, output, session
){

  ns <- session$ns

  ## reactives ##
  ###############

  # status
  rctval_status <-
    shiny::reactiveValues(
      input = list(index = 0, is_valid = NULL, message = NULL),
      result = list(index = 0, is_valid = NULL, message = NULL)
    )

  rctval_content <-
    shiny::reactiveValues(
      text = NULL
    )

  rct_text <- shiny::reactive({

    shiny::validate(
      shiny::need(rctval_content$text, "No text available")
    )

    rctval_content$text
  })

  rct_state_input <- shiny::reactive({

    list(
      has_file = isValidy(input$file)
    )
  })

  rct_status_content <- shiny::reactive(status_content(rctval_status))


  ## observers ##
  ###############

  # input state
  shiny::observeEvent(
    eventExpr = rct_state_input(),
    handlerExpr = {

      is_valid <- TRUE
      message <- NULL

      state <- rct_state_input()

      if (!state$has_file){
        is_valid <- FALSE
        message <- c(message, "Please select a file")
      }

      rctval_status$input$index <- rctval_status$input$index + 1
      rctval_status$input$is_valid <- is_valid
      rctval_status$input$message <- paste(message, collapse = "\n")

    },
    ignoreNULL = FALSE, # makes sure we evaluate on initialization
    priority = 1 # always execute before others
  )

  # result state
  shiny::observeEvent(
    eventExpr = input$file,
    handlerExpr = {

      # do the observers fire after all the reactives have updated?
      file_name <- input$file$name
      text <- readr::read_file(input$file$datapath)

      if (isValidy(text)){
        is_valid <- TRUE
        message <- paste("Uploaded file:", file_name)
      } else {
        is_valid <- FALSE
        message <- paste("Cannot find text in file:", file_name)
      }

      rctval_content$text <- text

      rctval_status$result$index <- rctval_status$input$index
      rctval_status$result$is_valid <- is_valid
      rctval_status$result$message <- paste(message, collapse = "\n")
    }
  )

  ## outputs ##
  #############

  output$status <-
    shiny::renderText(rct_status_content()$message)

  output$text <-
    shiny::renderUI(text_html(rct_text()))

  ## return value ##
  ##################

  list(
    rct_result = rct_text,
    rct_input_state = rct_input_state,
    rct_status = rct_status
  )

}
