#' UI input elements for module that {{{ description }}}.
#'
#' Used to define the UI input elements within the \code{ {{{ name }}} } shiny module.
#'
#' This function returns a \code{shiny::\link[shiny]{tagList}} with members:
#'
#' \describe{
#'  \item{button_yell}{\code{shiny::\link[shiny]{fileInput}}, button to ivoke upper-case}
#' }
#'
#' The purpose is to specify the UI elements - another set of functions can be used to specify layout.
#'
#' @family {{{ name }}} module functions
#
#' @param id, character used to specify namesapce, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#'
#' @export
#
{{{ name }}}_ui_input <- function(id){

  ns <- shiny::NS(id)

  ui_input <- shiny::tagList()

  # action button
  ui_input$button <-
    shiny::actionButton(
      inputId = ns("button_yell"),
      label = "yell",
      class = "btn-primary"
    )

  ui_input
}

#' UI output elements for module that {{{ description }}}.
#'
#' Used to define the UI output elements within the \code{ {{{ name }}} } shiny module.
#'
#' This function returns a \code{shiny::\link[shiny]{tagList}} with members:
#'
#' \describe{
#'  \item{status}{\code{shiny::\link[shiny]{htmlOutput}}, used to display status of the module}
#' }
#'
#' The purpose is to specify the UI elements - another set of functions can be used to specify layout.
#'
#' @family {{{ name }}} module functions
#
#' @param id, character used to specify namesapce, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#'
#' @export
#
{{{ name }}}_ui_output <- function(id){

  ns <- shiny::NS(id)

  ui_output <- shiny::tagList()

  ui_output$status <-
    shiny::htmlOutput(
      outputId = ns("status"),
      container = shinypod::pre_scroll
    )

  ui_output$data <-
    shiny::htmlOutput(
      outputId = ns("data"),
      container = shinypod::pre_scroll
    )

  ui_output$data_new <-
    shiny::htmlOutput(
      outputId = ns("data_new"),
      container = shinypod::pre_scroll
    )

  ui_output
}

#' UI miscellaneous elements for module that {{{ description }}}.
#'
#' Used to define the UI miscellaneous elements within the \code{ {{{ name }}} } shiny module.
#'
#' This function returns a \code{shiny::\link[shiny]{tagList}} with members:
#'
#' \describe{
#' }
#'
#' The purpose is to specify the UI elements - another set of functions can be used to specify layout.
#'
#' @family {{{ name }}} module functions
#
#' @param id, character used to specify namesapce, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#'
#' @export
#
{{{ name }}}_ui_misc <- function(id){

  # this is for elements that are neither inputs nor outputs

  ui_misc <- shiny::tagList()

  ui_misc
}

#' Server function for {{{ description }}}.
#'
#' Used to define the server within the \code{ {{{ name }}} } shiny module.
#'
#' @family {{{ name }}} module functions
#
#' @param input   standard \code{shiny} input
#' @param output  standard \code{shiny} output
#' @param session standard \code{shiny} session
#' @param data    data.frame, possibly reactive
#' @param status_show logical, possibly reactive, indicates if to show the status output
#' @param status_alert logical, possibly reactive, indicates if to change alert-class of status output
#'
#' @return \code{ {{{ name }}}_server}: a list containing reactives that return a data.frame
#'    and a list of logicals that describe the state of the module,
#'    \code{ {{{ name }}}_sidebar_server}: a reactive that returns a data.frame
#'
#' @examples
#' shinyServer(function(input, output, session) {
#'
#'   list_rct <- callModule(
#'     module = {{{ name }}}_server,
#'     id = "foo"
#'   )
#'
#' })
#'
#' @export
#
{{{ name }}}_server <- function(
  input, output, session,
  data,
  status_alert = TRUE
){

  ns <- session$ns

  ## reactives ##
  ###############

  rct_data <-
    shinypod::reactive_validate(data, is.data.frame, message = "Please supply a dataset")

  rct_status_alert <- shiny::reactive({
    shinypod::static(status_alert)
  })

  rct_state <- shiny::reactive({
    list(
      has_data = shinypod::isValidy(rct_data()),
      has_data_new = shinypod::isValidy(rct_data_new())
    )
  })

  # new data
  rctval <- shiny::reactiveValues(data_new = NULL)

  rct_data_new <- shiny::reactive(rctval$data_new)

  # status
  rctval_status <-
    shiny::reactiveValues(
      input = list(index = 0, is_valid = NULL, message = NULL),
      result = list(index = 0, is_valid = NULL, message = NULL)
    )

  rct_status_content <- shiny::reactive(shinypod::status_content(rctval_status))

  ## input updates ##
  ###################

  ## observers ##
  ###############

  # button should be active only when we have data
  observe({
    shinyjs::toggleState(id = "button-scream", condition = rct_state()$has_data)
  })

  # input
  observeEvent(
    eventExpr = {
      rct_data()
    },
    handlerExpr = {

      rctval_status$input$index <- rctval_status$input$index + 1

      if (!shinypod::isValidy(rct_data())){
        rctval_status$input$is_valid <- FALSE
        rctval_status$input$message <- "Please supply a dataset"
      }  else {
        rctval_status$input$is_valid <- TRUE
        rctval_status$input$message <- "Ready to yell!"
      }

    },
    ignoreNULL = FALSE, # makes sure we evaluate on initialization
    priority = 1 # always execute before others
  )

  # result
  observeEvent(
    eventExpr = input$button_yell,
    handlerExpr = {

      rctval_status$result$index <- rctval_status$input$index

      # stuff that results from hitting the button
      rctval$data_new <-
        tryCatch(
          expr = {
            data_new <- rct_data()
            colnames(data_new) <- toupper(colnames(data_new))
            data_new
          },
          error = function(e){
            error_message <<- e$message # this will be a side-effect
            NULL
          }
        )

      if (is.null(rctval$data_new)){
        rctval_status$result$is_valid <- FALSE
        rctval_status$result$message <- "Cannot make column names yell"
      } else {
        rctval_status$result$is_valid <- TRUE
        rctval_status$result$message <- "Column names are now YELLING!"
      }

    }
  )

  # used to change the class of the status box
  observe({
    if (rct_status_alert()){
      shinypod::observe_class_swap(id = "status", rct_status_content()$class)
    }
  })

  ## outputs ##
  #############

  output$status <-
    shiny::renderText(rct_status_content()$message)

  # old dataframe
  output$data <- shiny::renderUI(shinypod::tibble_html(rct_data()))

  # NEW DATAFRAME
  output$data_new <- shiny::renderUI(shinypod::tibble_html(rct_data_new()))

  # returns a list
  list(rct_data = rct_data, rct_state = rct_state)
}
