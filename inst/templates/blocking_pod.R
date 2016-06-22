#' UI input elements for module that {{{ description }}}.
#'
#' Used to define the UI input elements within the \code{ {{{ name }}} } shiny module.
#'
#' This function returns a \code{shiny::\link[shiny]{tagList}} with members:
#'
#' \describe{
#'  \item{choice}{\code{shiny::\link[shiny]{select}}, input to choose action}
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

  # choice
  ui_input$choice <-
    shiny::selectizeInput(
      inputId = ns("choice"),
      label = "Action for column-names",
      choices = c(
        `make upper-case` = "toupper",
        `make lower-case` = "tolower",
        `none` = "identity"
      )
    )

  # button
  ui_input$button <-
    shiny::actionButton(
      inputId = ns("button"),
      label = "Do it!",
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
#'  \item{status}{\code{shiny::\link[shiny]{htmlOutput}},
#'    used to display status of the module}
#'  \item{data_preview}{\code{shiny::\link[shiny]{htmlOutput}},
#'    used to display the first few rows of the dataframe}
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

  ui_output$data_preview <-
    shiny::htmlOutput(
      outputId = ns("data_preview"),
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

#' Server function for module that {{{ description }}}.
#'
#' Used to define the server within the \code{ {{{ name }}} } shiny module.
#'
#' @family {{{ name }}} module functions
#
#' @param input   standard \code{shiny} input
#' @param output  standard \code{shiny} output
#' @param session standard \code{shiny} session
#' @param data    data.frame or a reactive that returns a data.frame
#'
#' @return \describe{
#'   \item{\code{ {{{ name }}}_server}}{a list containing:
#'     \itemize{
#'       \item \code{rct_result} a \code{shiny::\link[shiny]{reactive}},
#'         returning the resulting data.frame
#'       \item \code{rct_input_state} a \code{shiny::\link[shiny]{reactive}},
#'         returning a list of logicals describing the state of the inputs
#'       \item \code{rct_status_content} a \code{shiny::\link[shiny]{reactive}},
#'         returning a list with members \code{class} and \code{message} used to
#'         build the status output.
#'     }
#'   }
#'   \item{\code{ {{{ name }}}_sidebar_server}}{a reactive that returns a data.frame}
#' }
#'
#' @examples
#' shinyServer(function(input, output, session) {
#'
#'   list_rct <- callModule(
#'     module = {{{ name }}}_sidebar_server,
#'     id = "foo",
#'     data = iris
#'   )
#'
#' })
#'
#' @export
#
{{{ name }}}_server <- function(
  input, output, session,
  data
){

  ns <- session$ns

  ## functions ##
  ###############

  rename <- function(data, fn_rename){
    # given a dataframe and a function (or name of a function),
    # rename the columns of the dataframe according to the function
    names(data) <- do.call(fn_rename, list(names(data)))
    data
  }

  ## reactive sources ##
  ######################

  rctval_status <-
    shiny::reactiveValues(
      input = list(index = 0, is_valid = NULL, message = NULL),
      result = list(index = 0, is_valid = NULL, message = NULL)
    )

  rctval_result <- shiny::reactiveValues(data_new = NULL)

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
        has_data = shinypod::isValidy(rct_data()),
        has_fn_rename = shinypod::isValidy(rct_fn_rename())
      )
    })

  rct_data_new <- shiny::reactive(rctval_result$data_new)

  rct_status_content <- shiny::reactive(shinypod::status_content(rctval_status))

  rct_is_ready <- shiny::reactive({
    state <- rct_input_state()

    state$has_data && state$has_fn_rename
  })

  ## input-update observers ##
  ############################

  ## other observers ##
  #####################

  # input
  shiny::observeEvent(
    eventExpr = rct_input_state(),
    handlerExpr = {

      state <- rct_input_state()

      # default (all is well)
      is_valid <- TRUE
      message <- "Ready to transform column names"

      # for each potential invalid input state,
      # provide a message for the status output
      if (!state$has_data){
        is_valid <- FALSE
        message <- "Please supply a dataset"
      }  else if (!state$has_fn_rename) {
        is_valid <- FALSE
        message <- "Please make a renaming choice"
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
    eventExpr = input$button,
    handlerExpr = {
      # put the result in a reactive source
      rctval_result$data_new <- rename(rct_data(), rct_fn_rename())
    }
  )

  # result
  shiny::observeEvent(
    eventExpr = rct_data_new(),
    handlerExpr = {

      # default (all is well)
      is_valid = TRUE
      message = "Column names transformed"

      # for each potential problem in the result,
      # provide a message for the status output
      if (!shinypod::isValidy(rct_data_new())){
        rctval_status$result$is_valid <- FALSE
        rctval_status$result$message <- "Cannot transform column names"
      }

      rctval_status$result$index <- rctval_status$input$index
      rctval_status$result$is_valid <- is_valid
      rctval_status$result$message <- message
    }
  )

  shiny::observe(
    shinyjs::toggleState("button", condition = rct_is_ready())
  )

  ## outputs ##
  #############

  output$status <- shiny::renderText(rct_status_content()$message)

  output$data_preview <- shiny::renderUI(shinypod::tibble_html(rct_data_new()))

  # returns a list
  list(
    rct_result = rct_data_new,
    rct_input_state = rct_input_state,
    rct_status_content = rct_status_content
  )
}
