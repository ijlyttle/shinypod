#' UI input elements for module that makes a dygraph.
#'
#' Used to define the UI input elements within the \code{ dygraph } shiny module.
#'
#' This function returns a \code{shiny::\link[shiny]{tagList}} with members:
#'
#' \describe{
#'  \item{choice}{\code{shiny::\link[shiny]{selectizeInput}}, input to choose action}
#' }
#'
#' The purpose is to specify the UI elements - another set of functions can be used to specify layout.
#'
#' @family dygraph module functions
#
#' @param id, character used to specify namesapce, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#'
#' @export
#
dygraph_ui_input <- function(id){

  ns <- shiny::NS(id)

  ui_input <- shiny::tagList()

  ui_input$time <-
    shiny::selectizeInput(
      inputId = ns("time"),
      label = "Time",
      choices = NULL,
      selected = NULL,
      multiple = FALSE
    )

  ui_input$y1 <-
    shiny::selectizeInput(
      inputId = ns("y1"),
      label = "Y1 axis",
      choices = NULL,
      selected = NULL,
      multiple = TRUE
    )

  ui_input$y2 <-
    shiny::selectizeInput(
      inputId = ns("y2"),
      label = "Y2 axis",
      choices = NULL,
      selected = NULL,
      multiple = TRUE
    )

  ui_input
}

#' UI output elements for module that makes a dygraph.
#'
#' Used to define the UI output elements within the \code{ dygraph } shiny module.
#'
#' This function returns a \code{shiny::\link[shiny]{tagList}} with members:
#'
#' \describe{
#'  \item{status}{\code{shiny::\link[shiny]{htmlOutput}},
#'    used to display status of the module}
#' }
#'
#' The purpose is to specify the UI elements - another set of functions can be used to specify layout.
#'
#' @family dygraph module functions
#
#' @param id, character used to specify namesapce, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#'
#' @export
#
dygraph_ui_output <- function(id){

  ns <- shiny::NS(id)

  ui_output <- shiny::tagList()

  ui_output$status <-
    shiny::htmlOutput(
      outputId = ns("status"),
      container = shinypod::pre_scroll
    )

  ui_output
}

#' UI miscellaneous elements for module that makes a dygraph.
#'
#' Used to define the UI miscellaneous elements within the \code{ dygraph } shiny module.
#'
#' This function returns a \code{shiny::\link[shiny]{tagList}} with members:
#'
#' \describe{
#' }
#'
#' The purpose is to specify the UI elements - another set of functions can be used to specify layout.
#'
#' @family dygraph module functions
#
#' @param id, character used to specify namesapce, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#'
#' @export
#
dygraph_ui_misc <- function(id){

  # this is for elements that are neither inputs nor outputs

  ui_misc <- shiny::tagList()

  ui_misc
}

#' Server function for module that makes a dygraph.
#'
#' Used to define the server within the \code{ dygraph } shiny module.
#'
#' @family dygraph module functions
#
#' @param input   standard \code{shiny} input
#' @param output  standard \code{shiny} output
#' @param session standard \code{shiny} session
#' @param data    data.frame or a reactive that returns a data.frame
#'
#' @return \describe{
#'   \item{\code{ dygraph_server}}{a list containing:
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
#'   \item{\code{ dygraph_sidebar_server}}{a reactive that returns a data.frame}
#' }
#'
#' @examples
#' shinyServer(function(input, output, session) {
#'
#'   list_rct <- callModule(
#'     module = dygraph_sidebar_server,
#'     id = "foo",
#'     data = shinypod::wx_ames
#'   )
#'
#' })
#'
#' @export
#
dygraph_server <- function(
  input, output, session,
  data
){

  ns <- session$ns

  ## functions ##
  ###############

  .dygraph <- function(data, var_time, var_y1, var_y2){

    # create the mts object
    vec_time <- data[[var_time]]
    df_num <- data[c(var_y1, var_y2)]

    # if no tz, use UTC
    tz <- lubridate::tz(vec_time)
    if (identical(tz, "")) {
      tz <- "UTC"
    }

    dy_xts <- xts::xts(df_num, order.by = vec_time, tzone = tz)

    dyg <- dygraphs::dygraph(dy_xts)
    dyg <- dygraphs::dyAxis(dyg, "x", label = var_time)
    dyg <- dygraphs::dyAxis(dyg, "y", label = paste(var_y1, collapse = ", "))
    dyg <- dygraphs::dyAxis(dyg, "y2", label = paste(var_y2, collapse = ", "))

    # put stuff on y2 axis
    for(i in seq_along(var_y2)) {
      dyg <- dygraphs::dySeries(dyg, var_y2[i], axis = "y2")
    }

    dyg
  }

  ## reactive sources ##
  ######################

  rctval_status <-
    shiny::reactiveValues(
      input = list(index = 0, is_valid = NULL, message = NULL),
      result = list(index = 0, is_valid = NULL, message = NULL)
    )

  ## reactive conductors ##
  #########################

  rct_data <-
    shinypod::reactive_validate(data, is.data.frame, message = "Please supply a dataset")

  # names of time variables
  rct_var_time <-
    shiny::reactive({

      if (!shinypod::isValidy(rct_data())) return(character(0))

      var_time <- df_names_inherits(rct_data(), c("POSIXct"))

      shiny::validate(
        shiny::need(var_time, "Cannot display graph: dataset has no time variables")
      )

      var_time
    })

  # names of numeric variables
  rct_var_num <-
    shiny::reactive({

      if (!shinypod::isValidy(rct_data())) return(character(0))

      var_num <- df_names_inherits(rct_data(), c("numeric", "integer"))

      shiny::validate(
        shiny::need(var_num, "Cannot display graph: dataset has no numeric variables")
      )

      var_num
    })

  # names of variables available to y1-axis control
  rct_choice_y1 <-
    shiny::reactive({
      choice_y1 <- setdiff(rct_var_num(), input$y2)

      choice_y1
    })

  # names of variables available to y2-axis control
  rct_choice_y2 <-
    shiny::reactive({
      choice_y2 <- setdiff(rct_var_num(), input$y1)

      choice_y2
    })

  rct_input_state <-
    shiny::reactive({
      list(
        has_data = shinypod::isValidy(rct_data()),
        has_var_time = shinypod::isValidy(rct_var_time()),
        has_var_num = shinypod::isValidy(rct_var_num()),
        has_time = shinypod::isValidy(input$time),
        has_y1 = shinypod::isValidy(input$y1),
        has_y2 = shinypod::isValidy(input$y2)
      )
    })

  rct_dyg <- reactive({

    shiny::validate(
      shiny::need(
        input$time %in% names(rct_data()),
        "Graph cannot display without a time-variable"
      ),
      shiny::need(
        c(input$y1, input$y2) %in% names(rct_data()),
        "Graph cannot display without any y-variables"
      )
    )

    .dygraph(rct_data(), input$time, input$y1, input$y2)
  })

  rct_status_content <- shiny::reactive(shinypod::status_content(rctval_status))

  ## input-update observers ##
  ############################

  # update choices for time variable
  shiny::observeEvent(
    eventExpr = rct_var_time(),
    handlerExpr = {
      updateSelectInput(
        session,
        inputId = "time",
        choices = rct_var_time(),
        selected = update_selected(input$time, rct_var_time(), index = 1)
      )
    },
    ignoreNULL = FALSE
  )

  # update choices for y1 variable
  shiny::observeEvent(
    eventExpr = rct_choice_y1(),
    handlerExpr = {
      updateSelectInput(
        session,
        inputId = "y1",
        choices = rct_choice_y1(),
        selected = update_selected(input$y1, rct_choice_y1(), index = 1)
      )
    },
    ignoreNULL = FALSE
  )

  # update choices for y2 variable
  shiny::observeEvent(
    eventExpr = rct_choice_y2(),
    handlerExpr = {
      updateSelectInput(
        session,
        inputId = "y2",
        choices = rct_choice_y2(),
        selected = update_selected(input$y2, rct_choice_y2(), index = NULL)
      )
    },
    ignoreNULL = FALSE
  )


  ## other observers ##
  #####################

  # input
  shiny::observeEvent(
    eventExpr = rct_input_state(),
    handlerExpr = {

      state <- rct_input_state()

      # default
      is_valid <- TRUE
      message <- "" # will not be displayed

      if (!state$has_data){
        is_valid <- FALSE
        message <- "Please supply a dataset"
      } else if (!state$has_time){
        is_valid <- FALSE
        message <- "Please supply a time variable to graph"
      } else if (!state$has_y1 && !state$has_y2){
        is_valid <- FALSE
        message <- "Please supply a y-variable to graph"
      }

      rctval_status$input$index <- rctval_status$input$index + 1
      rctval_status$input$is_valid <- is_valid
      rctval_status$input$message <- message
    },
    ignoreNULL = FALSE, # makes sure we evaluate on initialization
    priority = 1 # always execute before others
  )

  # result
  shiny::observeEvent(
    eventExpr = rct_dyg(),
    handlerExpr = {

      # default
      is_valid <- TRUE
      message <- "Zoom: Click-drag\tPan: Shift-Click-Drag\tReset: Double-Click"

      if (!shinypod::isValidy(rct_dyg())){
        is_valid <- FALSE
        message <- "Cannot make dygraph"
      }

      rctval_status$result$index <- rctval_status$input$index
      rctval_status$result$is_valid <- is_valid
      rctval_status$result$message <- message
    }
  )

  ## outputs ##
  #############

  output$status <- shiny::renderText(rct_status_content()$message)

  # returns a list
  list(
    rct_result = rct_dyg,
    rct_input_state = rct_input_state,
    rct_status_content = rct_status_content
  )
}
