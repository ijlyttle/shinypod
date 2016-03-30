#' UI input elements for dygraph module.
#'
#' Used to define the UI input elements within the \code{dygraph} shiny module.
#'
#' This function returns a \code{shiny::\link[shiny]{tagList}} with members:
#'
#' \describe{
#'  \item{time}{\code{shiny::\link[shiny]{selectizeInput}}, used to specify time variable}
#'  \item{y1}{\code{shiny::\link[shiny]{selectizeInput}}, used to specify y1-axis variable}
#'  \item{y2}{\code{shiny::\link[shiny]{selectizeInput}}, used to specify y2-axis variable}
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
dygraph_ui_input <- function(id) {

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

#' UI output elements for dygraph module.
#'
#' Used to define the UI output elements within the \code{dygraph} shiny module.
#'
#' This function returns a \code{shiny::\link[shiny]{tagList}} with members:
#'
#' \describe{
#'  \item{status}{\code{shiny::\link[shiny]{htmlOutput}}, used to display status of the module}
#' }
#'
#' The purpose is to specify the UI elements - another set of functions can be used to specify layout.
#'
#' @family dygraph module functions
#
#' @param id, character used to specify namesapce, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}}
#'
#' @export
#
dygraph_ui_output <- function(id) {

  ns <- shiny::NS(id)

  ui_output <- shiny::tagList()

  ui_output$status <-
    shiny::htmlOutput(
      outputId = ns("status"),
      container = pre_scroll
    )

  ui_output
}

#' UI miscellaneous elements for dygraph module.
#'
#' Used to define the UI input elements within the \code{dygraph} shiny module.
#'
#' This function returns a \code{shiny::\link[shiny]{tagList}} with members:
#'
#' \describe{
#'  \item{help}{\code{shiny::\link[shiny]{tags}$pre}, contains guidance for using dygraph}
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
dygraph_ui_misc <- function(id) {

  ui_misc <- shiny::tagList()

  # ui_misc$help <-
  #   shiny::tags$pre("Zoom: Click-drag\tPan: Shift-Click-Drag\tReset: Double-Click")

  ui_misc
}


#' Server function for dygraph module.
#'
#' Used to define the server within the \code{dygraph} shiny module.
#'
#' @family dygraph module functions
#
#' @param input   standard \code{shiny} input
#' @param output  standard \code{shiny} output
#' @param session standard \code{shiny} session
#' @param data    data frame or \code{shiny::\link[shiny]{reactive}} that returns a data frame
#'
#' @return a \code{shiny::\link[shiny]{reactive}} that returns a dygraph
#'
#' @examples
#'
#' @export
#
dygraph_server <- function(
  input, output, session,
  data) {

  ns <- session$ns

  ### reactives ###
  #################

  rct_data <-
    shinypod::reactive_validate(data, is.data.frame, "Cannot display graph: no data")

  # names of time variables
  rct_var_time <- reactive({

    if (!isValidy(rct_data())) return(character(0))

    var_time <- df_names_inherits(rct_data(), c("POSIXct"))

    shiny::validate(
      shiny::need(var_time, "Cannot display graph: dataset has no time variables")
    )

    var_time
  })

  # names of numeric variables
  rct_var_num <- reactive({

    if (!isValidy(rct_data())) return(character(0))

    var_num <- df_names_inherits(rct_data(), c("numeric", "integer"))

    shiny::validate(
      shiny::need(var_num, "Cannot display graph: dataset has no numeric variables")
    )

    var_num
  })

  # names of variables available to y1-axis control
  rct_choice_y1 <- reactive({
    choice_y1 <- setdiff(rct_var_num(), input[["y2"]])

    choice_y1
  })

  # names of variables available to y2-axis control
  rct_choice_y2 <- reactive({
    choice_y2 <- setdiff(rct_var_num(), input[["y1"]])

    choice_y2
  })

  # basic dygraph
  rct_dyg <- reactive({

    var_time <- input[["time"]]
    var_y1 <- input[["y1"]]
    var_y2 <- input[["y2"]]

    shiny::validate(
      shiny::need(
        var_time %in% names(rct_data()),
        "Graph cannot display without a time-variable"
      ),
      shiny::need(
        c(var_y1, var_y2) %in% names(rct_data()),
        "Graph cannot display without any y-variables"
      )
    )

    dyg <- .dygraph(rct_data(), var_time, var_y1, var_y2)

    dyg
  })

  rct_state = reactive({
    list(
      has_data = isValidy(rct_data()),
      has_var_time = isValidy(rct_var_time()),
      has_var_num = isValidy(rct_var_num()),
      has_dyg = isValidy(rct_dyg())
    )
  })

  # status
  rctval_status <-
    shiny::reactiveValues(
      input = list(index = 0, is_valid = NULL, message = ""),
      result = list(index = 0, is_valid = NULL, message = "")
    )

  rct_status_content <- shiny::reactive(status_content(rctval_status))

  ### observers ###
  #################

  shiny::observe({
    shinyjs::toggleState("time", condition = rct_state()$has_var_time)
    shinyjs::toggleState("y1", condition = rct_state()$has_var_num)
    shinyjs::toggleState("y2", condition = rct_state()$has_var_num)
  })

  # input
  observeEvent(
    eventExpr = {
      isValidy(rct_data())
      input$time
      input$y1
      input$y2
    },
    handlerExpr = {

      rctval_status$input$index <- rctval_status$input$index + 1

      if (!isValidy(rct_data())){
        rctval_status$input$is_valid <- FALSE
        rctval_status$input$message <- "Please supply a dataset to graph"
      } else if (!isValidy(input$time)){
        rctval_status$input$is_valid <- FALSE
        rctval_status$input$message <- "Please supply a time variable to graph"
      } else if (!isValidy(input$y1) && !isValidy(input$y2)){
        rctval_status$input$is_valid <- FALSE
        rctval_status$input$message <- "Please supply a y-variable to graph"
      } else {
        rctval_status$input$is_valid <- TRUE
        rctval_status$input$message <- ""
      }

    },
    ignoreNULL = FALSE, # makes sure we evaluate on initialization
    priority = 1 # always execute before others
  )

  # result
  observeEvent(
    eventExpr = rct_dyg(),
    handlerExpr = {

      rctval_status$result$index <- rctval_status$input$index

      if (!isValidy(rct_dyg())){
        rctval_status$result$is_valid <- FALSE
        rctval_status$result$message <- "Cannot construct graph"
      } else {
        rctval_status$result$is_valid <- TRUE
        rctval_status$result$message <- "Zoom: Click-drag\tPan: Shift-Click-Drag\tReset: Double-Click"
      }

    }
  )

  # update choices for time variable
  shiny::observeEvent(
    eventExpr = rct_var_time(),
    handlerExpr = {
      updateSelectInput(
        session,
        inputId = "time",
        choices = rct_var_time(),
        selected = update_selected(input[["time"]], rct_var_time(), index = 1)
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
        selected = update_selected(input[["y1"]], rct_choice_y1(), index = 1)
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
        selected = update_selected(input[["y2"]], rct_choice_y2(), index = NULL)
      )
    },
    ignoreNULL = FALSE
  )

  observe_class_swap(id = "status", rct_status_content()$class)

  ## outputs ##
  #############

  output$status <-
    shiny::renderText(rct_status_content()$message)

  list(
    rct_dyg = rct_dyg,
    rct_state = rct_state
  )
}

# function that builds basic dygraph
# .dygraph(wx_ames, "date", "temp", "hum")
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

