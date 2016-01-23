#' UI input elements for dygraph module.
#'
#' Used to define the UI input elements within the \code{dygraph} shiny module.
#'
#' This function returns a \code{shiny::\link[shiny]{tagList}} with members:
#'
#' \describe{
#'  \item{time}{\code{shiny::\link[shiny]{selectInput}}, used to specify time variable}
#'  \item{y1}{\code{shiny::\link[shiny]{selectInput}}, used to specify time variable}
#'  \item{y2}{\code{shiny::\link[shiny]{selectInput}}, used to specify time variable}
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

  ns <- NS(id)

  ui_input <- shiny::tagList()

  ui_input$time <- shiny::uiOutput(ns("time_out"))

  ui_input$y1 <- shiny::uiOutput(ns("y1_out"))

  ui_input$y2 <- shiny::uiOutput(ns("y2_out"))

  ui_input
}

#' UI output elements for delimited-file reader.
#'
#' Used to define the UI output elements within the \code{read_delim} shiny module.
#'
#' Because there are no outputs,
#' this function returns an empty \code{shiny::\link[shiny]{tagList}}.
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

  ui_output <- shiny::tagList()

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

  ui_misc$help <-
    shiny::tags$pre("Zoom: Click-drag\tPan: Shift-Click-Drag\tReset: Double-Click")

  ui_misc
}


#' Server function for delimted-file reader.
#'
#' Used to define the server within the \code{read_delim} shiny module.
#'
#' @family read_delim module functions
#
#' @param input   standard \code{shiny} input
#' @param output  standard \code{shiny} output
#' @param session standard \code{shiny} session
#' @param delim   character, default for parsing delimiter
#' @param decimal_mark character, default for decimal mark
#'
#' @return a \code{shiny::\link[shiny]{reactive}} containing a tbl_df of the parsed text
#'
#' @examples
#' shinyServer(function(input, output, session) {
#'
#'   rct_data <- callModule(
#'     module = dygraph_server,
#'     id = "foo"
#'   )
#'
#'   observe(print(rct_data()))
#' })
#'
#' @export
#
dygraph_server <- function(
  input, output, session,
  data) {

  # Make sure data is reactive
  if (!shiny::is.reactive(data)) {
    static_data <- data
    data <- function() static_data
  }

  # reactives
  rct_data <- reactive({

    shiny::validate(
      shiny::need(data(), "No data")
    )

    data()
  })

  rct_var_time <- reactive({
    var_time <- df_names_inherits(rct_data(), c("POSIXct"))

    shiny::validate(
      shiny::need(var_time, "Cannot display graph: dataset has no time variables")
    )

    var_time

  })

  rct_var_num <- reactive({
    var_num <- df_names_inherits(rct_data(), c("numeric", "integer"))

    shiny::validate(
      shiny::need(var_num, "Cannot display graph: dataset has no numeric variables")
    )

    var_num
  })

  rct_dyg <- reactive({

    shiny::req(rct_data())

    var_time <- selection$time
    var_y1 <- selection$Y1
    var_y2 <- selection$Y2

    shiny::validate(
      shiny::need(var_time, "Graph cannot display without a time-variable"),
      shiny::need(c(var_y1, var_y2), "Graph cannot display without any y-variables")
    )

    # create the mts object
    vec_time <- rct_data()[[var_time]]
    df_num <- rct_data()[c(var_y1, var_y2)]

    dy_xts <- xts::xts(df_num, order.by = vec_time, lubridate::tz(vec_time))

    dyg <- dygraphs::dygraph(dy_xts)
    #dyg <- do.call(dygraphs::dyOptions, c(list(dyg), rctval_dyopt[[item_dyopt]]))
    dyg <- dygraphs::dyAxis(dyg, "x", label = var_time)
    dyg <- dygraphs::dyAxis(dyg, "y", label = paste(var_y1, collapse = ", "))
    dyg <- dygraphs::dyAxis(dyg, "y2", label = paste(var_y2, collapse = ", "))

    # put stuff on y2 axis
    for(i in seq_along(var_y2)) {
      dyg <- dygraphs::dySeries(dyg, var_y2[i], axis = "y2")
    }

    dyg
  })

  selection <- reactiveValues(
    time = NULL,
    Y1 = NULL,
    Y2 = NULL
  )

  # observers
  shiny::observe({
    selection$time <- input[["controller_time"]]
    selection$Y1 <- input[["controller_y1"]]
    selection$Y2 <- input[["controller_y2"]]
  })

  # when the y-variables change,
  #   if there are y-varaiables available to select,
  #     and there are no y-variables selected:
  #     then - put the first y-variable on the Y1 axis
  shiny::observeEvent(
    eventExpr = rct_var_num(),
    handlerExpr = {
      if (is.null(selection$Y1) &&
          is.null(selection$Y2)   ){
        selection$Y1 <- rct_var_num()[[1]]
      }
    }
  )

  # outputs

  # select time variable
  output[["time_out"]] <-
    renderUI({
      ns <- session$ns
      selectizeInput(
        inputId = ns("controller_time"),
        label = "Time",
        choices = rct_var_time(),
        selected = selection$time
      )
    })

  # select Y1 variable
  output[["y1_out"]] <-
    renderUI({
      ns <- session$ns
      selectizeInput(
        inputId = ns("controller_y1"),
        label = "Y1 axis",
        choices = setdiff(rct_var_num(), input[["controller_y2"]]),
        multiple = TRUE,
        selected = selection$Y1
      )
    })

  # select Y2 variable
  output[["y2_out"]] <-
    renderUI({
      ns <- session$ns
      selectizeInput(
        inputId = ns("controller_y2"),
        label = "Y2 axis",
        choices = setdiff(rct_var_num(), input[["controller_y1"]]),
        multiple = TRUE,
        selected = selection$Y2
      )
    })

  # dygraph
  output[["view_dygraph"]] <- dygraphs::renderDygraph({rct_dyg()})

  return(rct_dyg)
}
