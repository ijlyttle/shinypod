#' UI input elements for module that parses a delimited text file.
#'
#' Used to define the UI input elements within the \code{ read_delim } shiny module.
#'
#' This function returns a \code{shiny::\link[shiny]{tagList}} with members:
#'
#' \describe{
#'  \item{delim}{\code{shiny::\link[shiny]{selectizeInput}}, used to specify delimiter character}
#'  \item{decimal_mark}{\code{shiny::\link[shiny]{selectizeInput}}, used to specify decimal mark}
#'  \item{tz_parse}{\code{shiny::\link[shiny]{selectizeInput}}, used to specify timezone to parse}
#'  \item{tz_display}{\code{shiny::\link[shiny]{selectizeInput}}, used to specify timezone to display}
#' }
#'
#' The purpose is to specify the UI elements - another set of functions can be used to specify layout.
#'
#' @family read_delim module functions
#
#' @param id, character used to specify namesapce, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#'
#' @export
#
read_delim_ui_input <- function(id){

  ns <- shiny::NS(id)

  ui_input <- shiny::tagList()

  # specify delim
  ui_input$delim <-
    shiny::selectizeInput(
      inputId = ns("delim"),
      label = "Delimiter",
      choices = c(Comma = ",", Semicolon = ";", Tab = "\t")
    )

  # specify decimal
  ui_input$decimal_mark <-
    shiny::selectizeInput(
      inputId = ns("decimal_mark"),
      label = "Decimal mark",
      choices = c(Point = ".", Comma = ",")
    )

  # specify timezones
  tz_choice <- c("UTC", lubridate::olson_time_zones())

  # timezone to parse
  ui_input$tz_parse <-
    shiny::selectizeInput(
      inputId = ns("tz_parse"),
      label = "Timezone to parse",
      choices = tz_choice
    )

  # timezone to display
  ui_input$tz_display <-
    shiny::selectizeInput(
      inputId = ns("tz_display"),
      label = "Timezone to display",
      choices = tz_choice
    )

  # # column for additional parsing
  # ui_input$parse_column <-
  #   shiny::selectizeInput(
  #     inputId = ns("parse_column"),
  #     label = "Additional datetime columns",
  #     choices = "",
  #     selected = "",
  #     multiple = TRUE
  #   )
  #
  # # format to parse
  # ui_input$parse_format <-
  #   shiny::selectizeInput(
  #     inputId = ns("parse_format"),
  #     label = "Format to parse",
  #     choices = .choices_format(),
  #     selected = ""
  #   )

  ui_input
}

#' UI output elements for module that parses a delimited text file.
#'
#' Used to define the UI output elements within the \code{ read_delim } shiny module.
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
#' @family read_delim module functions
#
#' @param id, character used to specify namesapce, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#'
#' @export
#
read_delim_ui_output <- function(id){

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

#' UI miscellaneous elements for module that parses a delimited text file.
#'
#' Used to define the UI miscellaneous elements within the \code{ read_delim } shiny module.
#'
#' This function returns a \code{shiny::\link[shiny]{tagList}} with members:
#'
#' \describe{
#' }
#'
#' The purpose is to specify the UI elements - another set of functions can be used to specify layout.
#'
#' @family read_delim module functions
#
#' @param id, character used to specify namesapce, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#'
#' @export
#
read_delim_ui_misc <- function(id){

  # this is for elements that are neither inputs nor outputs

  ui_misc <- shiny::tagList()

  ui_misc
}

#' Server function for module that parses a delimited text file.
#'
#' Used to define the server within the \code{ read_delim } shiny module.
#'
#' @family read_delim module functions
#
#' @param input        standard \code{shiny} input
#' @param output       standard \code{shiny} output
#' @param session      standard \code{shiny} session
#' @param data         data.frame or a reactive that returns a data.frame
#' @param delim        character, defualt delimiter
#' @param decimal_mark character, default decimal-mark
#'
#' @return \describe{
#'   \item{\code{ read_delim_server}}{a list containing:
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
#'   \item{\code{ read_delim_sidebar_server}}{a reactive that returns a data.frame}
#' }
#'
#' @examples
#' shinyServer(function(input, output, session) {
#'
#'   list_rct <- callModule(
#'     module = read_delim_sidebar_server,
#'     id = "foo",
#'     data = iris
#'   )
#'
#' })
#'
#' @export
#
read_delim_server <- function(
  input, output, session,
  text, delim = ",", decimal_mark = "."
){

  ns <- session$ns

  ## defaults for inputs ##
  #########################

  shiny::updateSelectizeInput(
    session,
    inputId = "delim",
    selected = delim
  )

  shiny::updateSelectizeInput(
    session,
    inputId = "decimal_mark",
    selected = decimal_mark
  )

  ## functions ##
  ###############

  fn_parse <- function(text, delim, decimal_mark, tz){

    locale <- readr::locale(decimal_mark = decimal_mark, tz = tz)

    readr::read_delim(text, delim = delim, locale = locale)
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

  rct_text <-
    shinypod::reactive_validate(text, is.character, message = "Please supply text to parse")

  rct_delim <- reactive({

    shiny::validate(
      shiny::need(input$delim, message = "Need a delimiter")
    )

    input$delim
  })

  rct_decimal_mark <- reactive({

    shiny::validate(
      shiny::need(input$decimal_mark, message = "Need a decimal mark")
    )

    input$decimal_mark
  })

  rct_tz_parse <- reactive({

    result <- input$tz_parse
    if (!result %in% c("UTC", lubridate::olson_time_zones())){
      result <- "UTC"
    }

    result
  })

  rct_tz_display <- reactive({

    result <- input$tz_display
    if (!result %in% c("UTC", lubridate::olson_time_zones())){
      result <- "UTC"
    }

    result
  })

  rct_input_state <-
    shiny::reactive({
      list(
        has_text = shinypod::isValidy(rct_text()),
        has_delim = shinypod::isValidy(rct_delim()),
        has_decimal_mark = shinypod::isValidy(rct_decimal_mark()),
        has_tz_parse = shinypod::isValidy(rct_tz_parse()),
        has_tz_display = shinypod::isValidy(rct_tz_display())
        # has_parse_column = shinypod::isValidy(input$parse_column),
        # has_parse_format = shinypod::isValidy(input$parse_format)
      )
    })

  # rct_parse_lang <- reactive({
  #
  #   shiny::validate(
  #     shiny::need(
  #       input$parse_lang %in% c(readr::date_names_langs(), ""),
  #       message = "locale language not recognized"
  #     )
  #   )
  #
  #   input$parse_lang
  # })

  rct_data_provisional <-
    shiny::reactive({
      fn_parse(
        rct_text(),
        delim = rct_delim(),
        decimal_mark = rct_decimal_mark(),
        tz = rct_tz_parse()
      )
    })

  rct_data <-
    shiny::reactive({

      df <- rct_data_provisional()
      # put column-parsing datetime function here
      df <- lubridate::with_tz(df, tzone = rct_tz_display())

      df
    })

  rct_status_content <- shiny::reactive(shinypod::status_content(rctval_status))

  ## input-update observers ##
  ############################

  # updates the display tz if the parse tz changes
  shiny::observeEvent(
    eventExpr = rct_tz_parse(),
    handlerExpr = {
      shiny::updateSelectInput(
        session,
        inputId = "tz_display",
        selected = rct_tz_parse()
      )
    }
  )

  # updates the columns for custom-parsing
  shiny::observeEvent(
    eventExpr = rct_data_provisional(),
    handlerExpr = {
      col_names <-
        df_names_inherits(
          rct_data_provisional(),
          c("numeric", "integer", "character")
        )
      shiny::updateSelectInput(
        session,
        inputId = "parse_column",
        choices = col_names,
        selected = ""
      )
    }
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

      if (!state$has_text){
        is_valid <- FALSE
        message <- "Please supply text to parse"
      } else if (!state$has_delim){
        is_valid <- FALSE
        message <- "Please supply a delimiter"
      } else if (!state$has_decimal_mark){
        is_valid <- FALSE
        message <- "Please supply a decimal mark"
      } else if (!state$has_tz_parse){
        is_valid <- FALSE
        message <- "Please select a timezone for parsing"
      } else if (!state$has_tz_display){
        is_valid <- FALSE
        message <- "Please select a timezone for display"
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
    eventExpr = rct_data(),
    handlerExpr = {

      # default
      is_valid <- TRUE
      message <- "Text parsed"

      if (!shinypod::isValidy(rct_data())){
        is_valid <- FALSE
        message <- "Cannot parse text"
      }

      rctval_status$result$index <- rctval_status$input$index
      rctval_status$result$is_valid <- is_valid
      rctval_status$result$message <- message
    }
  )

  ## outputs ##
  #############

  output$status <- shiny::renderText(rct_status_content()$message)

  output$data_preview <- shiny::renderUI(shinypod::tibble_html(rct_data()))

  # returns a list
  list(
    rct_result = rct_data,
    rct_input_state = rct_input_state,
    rct_status_content = rct_status_content
  )
}
