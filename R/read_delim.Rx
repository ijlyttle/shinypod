#' UI input elements for delimited-file reader.
#'
#' Used to define the UI input elements within the \code{read_delim} shiny module.
#'
#' This function returns a \code{shiny::\link[shiny]{tagList}} with members:
#'
#' \describe{
#'  \item{file}{\code{shiny::\link[shiny]{fileInput}}, used to specify file}
#'  \item{delim}{\code{shiny::\link[shiny]{selectizeInput}}, used to specify delimiter character}
#'  \item{decimal_mark}{\code{shiny::\link[shiny]{selectizeInput}}, used to specify decimal mark}
#'  \item{tz_parse}{\code{shiny::\link[shiny]{selectizeInput}}, used to specify timezone to parse}
#'  \item{tz_display}{\code{shiny::\link[shiny]{selectizeInput}}, used to specify timezone to display}
#'  \item{is_parse}{\code{shiny::\link[shiny]{checkboxInput}}, used to indicate if custom datetime-parsing is needed}
#'  \item{parse_column}{\code{shiny::\link[shiny]{selectizeInput}}, used to specify which column to parse}
#'  \item{parse_format}{\code{shiny::\link[shiny]{selectizeInput}}, used to specify a parsing format}
#'  \item{parse_locale}{\code{shiny::\link[shiny]{textInput}}, used to specify a language locale for parsing}
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

  # specify file
  ui_input$file <-
    shiny::fileInput(
      inputId = ns("file"),
      label = "File",
      accept = c("text/csv", ".csv", "text/comma-separated-values", "text/plain")
    )

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

  # custom parsing
  ui_input$is_parse <-
    shiny::checkboxInput(
      inputId = ns("is_parse"),
      label = "Custom datetime-parsing"
    )

  # column to parse to datetime
  ui_input$parse_column <-
    shiny::selectizeInput(
      inputId = ns("parse_column"),
      label = "Column to parse",
      choices = "",
      selected = ""
    )

  # ui_input$parse_lang <-
  #   shiny::textInput(
  #     inputId = ns("parse_lang"),
  #     label = "Language locale",
  #     value = "en"
  #   )

  ui_input$parse_format <-
    shiny::selectizeInput(
      inputId = ns("parse_format"),
      label = "Format to parse",
      choices = .choices_format(),
      selected = ""
    )

  ui_input
}

#' UI output elements for delimited-file reader.
#'
#' Used to define the UI output elements within the \code{read_delim} shiny module.
#'
#' This function returns a \code{shiny::\link[shiny]{tagList}} with members:
#'
#' \describe{
#'  \item{status}{\code{shiny::\link[shiny]{htmlOutput}}, used to display status of the module}
#'  \item{text}{\code{shiny::\link[shiny]{htmlOutput}}, used to display first few lines of text from file}
#'  \item{data}{\code{shiny::\link[shiny]{htmlOutput}}, used to display first few lines of the parsed dataframe}
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
      container = pre_scroll
    )

  # text output
  ui_output$text <-
    shiny::htmlOutput(
      outputId = ns("text"),
      container = pre_scroll
    )

  # data-frame output
  ui_output$data <-
    shiny::htmlOutput(
      outputId = ns("data"),
      container = pre_scroll
    )

  ui_output
}

#' UI miscellaneous elements for delimited-file reader.
#'
#' Used to define the UI miscellaneous elements within the \code{read_delim} shiny module.
#'
#' This function returns a \code{shiny::\link[shiny]{tagList}} with members:
#'
#' \describe{
#'  \item{tz_help}{\code{htmltools::\link[htmltools]{HTML}}, contains help for time parsing
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

  ui_misc <- shiny::tagList()

  ui_misc$tz_help <-
    htmltools::HTML(
      readr::read_lines(
        system.file("help", "read_delim", "tz.html", package = "shinypod")
      )
    )

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
#' @return a list with members:
#' \code{rct_txt} \code{shiny::\link[shiny]{reactive}}, returns raw text
#' \code{rct_data} \code{shiny::\link[shiny]{reactive}}, returns tbl_df of the parsed text
#'
#' @examples
#' shinyServer(function(input, output, session) {
#'
#'   list_rct <- callModule(
#'     module = read_delim_server,
#'     id = "foo"
#'   )
#'
#'   observe(print(list_rct$rct_data()))
#' })
#'
#' @export
#
read_delim_server <- function(
  input, output, session,
  delim = ",",
  decimal_mark = "."
){

  ns <- session$ns

  ## input updates ##
  ###################

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

  ## reactives ##
  ###############

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

  rct_tz_display <- reactive({

    result <- input$tz_display
    if (!result %in% c("UTC", lubridate::olson_time_zones())){
      result <- "UTC"
    }

    result
  })

  # reactive to read in the raw text from the file-specification input
  rct_txt <- reactive({

    shiny::validate(
      shiny::need(input$file, "File not selected")
    )

    infile <- input$file$datapath

    readr::read_file(infile)
  })

  rct_data_provisional <- reactive({

    df <-
      readr::read_delim(
        file = rct_txt(),
        delim = rct_delim(),
        locale = readr::locale(
          decimal_mark = rct_decimal_mark(),
          tz = rct_tz_parse()
        )
      )

    df <- lubridate::with_tz(df, tzone = rct_tz_display())

    shiny::validate(
      shiny::need(is.data.frame(df), "No data")
    )

    df
  })

  rct_parse_column <- reactive({

    shiny::validate(
      shiny::need(input$is_parse, message = "parsing not selected"),
      shiny::need(input$parse_column, label = "parsing column")
    )

    rct_data_provisional()[[input$parse_column]]
  })

  rct_parse_column_dtm <- reactive({

    shiny::validate(shiny::need(rct_parse_column(), label = "parsing column"))

    if (identical(class(rct_parse_column()), "character") ){
      dtm <-
        .parse_datetime(
          rct_parse_column(),
          format = input$parse_format,
          tz = rct_tz_parse()
        )
    } else {
      # this is a numeric column - we are parsing as seconds since the epoch
      dtm <- as.POSIXct(rct_parse_column(), origin = "1970-01-01", tz = "UTC")
    }

    dtm <- lubridate::with_tz(dtm, tzone = rct_tz_display())

    dtm
  })

  rct_data <- reactive({
    df <- rct_data_provisional()

    if (isValidy(rct_parse_column_dtm()) && isValidy(input$parse_column)) {
      df[[input$parse_column]] <- rct_parse_column_dtm()
    }

    df
  })

  rct_state = reactive({
    list(
      has_data = isValidy(rct_data()),
      has_txt = isValidy(rct_txt()),
      has_delim = isValidy(rct_delim()),
      has_decimal_mark = isValidy(rct_decimal_mark()),
      has_tz_parse = isValidy(rct_tz_parse()),
      has_tz_display = isValidy(rct_tz_display()),
      has_numeric =
        isValidy(length(df_names_inherits(rct_data(), "numeric")) > 0),
      has_time_non_8601 =
        isValidy(df_has_time_non_8601(rct_txt(), delim = input$delim)),
      has_time =
        isValidy(length(df_names_inherits(rct_data(), "POSIXct")) > 0)
    )
  })

  # status
  rctval_status <-
    shiny::reactiveValues(
      input = list(index = 0, is_valid = NULL, message = NULL),
      result = list(index = 0, is_valid = NULL, message = NULL)
    )

  rct_status_content <- shiny::reactive(status_content(rctval_status))

  ## observers ##
  ###############

  # input
  observeEvent(
    eventExpr = {
      input$file
      rct_state()
    },
    handlerExpr = {

      rctval_status$input$index <- rctval_status$input$index + 1

      if (is.null(input$file)){
        rctval_status$input$is_valid <- FALSE
        rctval_status$input$message <- "Please select a file"
      } else if (!rct_state()$has_delim){
        rctval_status$input$is_valid <- FALSE
        rctval_status$input$message <- "Please select a delimiter"
      } else if (!rct_state()$has_decimal_mark){
        rctval_status$input$is_valid <- FALSE
        rctval_status$input$message <- "Please select a decimal mark"
      } else if (!rct_state()$has_tz_parse){
        rctval_status$input$is_valid <- FALSE
        rctval_status$input$message <- "Please select a timezone for parsing"
      } else if (!rct_state()$has_tz_display){
        rctval_status$input$is_valid <- FALSE
        rctval_status$input$message <- "Please select a timezone for display"
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
    eventExpr = {
      rct_data()
    },
    handlerExpr = {

      rctval_status$result$index <- rctval_status$input$index

      if (is.null(input$file$datapath)){
        rctval_status$result$is_valid <- FALSE
        rctval_status$result$message <- paste("Cannot find file:", input$file$name)
      } else {
        rctval_status$result$is_valid <- TRUE
        rctval_status$result$message <- paste("Uploaded and parsed file:", input$file$name)
      }

    }
  )

  # observe(print(paste(rctval_status$input$index, rctval_status$result$index)))

  # updates the display tz if the parse tz changes
  shiny::observeEvent(
    eventExpr = input$tz_parse,
    handlerExpr = {
      shiny::updateSelectInput(
        session,
        inputId = "tz_display",
        selected = input$tz_parse
      )
    }
  )

  # updates the columns for custom-parsing
  shiny::observeEvent(
    eventExpr = rct_data_provisional(),
    handlerExpr = {
      col_names <- df_names_inherits(rct_data_provisional(), c("numeric", "integer", "character"))
      shiny::updateSelectInput(
        session,
        inputId = "parse_column",
        choices = col_names,
        selected = ""
      )
    }
  )

  observe_class_swap(id = "status", rct_status_content()$class)

  # observer to toggle the activation of the custom-parsing inputs
  observeEvent(
    eventExpr = {
      input$is_parse
      isValidy(rct_parse_column())
    },
    handlerExpr = {

      is_char <-
        isValidy(rct_parse_column()) &&
        identical(class(rct_parse_column()), "character")

      shinyjs::toggleState(
        id = "parse_column",
        condition = input$is_parse
      )

      shinyjs::toggleState(
        id = "parse_format",
        condition = is_char
      )

      shinyjs::toggleState(
        id = "parse_locale",
        condition = is_char
      )
    }
  )

  ## outputs ##
  #############

  output$status <-
    shiny::renderText(rct_status_content()$message)

  # sets the output for the raw text
  output$text <-
    shiny::renderUI({text_html(rct_txt())})

  # sets the output for the parsed dataframe
  output$data <- shiny::renderUI({tibble_html(rct_data())})

  # returns a list
  list(rct_data = rct_data, rct_state = rct_state)
}
