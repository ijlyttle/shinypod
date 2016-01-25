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
#'  \item{tz_parse_modal}{\code{shinyBS::\link[shinyBS]{bsModal}}, used explain timezone-parsing}
#'  \item{tz_display}{\code{shiny::\link[shiny]{selectizeInput}}, used to specify timezone to display}
#'  \item{tz_display_modal}{\code{shinyBS::\link[shinyBS]{bsModal}}, used explain timezone-parsing}
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
    shinyjs::hidden(
      shiny::selectizeInput(
        inputId = ns("delim"),
        label = "Delimiter",
        choices = c(Comma = ",", Semicolon = ";", Tab = "\t")
      )
    )


  # specify decimal
  ui_input$decimal_mark <-
    shinyjs::hidden(
      shiny::selectizeInput(
        inputId = ns("decimal_mark"),
        label = "Decimal mark",
        choices = c(Point = ".", Comma = ",")
      )
    )


  # specify timezones
  tz_choice <- c("UTC", lubridate::olson_time_zones())

  # timezone to parse
  ui_input$tz_parse <-
    shinyjs::hidden(
      shiny::selectizeInput(
        inputId = ns("tz_parse"),
        label = shiny::tags$span(
          shiny::tags$a(
            id = ns("tz_parse_link"),
            "Timezone to parse",
            shiny::icon("info-circle")
          )
        ),
        choices = tz_choice
      )
    )


  ui_input$tz_parse_modal <-
    shinyBS::bsModal(
      id = ns("tz_parse_modal"),
      title = "Timezones",
      trigger = ns("tz_parse_link"),
      size = "large",
      shiny::HTML(
        readr::read_lines(
          system.file("help", "read_delim", "tz.html", package = "shinypod")
        )
      )
    )

  # timezone to display
  ui_input$tz_display <-
    shinyjs::hidden(
      shiny::selectizeInput(
        inputId = ns("tz_display"),
        label = shiny::tags$span(
          shiny::tags$a(
            id = ns("tz_display_link"),
            "Timezone to display",
            shiny::icon("info-circle")
          )
        ),
        choices = tz_choice
      )
    )

  ui_input$tz_display_modal <-
    shinyBS::bsModal(
      id = ns("tz_display_modal"),
      title = "Timezones",
      trigger = ns("tz_display_link"),
      size = "large",
      shiny::HTML(
        readr::read_lines(
          system.file("help", "read_delim", "tz.html", package = "shinypod")
        )
      )
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
#'     module = read_delim_server,
#'     id = "foo"
#'   )
#'
#'   observe(print(rct_data()))
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

  # reactive to read in the raw text from the file-specification input
  rct_txt <- reactive({

    shiny::validate(
      shiny::need(input$file, "File not selected")
    )

    infile <- input$file$datapath

    readr::read_file(infile)
  })

  rct_data <- reactive({

    df <-
      readr::read_delim(
        file = rct_txt(),
        delim = input$delim,
        locale = readr::locale(
          decimal_mark = input$decimal_mark,
          tz = input$tz_parse
        )
      )

    df <- df_with_tz(df, tz = input$tz_display)

    shiny::validate(
      shiny::need(is.data.frame(df), "No data")
    )

    df
  })

  ## observers ##
  ###############

  shiny::observe({

    # shows and hides controls based on the availabilty and nature of data

    has_data <- !is.null(rct_data())
    has_numeric <- length(df_names_inherits(rct_data(), "numeric")) > 0
    has_time_non_8601 <- df_has_time_non_8601(rct_txt(), delim = input$delim)
    has_time <- length(df_names_inherits(rct_data(), "POSIXct")) > 0

    shinyjs::toggle("delim", condition = has_data)
    shinyjs::toggle("decimal_mark", condition = has_numeric)
    shinyjs::toggle("tz_parse", condition = has_time_non_8601)
    shinyjs::toggle("tz_display", condition = has_time)

  })


  ## outputs ##
  #############

  # sets the output for the raw text
  output$text <-
    shiny::renderUI({

      shiny::validate(
        shiny::need(rct_txt(), "File did not load properly")
      )

      h <- rct_txt()
      h <- readr::read_lines(h, n_max = 7)
      h <- paste(h, collapse = "<br/>")
      h <- shiny::HTML(h)

      h
    })

  # sets the output for the parsed dataframe
  output$data <-
    shiny::renderUI({

      h <-
        withr::with_options(
          list(width = 10000, dpylr.width = Inf, dplyr.print_min = 6),
          capture.output(print(rct_data()))
        )
      h <- paste(h, collapse = "<br/>")
      h <- shiny::HTML(h)

      h
    })


  # returns a dataframe
  rct_data
}
