#' UI input elements for delimited-file writer.
#'
#' Used to define the UI input elements within the \code{write_delim} shiny module.
#'
#' This function returns a \code{shiny::\link[shiny]{tagList}} with members:
#'
#' \describe{
#'  \item{delim}{\code{shiny::\link[shiny]{selectizeInput}}, used to specify delimiter character}
#'  \item{filename}{\code{shiny::\link[shiny]{textInput}}, used to specify file name}
#'  \item{download}{\code{shiny::\link[shiny]{downloadButton}}, download button}
#' }
#'
#' The purpose is to specify the UI elements - another set of functions can be used to specify layout.
#'
#' @family write_delim module functions
#
#' @param id, character used to specify namesapce, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#'
#' @export
#
write_delim_input <- function(id) {
  ns <- NS(id)
  ui_input <- shiny::tagList()

  ui_input$delim <- 
    shiny::uiOutput(ns("controller_delim"))

  # specify filename
  ui_input$filename <-
    shiny::textInput(
      inputId = ns("file"),
      label = "Filename",
      value = "data.csv"
    )

  # download button
  ui_input$download <-
    shiny::downloadButton(
      outputId = ns("download"),
      label = "Download",
      class = "btn-primary"
    )

  ui_input
}


#' UI output elements for delimited-file writer.
#'
#' Used to define the UI output elements within the \code{write_delim} shiny module.
#'
#' This function returns a \code{shiny::\link[shiny]{tagList}} with members:
#'
#' \describe{
#'  \item{data}{\code{shiny::\link[shiny]{htmlOutput}}, used to display first few lines of the dataframe}
#'  \item{text}{\code{shiny::\link[shiny]{htmlOutput}}, used to display first few lines of text from file}
#'  \item{text}{\code{shiny::\link[shiny]{htmlOutput}}, used to display first text status }
#' }
#'
#' The purpose is to specify the UI elements - another set of functions can be used to specify layout.
#'
#' @family write_delim module functions
#
#' @param id, character used to specify namesapce, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#'
#' @export
#
write_delim_output <- function(id) {
  ns <- NS(id)

  ## ui_view ##
  ui_output <- shiny::tagList()

  # shows the first few lines of the data-frame
  ui_output$data <-
    shiny::htmlOutput(
      outputId = ns("text_data"),
      container = pre_scroll
    )

  # shows the raw text of the file (first few lines)
  ui_output$text <-
    shiny::htmlOutput(
      outputId = ns("text_preview"),
      container = pre_scroll
    )

  ui_output$status <-
    shiny::htmlOutput(
      outputId = ns("text_status"),
      container = pre_scroll
    )

  ui_output
}

#' Server function for delimted-file writer.
#'
#' Used to define the server within the \code{write_delim} shiny module.
#'
#' @family write_delim module functions
#
#' @param input   standard \code{shiny} input
#' @param output  standard \code{shiny} output
#' @param session standard \code{shiny} session
#' @param data    data.frame
#' @param delim   character, delimiter mark
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
write_delim_server <- function(
  input, output, session, data, delim = ","
) {

  ns <- session$ns

  # reactives
  rct_data <- reactive({
    if (shiny::is.reactive(data)) {
      static_data = data()
    } else {
      static_data = data
    }

    shiny::validate(
      shiny::need(is.data.frame(static_data), "No data")
    )

    dplyr::tbl_df(static_data)
  })

  rct_txt <- reactive({
    shinyjs::disable(ns("file"))
    shinyjs::disable(ns("download"))

    shiny::validate(
      shiny::need(rct_data(), "No data")
    )

    shinyjs::enable(ns("file"))
    shinyjs::enable(ns("download"))

    txt <-
      readr::format_delim(
        x = rct_data(),
        delim = input[["delim"]]
      )

    txt <- stringr::str_replace_all(txt, pattern = "\n", replacement = "\r\n")

    txt
  })

  rct_filename <- reactive({
    shinyjs::disable(ns("download"))

    # just for the reactive dependency
    rct_data()

    shiny::validate(
      shiny::need(
        input[["file"]],
        "Need a valid filename"
      )
    )

    shinyjs::enable(ns("download"))
    input[["file"]]
  })

  #render UIs
  output[["controller_delim"]] <-
    renderUI({
      #ns <- session$ns
      shiny::selectizeInput(
        inputId = ns("delim"),
        label = "Delimiter",
        choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
        selected = defaults$delim
      )
    })

  # outputs

  # sets the output for the input dataframe
  output[["text_data"]] <-
    renderUI({
      h <-
      devtools::with_options(
          list(width = 10000, dpylr.width = Inf, dplyr.print_min = 6),
          capture.output(print(rct_data()))
        )
      h <- paste(h, collapse = "<br/>")
      h <- htmltools::HTML(h)

      h
    })

  # sets the output for the raw text
  output[["text_preview"]] <-
    renderUI({
      h <- rct_txt()
      h <- readr::read_lines(h, n_max = 7)
      h <- paste(h, collapse = "<br/>")
      h <- htmltools::HTML(h)

      h
    })

  # sets the output for the status
  output[["text_status"]] <-
    renderUI({
      paste(
        "Ready to download file",
        paste0("\"", rct_filename(), "\""),
        sep = ": "
      )
    })

  # do the download
  output[["download"]] <-
    shiny::downloadHandler(
      filename = rct_filename,
      content = function(con){
        writeChar(rct_txt(), con)
      },
      contentType = "text/csv"
    )

  return(rct_data)
}