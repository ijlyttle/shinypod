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
write_delim_ui_input <- function(id) {
  ns <- shiny::NS(id)
  ui_input <- shiny::tagList()

  ui_input$delim <-
    shiny::selectizeInput(
      inputId = ns("delim"),
      label = "Delimiter",
      choices = c(Comma = ",", Semicolon = ";", Tab = "\t")
    )

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
write_delim_ui_output <- function(id) {
  ns <- shiny::NS(id)

  ## ui_view ##
  ui_output <- shiny::tagList()

  ui_output$status <-
    shiny::htmlOutput(
      outputId = ns("status"),
      container = pre_scroll
    )

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

  ui_output
}

#' Server function for delimted-file writer.
#'
#' Used to define the server within the \code{write_delim} shiny module.
#'
#' @family write_delim module functions
#
#' @param input        standard \code{shiny} input
#' @param output       standard \code{shiny} output
#' @param session      standard \code{shiny} session
#' @param data         data.frame
#' @param filename     path
#' @param delim        character, possibly reactive, delimiter mark to use as a default
#' @param status_show  logical, possibly reactive, indicates if to show status
#' @param status_alert logical, possibly reactive, indicates if to change alert-class of status output
#'
#' @return a \code{shiny::\link[shiny]{reactive}} containing a tbl_df of the parsed text
#'
#' @examples
#' library("shiny")
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
  input, output, session,
  data,
  filename = "data.csv",
  delim = ",",
  status_alert = TRUE
) {

  ns <- session$ns

  # reactives
  rct_data <- shiny::reactive({

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

  rct_filename_default <- shiny::reactive({

    if (shiny::is.reactive(filename)) {
      static_filename = filename()
    } else {
      static_filename = filename
    }

    static_filename
  })

  rct_delim_default <- shiny::reactive({
    static(delim)
  })

  rct_static_alert <- shiny::reactive({
    static(status_alert)
  })

  rct_txt <- shiny::reactive({

    shiny::validate(
      shiny::need(input$delim, "No delimiter")
    )

    txt <-
      readr::format_delim(
        x = rct_data(),
        delim = input$delim
      )

    # put here for compatibility with Windows
    txt <- stringr::str_replace_all(txt, pattern = "\n", replacement = "\r\n")

    txt
  })

  rct_filename <- shiny::reactive({

    # just for the reactive dependency (why?)
    # rct_data()

    # http://stackoverflow.com/questions/17262493/
    # check for /\:*?"<>|

    shiny::validate(
      shiny::need(
        input$file,
        "Need a valid filename"
      )
    )

    input$file
  })

  rct_state = shiny::reactive({
    list(
      has_data = isValidy(rct_data()),
      has_txt = isValidy(rct_txt()),
      has_filename = isValidy(rct_filename())
    )
  })

  # #downloads
  rctval <- shiny::reactiveValues(download = 0)

  # status
  rctval_status <-
    shiny::reactiveValues(
      input = list(index = 0, is_valid = NULL, message = NULL),
      result = list(index = 0, is_valid = NULL, message = NULL)
    )

  rct_status_content <- shiny::reactive(status_content(rctval_status))

  ## observers ##
  ###############

  # update filename input
  shiny::observeEvent(
    eventExpr = rct_filename_default(),
    handlerExpr = {
      shiny::updateTextInput(
        session,
        inputId = "file",
        value = rct_filename_default()
      )
    }
  )

  # input
  shiny::observeEvent(
    eventExpr = {
      rct_state()$has_data
      input$delim
      input$file
    },
    handlerExpr = {

      rctval_status$input$index <- rctval_status$input$index + 1

      if (!rct_state()$has_data){
        rctval_status$input$is_valid <- FALSE
        rctval_status$input$message <- "No data are available"
      } else if (!isValidy(input$delim)){
        rctval_status$input$is_valid <- FALSE
        rctval_status$input$message <- "Please specify a delimiter"
      } else if (!isValidy(rct_filename())){
        rctval_status$input$is_valid <- FALSE
        rctval_status$input$message <- "Please specify a valid filename"
      } else {
        rctval_status$input$is_valid <- TRUE
        rctval_status$input$message <-
          paste("Ready to download file", rct_filename(), sep = ": ")
      }

    },
    ignoreNULL = FALSE, # makes sure we evaluate on initialization
    priority = 1 # always execute before others
  )

  # result
  # this code will not be useful until we can observe a download button
  # being clicked
  # observeEvent(
  #   eventExpr = output$download,
  #   handlerExpr = {
  #
  #     rctval_status$result$index <- rctval_status$input$index
  #
  #     # does downloadHandler give us some indication of success?
  #     rctval_status$result$is_valid <- TRUE
  #     rctval_status$result$message <- paste("Downloaded file:", rct_filename())
  #
  #     # if (is.null(input$file$datapath)){
  #     #   rctval_status$result$is_valid <- FALSE
  #     #   rctval_status$result$message <- paste("Cannot find file:", input$file$name)
  #     # } else {
  #     #   rctval_status$result$is_valid <- TRUE
  #     #   rctval_status$result$message <- paste("Uploaded file:", input$file$name)
  #     # }
  #
  #   }
  # )

  shiny::observe(
    shiny::updateSelectizeInput(
      session,
      inputId = "delim",
      selected = update_selected(rct_delim_default(), c(",", ";", "\t"))
    )
  )

  shiny::observe({
    shinyjs::toggleState(id = "delim", condition = rct_state()$has_data)
    shinyjs::toggleState(id = "file", condition = rct_state()$has_data)
    shinyjs::toggleState(
      id = "download",
      condition = rct_state()$has_txt && rct_state()$has_filename
    )
  })

  shiny::observe({
    if (rct_static_alert()){
      observe_class_swap(id = "status", rct_status_content()$class)
    }
  })


  ## outputs ##
  #############

  # sets the output for the status
  output$status <-
    shiny::renderText(rct_status_content()$message)

  # sets the output for the input dataframe
  output[["text_data"]] <-
    shiny::renderUI({
      h <-
      withr::with_options(
          list(width = 10000, dpylr.width = Inf, dplyr.print_min = 6),
          utils::capture.output(print(rct_data()))
        )
      h <- paste(h, collapse = "<br/>")
      h <- htmltools::HTML(h)

      h
    })

  # sets the output for the raw text
  output[["text_preview"]] <-
    shiny::renderUI({
      h <- rct_txt()
      h <- readr::read_lines(h, n_max = 7)
      h <- paste(h, collapse = "<br/>")
      h <- htmltools::HTML(h)

      h
    })


  # do the download
  output$download <-
    shiny::downloadHandler(
      filename = rct_filename,
      content = function(con){
        writeChar(rct_txt(), con)
      },
      contentType = "text/csv"
    )



  result <- list(
    rct_data = rct_data,
    rct_state = rct_state
  )

  result
}
