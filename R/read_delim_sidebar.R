#' Sidebar layout for read_delim module
#'
#' These functions return the ui elements for a side panel and a main panel.
#'
#' The side elements are the inputs; the main elements are the outputs.
#'
#' @param id character, used to identify a namespace
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#'
#' @examples
#' shinyUI(
#'   fluidPage(
#'     shinyjs::useShinyjs(),
#'     sidebarLayout(
#'       sidebarPanel(read_delim_sidebar_side("foo")),
#'       mainPanel(read_delim_sidebar_main("foo"))
#'     )
#'   )
#' )
#'
#' @export
#
read_delim_sidebar_side <- function(id){

  ns <- shiny::NS(id)

  sidebar_elems <- read_delim_ui_input(id)
  misc_elems <- read_delim_ui_misc(id)

  tz_modal <-
    bsplus::bs_modal(
      id = ns("tz_help"),
      title = "Timezones",
      size = "large",
      misc_elems$tz_help
    )

  # note: order is imporant here!
  #   - first, make hidden
  #   - then, bs_modal_helpify
  #
  sidebar_elems$delim         <- shinyjs::hidden(sidebar_elems$delim)
  sidebar_elems$decimal_mark  <- shinyjs::hidden(sidebar_elems$decimal_mark)
  sidebar_elems$tz_parse      <- shinyjs::hidden(sidebar_elems$tz_parse)
  sidebar_elems$tz_display    <- shinyjs::hidden(sidebar_elems$tz_display)

  sidebar_elems$tz_parse <-
    bsplus::bs_modal_helpify(
      input = sidebar_elems$tz_parse,
      bs_modal = tz_modal
    )

  sidebar_elems$tz_display <-
    bsplus::bs_modal_helpify(
      input = sidebar_elems$tz_display,
      bs_modal = tz_modal
    )

  sidebar_elems
}

#' @rdname read_delim_sidebar_side
#' @export
#
read_delim_sidebar_main <- function(id){

  main_elems <- read_delim_ui_output(id)

  main_elems$text <- shinyjs::hidden(main_elems$text)
  main_elems$data <- shinyjs::hidden(main_elems$data)

  main_elems
}

# note we are initializing the show/hide functions here, but controlling at the definition level

#' @seealso read_delim_sidebar_main
#' @rdname read_delim_server
#' @export
#
read_delim_sidebar_server <- function(
  input, output, session,
  delim = ",",
  decimal_mark = "."
){

  ## reactives ##
  ###############

  list_rct <- read_delim_server(input, output, session, delim, decimal_mark)

  rct_txt <- list_rct$rct_txt
  rct_data <- list_rct$rct_data

  ## observers ##
  ###############

  # shows and hides controls based on the availabilty and nature of data
  shiny::observe({

    has_text <- !is.null(rct_txt())
    has_data <- !is.null(rct_data())
    has_numeric <- length(df_names_inherits(rct_data(), "numeric")) > 0
    has_time_non_8601 <- df_has_time_non_8601(rct_txt(), delim = input$delim)
    has_time <- length(df_names_inherits(rct_data(), "POSIXct")) > 0

    shinyjs::toggle("text", condition = has_text)
    shinyjs::toggle("data", condition = has_data)
    shinyjs::toggle("delim", condition = has_data)
    shinyjs::toggle("decimal_mark", condition = has_numeric)
    shinyjs::toggle("tz_parse", condition = has_time_non_8601)
    shinyjs::toggle("tz_display", condition = has_time)

  })

  rct_data
}
