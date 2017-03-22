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
  # not necessary to hide - observe-toggle takes care of it
  #
  # sidebar_elems$delim         <- shinyjs::hidden(sidebar_elems$delim)
  # sidebar_elems$decimal_mark  <- shinyjs::hidden(sidebar_elems$decimal_mark)
  # sidebar_elems$tz_parse      <- shinyjs::hidden(sidebar_elems$tz_parse)
  # sidebar_elems$tz_display    <- shinyjs::hidden(sidebar_elems$tz_display)

  # sidebar_elems$tz_parse <-
  #   bsplus::bs_modal_helpify(
  #     input = sidebar_elems$tz_parse,
  #     bs_modal = tz_modal
  #   )
  #
  # sidebar_elems$tz_display <-
  #   bsplus::bs_modal_helpify(
  #     input = sidebar_elems$tz_display,
  #     bs_modal = tz_modal
  #   )

  sidebar_elems$tz_parse <- sidebar_elems$tz_parse
  sidebar_elems$tz_display <- sidebar_elems$tz_display

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

  rct_data <- list_rct$rct_data
  rct_state <- list_rct$rct_state

  ## observers ##
  ###############

  # shows and hides controls based on the availabilty and nature of data
  shiny::observe({
    # inputs
    shinyjs::toggle("delim", condition = rct_state()$has_txt)
    shinyjs::toggle("decimal_mark", condition = rct_state()$has_txt)
    shinyjs::toggle("tz_parse", condition = rct_state()$has_time_non_8601)
    shinyjs::toggle("tz_display", condition = rct_state()$has_time)
    # outputs
    shinyjs::toggle("text", condition = rct_state()$has_txt)
    shinyjs::toggle("data", condition = rct_state()$has_data)
  })

  rct_data
}
