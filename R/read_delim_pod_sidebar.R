#' Sidebar layout for read_delim module
#'
#' These functions return the ui elements for a side panel and a main panel.
#'
#' Generally, the side elements are the inputs; the main elements are the outputs.
#'
#' @family read_delim module functions
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

  sidebar_elems
}

#' @rdname read_delim_sidebar_side
#' @export
#
read_delim_sidebar_main <- function(id){

  main_elems <- read_delim_ui_output(id)

  main_elems
}

#' @rdname read_delim_server
#' @export
#
read_delim_sidebar_server <- function(
  input, output, session,
  text, delim = ",", decimal_mark = "."
){

  ## reactives ##
  ###############

  list_rct <- read_delim_server(input, output, session, text)

  rct_data <- list_rct$rct_result
  rct_input_state <- list_rct$rct_input_state
  rct_status_content <- list_rct$rct_status_content

  ## observers ##
  ###############

  # shows and hides controls based on the availabilty and nature of data
  shiny::observe({
    # outputs
    shinyjs::toggle(
      "data_preview",
      condition = shinypod::isValidy(rct_data())
    )
  })

  # change the class of the status window
  shinypod::observe_class_swap(id = "status", rct_status_content()$class)

  rct_data
}
