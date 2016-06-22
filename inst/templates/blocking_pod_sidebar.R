#' Sidebar layout for {{{ name }}} module
#'
#' These functions return the ui elements for a side panel and a main panel.
#'
#' Generally, the side elements are the inputs; the main elements are the outputs.
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
#'       sidebarPanel({{{ name }}}_sidebar_side("foo")),
#'       mainPanel({{{ name }}}_sidebar_main("foo"))
#'     )
#'   )
#' )
#'
#' @export
#
{{{ name }}}_sidebar_side <- function(id){

  ns <- shiny::NS(id)

  sidebar_elems <- {{{ name }}}_ui_input(id)

  sidebar_elems
}

#' @rdname {{{ name }}}_sidebar_side
#' @export
#
{{{ name }}}_sidebar_main <- function(id){

  main_elems <- {{{ name }}}_ui_output(id)

  main_elems
}

#' @seealso {{{ name }}}_sidebar_main
#' @rdname {{{ name }}}_server
#' @export
#
{{{ name }}}_sidebar_server <- function(
  input, output, session,
  data
){

  ## reactives ##
  ###############

  list_rct <- {{{ name }}}_server(input, output, session, data)

  rct_data_new <- list_rct$rct_result
  rct_input_state <- list_rct$rct_input_state
  rct_status_content <- list_rct$rct_status_content

  ## observers ##
  ###############

  # shows and hides controls based on the availabilty and nature of data
  shiny::observe({
    # outputs
    shinyjs::toggle(
      "data_preview",
      condition = shinypod::isValidy(rct_data_new())
    )
  })

  # change the class of the status window
  shinypod::observe_class_swap(id = "status", rct_status_content()$class)

  rct_data_new
}
