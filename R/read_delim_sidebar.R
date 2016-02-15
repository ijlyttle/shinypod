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
  sidebar_elems = read_delim_ui_input(id)
  sidebar_elems$delim         = shinyjs::hidden(sidebar_elems$delim)
  sidebar_elems$decimal_mark  = shinyjs::hidden(sidebar_elems$decimal_mark)
  sidebar_elems$tz_parse      = shinyjs::hidden(sidebar_elems$tz_parse)
  sidebar_elems$tz_display    = shinyjs::hidden(sidebar_elems$tz_display)

  sidebar_elems
}

#' @rdname read_delim_sidebar_side
#' @export
#
read_delim_sidebar_main <- function(id){

  read_delim_ui_output(id)
}


