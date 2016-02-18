#' Sidebar layout for write_delim module
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
#'       sidebarPanel(write_delim_sidebar("foo")),
#'       mainPanel(read_delim_main("foo"))
#'     )
#'   )
#' )
#'
#' @export
#
write_delim_sidebar <- function(id){
  sidebar_elems <- write_delim_ui_input(id)
  sidebar_elems$filename <- shinyjs::hidden(sidebar_elems$filename)

  sidebar_elems
}

#' @rdname write_delim_sidebar
#' @export
#
write_delim_main <- function(id){

  write_delim_ui_output(id)
}