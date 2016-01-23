#' Sidebar layout for dygraph module
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
#'       sidebarPanel(dygraph_sidebar_side("foo")),
#'       mainPanel(dygraph_sidebar_main("foo"))
#'     )
#'   )
#' )
#'
#' @export
#
dygraph_sidebar_side <- function(id){

  dygraph_ui_input(id)

}

#' @rdname dygraph_sidebar_side
#' @export
#
dygraph_sidebar_main <- function(id){

  dygraph_ui_misc(id)

}
