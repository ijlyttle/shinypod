#' Sidebar layout for dygraph module
#'
#' These functions return the ui elements for a side panel and a main panel.
#'
#' The side elements are the inputs; the main elements are the outputs.
#'
#' @param id character, used to identify a namespace
#' @param help logical, indicates if help panels to be displayed
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#'
#' @examples
#'
#' @export
#
dygraph_sidebar_side <- function(id){

  dygraph_ui_input(id)

}

#' @rdname dygraph_sidebar_side
#' @export
#
dygraph_sidebar_main <- function(id, help =  TRUE){

  if (help){
    dygraph_ui_misc(id)
  } else {
    shiny::tagList()
  }

}
