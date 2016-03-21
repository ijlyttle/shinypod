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
  sidebar_elems       <- dygraph_ui_input(id)
  sidebar_elems$time  <- shinyjs::hidden(sidebar_elems$time)
  sidebar_elems$y1    <- shinyjs::hidden(sidebar_elems$y1)
  sidebar_elems$y2    <- shinyjs::hidden(sidebar_elems$y2)

  sidebar_elems
}

#' @rdname dygraph_sidebar_side
#' @export
#
dygraph_sidebar_main <- function(id, help = TRUE){

  main_elems <- dygraph_ui_output(id)

  if (help){
    main_elems$help <- dygraph_ui_misc(id)
  }

  main_elems
}
