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
  # sidebar_elems$time  <- shinyjs::hidden(sidebar_elems$time)
  # sidebar_elems$y1    <- shinyjs::hidden(sidebar_elems$y1)
  # sidebar_elems$y2    <- shinyjs::hidden(sidebar_elems$y2)

  sidebar_elems
}

#' @rdname dygraph_sidebar_side
#' @export
#
dygraph_sidebar_main <- function(id, help = TRUE){

  main_elems <- dygraph_ui_output(id)

  main_elems
}

#' @export
#
dygraph_sidebar_server <- function(
  input, output, session,
  data) {

  list_rct <- dygraph_server(input, output, session, data)

  rct_dyg <- list_rct$rct_dyg
  rct_state <- list_rct$rct_state

  # shows and hides controls based on the availabilty and nature of data
  shiny::observe({
    shinyjs::toggle("time", condition = rct_state()$has_var_time)
    shinyjs::toggle("y1", condition = rct_state()$has_var_num)
    shinyjs::toggle("y2", condition = rct_state()$has_var_num)
  })

  rct_dyg
}



