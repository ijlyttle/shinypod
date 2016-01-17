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

  ui <- read_delim_ui(id)

  elements <- c(
    "file",
    "delim",
    "decimal_mark",
    "tz_parse", "tz_parse_modal",
    "tz_display", "tz_display_modal"
  )

  ui[elements]
}

#' @rdname read_delim_sidebar_side
#' @export
#
read_delim_sidebar_main <- function(id){

  ui <- read_delim_ui(id)

  elements <- c(
    "text",
    "data"
  )

  ui[elements]
}
