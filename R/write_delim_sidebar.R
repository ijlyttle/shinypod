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
write_delim_sidebar_side <- function(id){
  sidebar_elems <- write_delim_ui_input(id)

  sidebar_elems
}

#' @rdname write_delim_sidebar_side
#' @export
#
write_delim_sidebar_main <- function(id){

  write_delim_ui_output(id)
}

#' @export
#
write_delim_sidebar_server <- function(
  input, output, session,
  data,
  delim = ",",
  status_show = TRUE,
  status_alert = TRUE
) {

  list_rct <- write_delim_server(input, output, session, data, delim, status_alert)
  rct_data <- list_rct$rct_data
  rct_state <- list_rct$rct_state

  rct_status_show <- reactive({
    static(status_show)
  })

  # manage the appearance according to the status
  shiny::observe({
    shinyjs::toggle(id = "status", condition = rct_status_show())
    shinyjs::toggle(id = "text_data", condition = rct_state()$has_data)
    shinyjs::toggle(id = "text_preview", condition = rct_state()$has_txt)
  })

  rct_data
}
