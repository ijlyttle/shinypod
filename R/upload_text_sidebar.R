#' Sidebar layout for upload_text module
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
#'       sidebarPanel(upload_text_sidebar_side("foo")),
#'       mainPanel(upload_text_sidebar_main("foo"))
#'     )
#'   )
#' )
#'
#' @export
#
upload_text_sidebar_side <- function(id){

  ns <- shiny::NS(id)

  sidebar_elems <- upload_text_ui_input(id)
  misc_elems <- upload_text_ui_misc(id)

  sidebar_elems
}

#' @rdname upload_text_sidebar_side
#' @export
#
upload_text_sidebar_main <- function(id){

  main_elems <- upload_text_ui_output(id)

  main_elems$text <- shinyjs::hidden(main_elems$text)

  main_elems
}

# note we are initializing the show/hide functions here, but controlling at the definition level

#' @seealso upload_text_sidebar_main
#' @rdname upload_text_server
#' @export
#
upload_text_sidebar_server <- function(
  input, output, session
){

  ## reactives ##
  ###############

  list_rct <- upload_text_server(input, output, session)
  rct_text <- list_rct$rct_result


  ## observers ##
  ###############

  # shows and hides inputs/outputs based on input/result status
  shiny::observe({
    # outputs
    shinyjs::toggle("text", condition = isValidy(rct_text()))
  })

  # changes the class of the status panel
  observe_class_swap(id = "status", list_rct$rct_status_content()$class)

  # return the result
  rct_text
}
