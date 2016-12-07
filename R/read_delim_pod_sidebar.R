#' Presentation UI-elements for read-delim module
#'
#' This is essentially a wrapper around
#' \code{readr::\link[readr]{read_delim}}.
#' These functions are used to create the UI elements for a
#' sidebar presentation of the read-delim module.
#'
#'
#' \describe{
#'   \item{read_delim_sb_side}{}
#'   \item{read_delim_sb_main}{}
#' }
#'
#' @param id    character, id for the module
#'
#' @return
#' \describe{
#'   \item{read_delim_sb_side}{
#'     \code{shiny::\link[shiny]{tagList}} of containing elements
#'     to put in a \code{shiny::\link[shiny]{sidebarPanel}}
#'   }
#'   \item{read_delim_sb_main}{
#'     \code{shiny::\link[shiny]{tagList}} of containing elements
#'     to put in a \code{shiny::\link[shiny]{mainPanel}}
#'   }
#' }
#'
#' @seealso \code{\link{read_delim_ui}}, \code{\link{read_delim_sb_server}}
#' @export
#'
read_delim_sb_side <- function(id){

  ns <- shiny::NS(id)

  input_list <- sp_input(read_delim_ui(id))

  button_time <-
    shiny::actionButton(
      inputId = "button_time",
      label = "Datetime-parsing options"
    )

  button_advanced <-
    shiny::actionButton(
      inputId = "button_time",
      label = "Advanced options"
    )

  shiny::tagList(
    input_list$delim,
    input_list$decimal_mark,
    input_list$grouping_mark,
    button_time,
    button_advanced
  )
}

#' @rdname read_delim_sb_side
#' @export
read_delim_sb_main <- function(id){

  sp_output(read_delim_ui(id))
}
