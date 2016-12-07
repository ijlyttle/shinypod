#' Constructs sp_ui object: template for UI elements
#'
#' This function can be useful for composing your own
#' shinypods.
#'
#' As constructed, a sp_ui object is a list with three elements:
#'
#' \describe{
#'   \item{input}{empty \code{shiny::\link[shiny]{tagList}}}
#'   \item{output}{empty \code{shiny::\link[shiny]{tagList}}}
#'   \item{misc}{empty \code{shiny::\link[shiny]{tagList}}}
#' }
#'
#' @return sp_ui object, S3 class
#'
#' @seealso Accessor functions: \code{\link{sp_input.sp_ui}}, \code{\link{sp_output.sp_ui}},
#'   \code{\link{sp_misc.sp_ui}}
#' @examples
#' sp_ui()
#' @export
#'
sp_ui <- function(){

  structure(
    list(
      input = shiny::tagList(),
      output = shiny::tagList(),
      misc = shiny::tagList()
    ),
    class = "sp_ui"
  )
}


#' @keywords internal
#' @export
sp_input <- function(sp_ui){
  UseMethod("sp_input")
}

#' @keywords internal
#' @export
sp_input.default <- function(sp_ui){
  stop("Unknown class")
}

#' Access the Shiny input elements
#'
#' @param sp_ui \code{\link{sp_ui}} object
#'
#' @return \code{shiny::\link[shiny]{tagList}} containing Shiny inputs
#' @export
#'
sp_input.sp_ui <- function(sp_ui){
  sp_ui$input
}

#' @keywords internal
#' @export
sp_output <- function(sp_ui){
  UseMethod("sp_output")
}

#' @keywords internal
#' @export
sp_output.default <- function(sp_ui){
  stop("Unknown class")
}

#' Access the Shiny output elements
#'
#' @param sp_ui \code{\link{sp_ui}} object
#'
#' @return \code{shiny::\link[shiny]{tagList}} containing Shiny outputs
#' @export
#'
sp_output.sp_ui <- function(sp_ui){
  sp_ui$output
}

#' @keywords internal
#' @export
sp_misc <- function(sp_ui){
  UseMethod("sp_misc")
}

#' @keywords internal
#' @export
sp_misc.default <- function(sp_ui){
  stop("Unknown class")
}

#' Access the Shiny miscellaneous elements
#'
#' @param sp_ui \code{\link{sp_ui}} object
#'
#' @return \code{shiny::\link[shiny]{tagList}}
#'   containing Shiny miscellaneous elements
#' @export
#'
sp_misc.sp_ui <- function(sp_ui){
  sp_ui$misc
}

#' Constructs sp_srv object: template for server-return
#'
#' This function can be useful for composing your own
#' shinypods.
#'
#' The idea of the foundation layer is to define the inputs, outputs, and how
#' they relate to each other. A sp_srv object has three members:
#'
#' \describe{
#'   \item{rct_result}{A reactive that returns the
#'     result of the server function
#'   }
#'   \item{rct_state}{A reactive that returns a list of (presumably)
#'     logical values that can be used by the presentation server-function,
#'     for example, to show/hide various elements using
#'     \code{shinyjs::\link[shiny]{toggle}}.
#'   }
#'   \item{rct_notification}{A reactive that contains a list of arguements that
#'     the presentation server-function can use to construct a notification using
#'     \code{shiny::\link[shiny]{showNotification}}
#'   }
#' }
#'
#' Presumably, the presentation server-function itself return \code{rct_result},
#' act upon the information in \code{rct_state} to show or hide UI elements,
#' and issue notifications based on \code{rct_notification}.
#'
#' @return sp_srv object, S3 class
#'
#' @seealso Accessor functions: \code{\link{sp_rct_result.sp_srv}},
#'   \code{\link{sp_rct_state.sp_srv}},
#'   \code{\link{sp_rct_notification.sp_srv}}
#' @export
#'
sp_srv <- function(rct_result, rct_state, rct_notification){

  # need to learn how to do this with NSE & functional programming
  if (!shiny::is.reactive(rct_result)) {
    stop("rct_result is not reactive", call. = FALSE)
  }

  if (!shiny::is.reactive(rct_state)) {
    stop("rct_state is not reactive", call. = FALSE)
  }

  if (!shiny::is.reactive(rct_notification)) {
    stop("rct_notification is not reactive", call. = FALSE)
  }

  structure(
    list(
      rct_result = rct_result,
      rct_state = rct_state,
      rct_notification = rct_notification
    ),
    class = "sp_srv"
  )

}

#' @keywords internal
#' @export
sp_rct_result <- function(sp_srv){
  UseMethod("sp_rct_result")
}

#' @keywords internal
#' @export
sp_rct_result.default <- function(sp_srv){
  stop("Unknown class")
}

#' Access the result reactive
#'
#' @param sp_srv \code{\link{sp_srv}} object
#'
#' @return \code{shiny::\link[shiny]{reactive}} that returns your result
#' @export
#'
sp_rct_result.sp_srv <- function(sp_srv){
  sp_srv$rct_result
}

#' @keywords internal
#' @export
sp_rct_state <- function(sp_srv){
  UseMethod("sp_rct_state")
}

#' @keywords internal
#' @export
sp_rct_state.default <- function(sp_srv){
  stop("Unknown class")
}

#' Access the state reactive
#'
#' @param sp_srv \code{\link{sp_srv}} object
#'
#' @return \code{shiny::\link[shiny]{reactive}} that returns arguments
#'  that can be used with \code{shiny::\link[shiny]{showNotification}}
#' @export
#'
sp_rct_state.sp_srv <- function(sp_srv){
  sp_srv$rct_state
}

#' @keywords internal
#' @export
sp_rct_notification <- function(sp_srv){
  UseMethod("sp_rct_notification")
}

#' @keywords internal
#' @export
sp_rct_notification.default <- function(sp_srv){
  stop("Unknown class")
}

#' Access the notification reactive
#'
#' @param sp_srv \code{\link{sp_srv}} object
#'
#' @return \code{shiny::\link[shiny]{reactive}} that returns arguments
#'  that can be used with \code{shiny::\link[shiny]{showNotification}}
#' @export
#'
sp_rct_notification.sp_srv <- function(sp_srv){
  sp_srv$rct_notification
}
