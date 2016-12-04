
#' Evaluates the validity of a reactive expression
#'
#' Useful if you need to return \code{TRUE}/\code{FALSE} on the validity of a
#' shiny reactive expression
#'
#' @param ... expression to pass to \code{shiny::req()}
#'
#' @return logical, returns \code{TRUE} if shiny validation passes
#' @export
#
isValidy <- function(...){

  result <- tryCatch(
    expr = {
      shiny::req(...)
      TRUE
    },
    error = function(e){FALSE}
  )

  result
}


#' Get the value of a reactive function, if reactive
#'
#' This is useful for functions where you want to be able to take either reactive
#' arguements or static arguments.
#'
#' @param x expression
#'
#' @return \code{x()} if reactive, \code{x} if not reactive
#' @export
#' @keywords internal
#'
static <- function(x){

  if (shiny::is.reactive(x)) {
    static_x <- x()
  } else {
    static_x <- x
  }

  static_x
}

#' Validates Shiny-module input, returns reactive
#'
#' Using this function at the start of your Shiny module
#' allows you to call your modules using arguments that may or may
#' not be reactive.
#'
#' This function does three things, but these things are associated
#' as to form a pattern that merits its own function. For a given expression \code{x}:
#'
#' \enumerate{
#'   \item{if \code{x} is reactive, set \code{y <- x()};
#'     if \code{x} is not reactive, set \code{y <- x}}
#'   \item{validate \code{y}, using \code{shiny::\link[shiny]{validate}} and
#'     \code{shiny::\link[shiny]{need}} with \code{.f}, \code{message},
#'     \code{label}, and \code{...}}
#'   \item{return \code{shiny::\link[shiny]{reactive}(y)}
#'     to use later in your module}
#' }
#'
#' @param x       expression, or  \code{shiny::\link[shiny]{reactive}}
#'   that returns an expression
#' @param .f      function that takes a single arg (object), returns TRUE if valid
#' @param message character, passed to \code{shiny::\link[shiny]{need}}
#' @param label   character, passed to \code{shiny::\link[shiny]{need}}
#' @param ...     other args passed along to \code{.f}
#'
#' @return \code{shiny::\link[shiny]{reactive}} that returns:
#' \describe{
#'   \item{\code{x()}}{if \code{x} is reactive}
#'   \item{\code{x}}{if \code{x} is not reactive}
#' }
#' @examples
#' library("shiny")
#'
#' # module function
#' summary_server <- function(input, output, session, df){
#'
#'   rct_df <- reactive_validate(df, is.data.frame)
#'
#'   rct_summary <- reactive(summary(rct_df()))
#'
#'   rct_summary
#' }
#'
#' # server function
#' shinyServer(function(input, output, session){
#'
#'   # either of these constructions will work
#'
#'   df <- mtcars
#'   rct_summary <- callModule(summary_server, "summary", df)
#'
#'   rct_df <- reactive(mtcars)
#'   rct_summary <- callModule(summary_server, "summary", rct_df)
#'
#' })
#'
#' @export
#
reactive_validate <- function(x, .f = identity, message, label = NULL, ...){

  shiny::reactive(
    {
      static_x <- static(x)

      if (!is.null(.f)){
        shiny::validate(
          shiny::need(do.call(.f, list(static_x, ...)), message, label)
        )
      }

      static_x
    }
  )

}
