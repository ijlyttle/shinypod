
sp_result <- function(){}

sp_state <- function(){}

sp_ui_input <- function(){}

sp_ui_output <- function(){}

sp_ui_misc <- function(){}

sp_ui <- function(){

  list(
    input = shiny::tagList(),
    output = shiny::tagList(),
    misc = shiny::tagList()
  )
}

#' Get the names of all the columns of the dataframe
#' that inherit from the supplied class name
#'
#' @param data   dataframe
#' @param what   character, vector of class we wish to find
#'
#' @return character vector
#' @export
#
df_names_inherits <- function(data, what){

  inherits_class <- vapply(data, inherits, logical(1), what = what)

  names_class <- names(inherits_class)[inherits_class]

  names_class
}


#' determine the proper selection
#'
#' Used for \code{shiny::\link[shiny]{selectInput}} to allow you to
#' update its selection when its choices change.
#'
#' @param value    character vector, current value of an input
#' @param choices  character vector, new choices for an input
#' @param index    integer, if \code{value} is not in defualt \code{choices},
#'   uses this index of \code{choices}.
#'
#' @return character vector of proposed selection
#' @examples
#'   update_selected("a", c("a", "b", "c"))
#'   update_selected("a", NULL)
#'   update_selected("d", c("a", "b", "c"))
#'   update_selected("d", c("a", "b", "c"), index = 1)
#'
#' @export
#
update_selected <- function(value, choices, index = NULL){

  if (!isValidy(choices)){

    # we have no choices, select NULL
    selected <- NULL
  } else {

    # see if our current value is one of our choices
    selected <- value[value %in% choices]

    if (length(selected) == 0){
      # no - look at defaults

      if (is.null(index)){
        selected <- NULL
      } else {
        selected <- choices[index]
      }
    }

  }

  selected
}

#' swap out classes on an html element
#'
#' This function stores the value of the last class to be added (using this function),
#' then removes that class before addding the new class. For example, this may be useful
#' if you want to modify a panel to show an alert.
#'
#' As this is an observer, there is no return value. It is called for the side-effect of
#' changing the class of the html element.
#'
#' This is based on \code{shiny::renderText()}
#'
#' @param id A character vector to identify the html element to operate on.
#' @param expr An expression that returns a character vector to add to the html element.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @return nothing
#' @export
#
observe_class_swap <- function(id, expr, env = parent.frame(), quoted = FALSE){

  func <- shiny::exprToFunction(expr, env, quoted)

  # we use a reactive value to persist the value of the class we added previously
  rctval <- shiny::reactiveValues(class_current = NULL)

  shiny::observeEvent(
    eventExpr = func(),
    handlerExpr = {
      # print(paste(rctval$class_current, func(), sep = " -> "))
      shinyjs::removeClass(id = id, rctval$class_current)
      shinyjs::addClass(id = id, func())
      rctval$class_current <- func()
    },
    ignoreNULL = FALSE
  )

}















