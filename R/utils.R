#' html for scrollable pre-formatted text
#'
#' This is used as the \code{container} argument in  \code{shiny::\link[shiny]{htmlOutput}}
#'
#' @param ... expression used to fill text
#'
#' @source \url{http://stackoverflow.com/questions/10374171/how-to-make-twitter-bootstraps-pre-blocks-scroll-horizontally}
#' @export
#
pre_scroll <- function(...){
  shiny::pre(
    style = "overflow: auto; word-wrap: normal; white-space: pre;",
    ...
  )
}

pre_scroll_vert <- function(...){
  shiny::pre(
    class = "pre-scrollable",
    ...
  )
}

sp_result <- function(){}

sp_state <- function(){}

sp_input <- function(){}

sp_output <- function(){}

sp_misc <- function(){}


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

#' convert a tbl_df printout into an html fragment
#'
#' @param data, data.frame
#'
#' @return html fragment
#' @export
#
tibble_html <- function(data){
  h <-
    withr::with_options(
      list(width = 10000, tibble.print_min = 5, tibble.print_max = 5),
      utils::capture.output(print(data))
    )
  h <- htmltools::htmlEscape(h)
  h <- paste(h, collapse = "<br/>")
  h <- shiny::HTML(h)

  h
}

#' convert text into an html fragment
#'
#' @param text, text with newline character
#' @param n, number of lines to keep
#'
#' @return html fragment
#' @export
#
text_html <- function(text, n = 6){

  if (is.null(text)) return(NULL)

  # do more with n
  h <- htmltools::htmlEscape(text)
  h <- stringr::str_split(h, "\\r?\\n")
  h <- h[[1]]

  # truncate h, if needed
  if (length(h) > (n + 1)){
    h <- c(h[seq(n)], "...")
  }

  h <- paste(h, collapse = "<br/>")
  h <- shiny::HTML(h)

  h
}

#' strip away the reactivity
#'
#' This is useful for functions where you want to be able to take either reactive
#' arguements or static arguments.
#'
#' @param x object
#'
#' @return \code{x}, if not reactive, \code{x()} if reactive
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

#' Validates inputs for shiny modules, returns reactive
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
#' @return \code{shiny::\link[shiny]{reactive}} that returns \code{x}, or
#'   \code{x()}, if \code{x} is reactive
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

# @param str_dtm     character, string to be parsed as datetime
# @param format      character, format to use - see readr::parse_datetime
# @param lang        character, language to use - see readr::locale
# @param tz_parse    character, timezone to parse, default "UTC"
.parse_datetime <- function(str_dtm, format, lang = "en", tz_parse = "UTC"){

  locale <- readr::locale(date_names = lang, tz = tz_parse)

  dtm <- readr::parse_datetime(str_dtm, format = format, locale = locale)

  dtm
}

.choices_format <- function(dtm = lubridate::ymd_hms("2015-09-28 02:45:00")){

   print_format <- c(
    "%d-%b-%Y %H:%M:%S",
    "%m/%d/%Y %H:%M:%S %z"
  )

  # used to make the parsing format more-permissive than the printing format
  parse_format <- function(x){
    regex <- "(%[A-Za-z])[^%]+" # a "%", followed by any upper/lowercase letter,
    #  followed by one-or-more non-"%" characters

    # replace
    result <- stringr::str_replace_all(x, regex, "\\1%.")

    result
  }

  choices <- purrr::map_chr(print_format, parse_format)
  names(choices) <- purrr::map_chr(print_format, ~ format(dtm, format = .x))

  choices
}








