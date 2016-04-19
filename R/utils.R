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
    ...,
    style = "overflow: auto; word-wrap: normal; white-space: pre;"
  )
}

#' Sets the timezone of all time-based columns in a dataframe
#'
#' @param data  dataframe
#' @param tz    timezone, an Olson timezone or "UTC" (default)
#'
#' @return dataframe
#'
#' @examples
#' df_with_tz(wx_ames, tz = "UTC")
#'
#' @export
#
df_with_tz <- function(data, tz = "UTC"){

  .Deprecated(new = "lubridate::with_tz", package = "shinypod")
  data <- lubridate::with_tz(time = data, tzone = tz)

  data
}

# returns TRUE if the dataframe parsed using the text has any POSIXct columns
# not parsed from ISO-8601
#
# detects if any time columns in dataframe
#
# @param txt character, text used to make the dataframe
# @param delim character, delimiter
#
# @return logical, indicating if there are any non ISO-8601 time columns
#
df_has_time_non_8601 <- function(txt, delim){

  df <- readr::read_delim(txt, delim = delim)

  has_posixct <- (length(df_names_inherits(df, "POSIXct")))

  if (has_posixct) {

    # identify time columns of dataframe
    col_sum <- lapply(df, dplyr::type_sum)
    col_sum <- unlist(col_sum)

    # turn this into a col_types specification
    col_types <- ifelse(col_sum == "time", "c", "_")
    col_types <- paste0(col_types, collapse = "")

    # parse the text into character
    df_txt <- readr::read_delim(txt, delim = delim, col_types = col_types)

    # put into a matrix (limit to first 1000 rows)
    mat_txt <- as.matrix(head(df_txt, 1000))

    # test for iso_8601 pattern
    all_8601 <- all(is_time_8601(mat_txt), na.rm = TRUE)

    x <- !all_8601
  } else {
    x <- FALSE
  }

  x
}

# detects if a character string is in ISO-8601 format
is_time_8601 <- function(x){

  # \\d{4}    exactly 4 digits
  # -?        optional "-"
  # \\d{2}    exactly 2 digits
  # -?        optional "-"
  # \\d{2}    exactly 2 digits
  regex_8601_date <- "\\d{4}-?\\d{2}-?\\d{2}"

  # \\d{2}       exactly 2 digits
  # (:?\\d{2})?  optional (optional ":", exactly 2 digits)
  # (:?\\d{2})?  optional (optional ":", exactly 2 digits)
  # (\\.\\d{3})? optional (".", exactly 3 digits)
  regex_8601_time <- "\\d{2}(:?\\d{2})?(:?\\d{2})?(\\.\\d{3})?"

  # Z                       "Z"
  # |                       or
  # ([+-]\\d{2}(:?\\d{2})?) (one of "+,-", exactly 2 digits,
  #                          optional (optional ":", exactly 2 digits))
  regex_8601_zone <- "Z|([+-]\\d{2}(:?\\d{2})?)"

  # ^         beginning of string
  # [T ]      "T" or " "
  # $         end of string
  regex_8601 <- paste0("^", regex_8601_date, "[T ]", regex_8601_time, regex_8601_zone, "$")

  stringr::str_detect(x, regex_8601)
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
  rctval <- reactiveValues(class_current = NULL)

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

#' use input and result to generate message and class of status
#'
#' The argument \code{status} shall be a list with two members: \code{input} and \code{result}.
#' Each of those lists shall have components \code{index}, \code{is_valid}, and \code{message}.
#'
#' This return value is a list with members \code{class} and \code{message}. The \code{class} can be used by
#' \link{observe_class_swap} to change the appearance of an output. The \code{message} can be used as the
#' text displayed by the output.
#'
#' @param status  list with components \code{input} and \code{result}
#'
#' @return list with components \code{class} and \code{message}
#' @export
#
status_content <- function(status){

  if (shiny::is.reactivevalues(status)) {
    status <- shiny::reactiveValuesToList(status)
  }

  # print(status)

  is_danger <-
    identical(status$result$is_valid, FALSE) &&
    identical(status$result$index, status$input$index)

  is_warning <- identical(status$input$is_valid, FALSE)

  is_info <-
    !is.null(status$result$is_valid) &&
    !identical(status$input$index, status$result$index)

  is_success <- identical(status$result$is_valid, TRUE)

  # print(paste("is_danger:", is_danger))
  # print(paste("is_warning:", is_warning))
  # print(paste("is_info:", is_info))
  # print(paste("is_success:", is_success))

  if (is_danger) {
    class <- "alert-danger"
    message <- status$result$message
  } else if (is_warning) {
    class <- "alert-warning"
    message <- status$input$message
  } else if (is_info) {
    class <- "alert-info"
    message <- paste("Inputs have changed since generation of results",
                     status$input$message,
                     sep = "\n\n")
  } else if (is_success){
    class <- "alert-success"
    message <- status$result$message
  } else {
    class <- NULL
    message <- status$input$message
  }

  list(class = class, message = message)
}

#' checks to see that an expression passes shiny validation
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
#' @param x
#'
#' @return \code{x}, if not reactive, \code{x()} if reactive
#' @export
#
static <- function(x){

  if (shiny::is.reactive(x)) {
    static_x <- x()
  } else {
    static_x <- x
  }

  static_x
}

#' combines handling of reactive and validating the contents
#'
#' @param  expr   expression, or reactive that returns an expressiondo
#' @param .f      function that takes a single arg (object), returns TRUE if valid
#' @param message passed to need
#' @param label   passed to need
#' @param ...     other args to pass along to .f
#'
#' @return reactive that returns the expression
#' @export
#
reactive_validate <- function(expr, .f = identity, message, label = NULL, ...){

  shiny::reactive(
    {
      static_x <- static(expr)

      shiny::validate(
        shiny::need(do.call(.f, list(static_x, ...)), message, label)
      )

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








