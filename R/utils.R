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

  names_datetime <- df_names_inherits(data, "POSIXct")

  fn_set_tz <- function(x){
    attr(x, "tzone") <- tz
    x
  }

  data[names_datetime] <- lapply(data[names_datetime], fn_set_tz)

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

  if (is.null(choices)){

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


