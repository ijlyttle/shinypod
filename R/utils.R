#' html for scrollable pre-formatted text
#'
#' This is used as the \code{container} argument in \code{shiny::htmlOutput}
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
#' @param df  dataframe
#' @param tz  timezone, an Olson timezone or "UTC" (default)
#'
#' @return dataframe
#'
#' @examples
#' df_with_tz(wx_ames, tz = "UTC")
#'
#' @export
#
df_with_tz <- function(df, tz = "UTC"){

  names_datetime <- df_names_inherits(df, "POSIXct")

  fn_set_tz <- function(x){
    attr(x, "tzone") <- tz
    x
  }

  df[names_datetime] <- lapply(df[names_datetime], fn_set_tz)

  df
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
#' @param df     dataframe
#' @param what   character, vector of class we wish to find
#'
#' @return character vector
#' @export
#
df_names_inherits <- function(df, what){

  inherits_class <- vapply(df, inherits, logical(1), what = what)

  names_class <- names(inherits_class)[inherits_class]

  names_class
}
