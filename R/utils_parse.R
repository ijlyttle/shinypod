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
    col_types <- ifelse(col_sum == "dttm", "c", "_")
    col_types <- paste0(col_types, collapse = "")

    # parse the text into character
    df_txt <- readr::read_delim(txt, delim = delim, col_types = col_types)

    # put into a matrix (limit to first 1000 rows)
    mat_txt <- as.matrix(utils::head(df_txt, 1000))

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
