read_delim_side <- function(id){

  ui <- read_delim_ui(id)

  elements <- c(
    "file",
    "delim",
    "decimal_mark",
    "tz_parse", "tz_parse_modal",
    "tz_display", "tz_display_modal"
  )

  ui[elements]
}

read_delim_main <- function(id){

  ui <- read_delim_ui(id)

  elements <- c(
    "text",
    "data"
  )

  ui[elements]
}
