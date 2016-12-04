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


