#' Creates HTML container for scrollable pre-formatted text
#'
#' Used as the \code{container} argument in \code{shiny::\link[shiny]{htmlOutput}}.
#' These are containters for pre-formatted (verbatim) text.
#'
#' These functions are useful to help build
#' \code{shiny::\link[shiny]{htmlOutput}} for the UI. To
#' gernerate the HTML in your server function, use
#' \code{\link{text_html}} or \code{\link{tibble_html}} within
#' \code{shiny::\link[shiny]{renderUI}}.
#'
#' @param ... expression used to fill the body of the containter
#'
#' @return
#' \describe{
#'   \item{\code{pre_scroll}}{container with horizontal scroll-bars}
#'   \item{\code{pre_scroll_vert}}{container with vertical scroll-bars}
#' }
#'
#' @source \url{http://stackoverflow.com/questions/10374171/how-to-make-twitter-bootstraps-pre-blocks-scroll-horizontally}
#' @seealso \code{\link{text_html}}, \code{\link{tibble_html}}
#' @examples
#' library("shiny")
#' library("readr")
#'
#' ui <- shinyUI(
#'
#'   fluidPage(
#'     sidebarLayout(
#'       sidebarPanel(),
#'       mainPanel(
#'         htmlOutput(outputId = "text_preview", container = pre_scroll)
#'       )
#'     )
#'   )
#' )
#'
#' server <- shinyServer(function(input, output){
#'
#'   output$text_preview <-
#'     renderUI(
#'       system.file("extdata", "wx_ames.csv", package = "shinypod") %>%
#'       read_file() %>%
#'       text_html()
#'     )
#'
#' })
#'
#' \dontrun{
#' shinyApp(ui = ui, server = server)
#' }
#' @export
#
pre_scroll <- function(...){
  shiny::pre(
    style = "overflow: auto; word-wrap: normal; white-space: pre;",
    ...
  )
}

#' @rdname pre_scroll
#' @export
#'
pre_scroll_vert <- function(...){
  shiny::pre(
    class = "pre-scrollable",
    ...
  )
}

#' Converts dataframe into an HTML fragment
#'
#' This is useful to compose an HTML fragment to put in an output built using
#' \code{shiny::\link[shiny]{renderUI}}. It will use print the first
#' few rows of a dataframe, verbatim.
#'
#' @param df     data.frame, to display
#' @param n_row  numeric, number of rows of \code{df} to put into html
#'
#' @return chracter, HTML fragment
#' @seealso \code{\link{text_html}}, \code{\link{pre_scroll}}
#' @examples
#' library("shiny")
#' library("readr")
#'
#' ui <- shinyUI(
#'
#'   fluidPage(
#'     sidebarLayout(
#'       sidebarPanel(),
#'       mainPanel(
#'         htmlOutput(outputId = "text_preview", container = pre_scroll)
#'       )
#'     )
#'   )
#' )
#'
#' server <- shinyServer(function(input, output){
#'
#'   output$text_preview <-
#'     renderUI(
#'       system.file("extdata", "wx_ames.csv", package = "shinypod") %>%
#'       read_csv() %>%
#'       tibble_html()
#'     )
#'
#' })
#'
#' \dontrun{
#' shinyApp(ui = ui, server = server)
#' }
#' @export
#
tibble_html <- function(data, n_row = 5){
  h <-
    withr::with_options(
      list(width = 10000, tibble.print_min = n_row, tibble.print_max = n_row),
      utils::capture.output(print(tibble::as_tibble(data)))
    )
  h <- htmltools::htmlEscape(h)
  h <- paste(h, collapse = "<br/>")
  h <- shiny::HTML(h)

  h
}

#' Converts text into an HTML fragment
#'
#' This is useful to compose an HTML fragment to put in an output built using
#' \code{shiny::\link[shiny]{renderUI}}. This is a useful way to show the first
#' few lines of a text file, verbatim.
#'
#' @param text   character, text with newline character
#' @param n_line numeric, number of lines of \code{text} to put into HTML
#'
#' @return chracter, HTML fragment
#' @seealso \code{\link{tibble_html}}, \code{\link{pre_scroll}}
#' @examples
#' library("shiny")
#' library("readr")
#'
#' ui <- shinyUI(
#'
#'   fluidPage(
#'     sidebarLayout(
#'       sidebarPanel(),
#'       mainPanel(
#'         htmlOutput(outputId = "text_preview", container = pre_scroll)
#'       )
#'     )
#'   )
#' )
#'
#' server <- shinyServer(function(input, output){
#'
#'   output$text_preview <-
#'     renderUI(
#'       system.file("extdata", "wx_ames.csv", package = "shinypod") %>%
#'       read_file() %>%
#'       text_html()
#'     )
#'
#' })
#'
#' \dontrun{
#' shinyApp(ui = ui, server = server)
#' }
#'
#' @export
#'
text_html <- function(text, n_line = 6){

  if (is.null(text)){
    return(NULL)
  }

  # do more with n
  h <- htmltools::htmlEscape(text)
  h <- stringr::str_split(h, "\\r?\\n")
  h <- h[[1]]

  # truncate h, if needed
  if (length(h) > n_line){
    h <- c(
      h[seq(n_line)],
      paste0("... (", length(h) - n_line, " lines truncated)")
    )
  }

  h <- paste(h, collapse = "<br/>")
  h <- shiny::HTML(h)

  h
}


