#' Creates the UI elements for read-delim Shiny module
#'
#' It is unlikely that you would call this function directly
#' from your Shiny app. Instead, this function would be called
#' by the functions you use to compose the UI of your Shiny app,
#' for example  \code{\link{read_delim_sb_side}} and
#' \code{\link{read_delim_sb_main}}.
#'
#' @param id    character, id for the module
#'
#' @return list with three elements:
#' \describe{
#'   \item{input}{\code{shiny::\link[shiny]{tagList}}
#'     containing Shiny inputs:
#'     \describe{
#'       \item{delim}{\code{shiny::\link[shiny]{selectInput}}
#'         delimiter, maps to \code{delim} arguement
#'       }
#'     }
#'   }
#'   \item{output}{\code{shiny::\link[shiny]{taglist}}
#'     containing Shiny outputs:
#'     \describe{
#'       \item{data_preview}{\code{shiny::\link[shiny]{htmlOutput}}
#'         used to display the first few lines of the parsed dataframe
#'       }
#'     }
#'   }
#'   \item{misc}{empty \code{shiny::\link[shiny]{tagList}}}
#' }
#'
#' @seealso \code{\link{read_delim_sb_side}},
#'   \code{\link{read_delim_sb_main}}, \code{\link{read_delim_server}}
#'
#' @keywords internal
#' @export
#'
read_delim_ui <- function(id){

  ns <- shiny::NS(id)

  # specify timezones
  tz_choice <- OlsonNames()

  # returns a list with members: input, output, misc - each a shiny tagList
  ui <- sp_ui()

  ### input ###
  #############

  # delim
  ui$input$delim <-
    shiny::selectizeInput(
      inputId = ns("delim"),
      label = "Delimiter",
      choices = c(Comma = ",", Semicolon = ";", Tab = "\t")
    )

  # decimal_mark
  ui$input$decimal_mark <-
    shiny::selectizeInput(
      inputId = ns("decimal_mark"),
      label = "Decimal mark",
      choices = c(Point = ".", Comma = ",")
    )

  # decimal_mark
  ui$input$grouping_mark <-
    shiny::selectizeInput(
      inputId = ns("grouping_mark"),
      label = "Grouping mark",
      choices = c(Comma = ",", Point = ".")
    )

  ### output ###
  ##############

  # text_preview
  ui$output$data_preview <-
    shiny::htmlOutput(
      outputId = ns("data_preview"),
      container = shinypod::pre_scroll
    )

  ### misc ###
  ############


  ui
}
