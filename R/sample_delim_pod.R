#' Creates the UI elements for sample-delim Shiny module
#'
#' It is unlikely that you would call this function directly
#' from your Shiny app. Instead, this function would be called
#' by the functions you use to compose the UI of your Shiny app,
#' for example  \code{\link{sample_delim_sb_side}} and
#' \code{\link{sample_delim_sb_main}}.
#'
#' @param id    character, id for the module
#'
#' @return list with three elements:
#' \describe{
#'   \item{input}{\code{shiny::\link[shiny]{tagList}}
#'     containing Shiny inputs:
#'     \describe{
#'       \item{select_delim}{\code{shiny::\link[shiny]{selectizeInput}}
#'         used to choose sample text
#'       }
#'     }
#'   }
#'   \item{output}{\code{shiny::\link[shiny]{taglist}}
#'     containing Shiny outputs:
#'     \describe{
#'       \item{text_preview}{\code{shiny::\link[shiny]{htmlOutput}}
#'         used to display the first few lines of the sampled
#'       }
#'     }
#'   }
#'   \item{misc}{empty \code{shiny::\link[shiny]{tagList}}}
#' }
#'
#' @seealso \code{\link{sample_delim_sb_side}},
#'   \code{\link{sample_delim_sb_main}}, \code{\link{sample_delim_server}}
#'
#' @keywords internal
#' @export
#'
sample_delim_ui <- function(id){

  ns <- shiny::NS(id)

  # selection of files to provide
  choice_file <-
    system.file("extdata", package = "shinypod") %>%
    list.files(pattern = "\\.csv", full.names = TRUE)

  names(choice_file) <- basename(choice_file)

  # returns a list with members: input, output, misc - each a shiny tagList
  ui <- sp_ui()

  ### input ###
  #############

  # file
  ui$input$select_delim <-
    shiny::selectizeInput(
      inputId = ns("select_delim"),
      label = "Select delimited-text",
      choices = choice_file
    )

  ### output ###
  ##############

  # text_preview
  ui$output$text_preview <-
    shiny::htmlOutput(
      outputId = ns("text_preview"),
      container = shinypod::pre_scroll
    )

  ### misc ###
  ############


  ui
}

#' Server function for sample-delim Shiny module
#'
#' It is unlikely that you would call this function directly
#' from your Shiny app. Instead, this function would be called
#' by the server module that you call from your Shiny app,
#' for example  \code{\link{sample_delim_sb_server}}.
#'
#' The component \code{rct_notification} remains \code{NULL} - no notifications.
#'
#' @param input   \code{shiny::\link[shiny]{shinyServer}} input object
#' @param output  \code{shiny::\link[shiny]{shinyServer}} output object
#' @param session \code{shiny::\link[shiny]{shinyServer}} session object
#'
#' @return \code{\link{sp_srv}} object with members:
#' \describe{
#'   \item{rct_result}{\code{shiny::\link[shiny]{reactive}},
#'     returns the character string read from the file
#'   }
#'   \item{rct_state}{\code{shiny::\link[shiny]{reactive}},
#'     returns list of logical values indicating the state
#'     of information available within the server function.
#'     This module has no "state".
#'   }
#'   \item{rct_notification}{\code{shiny::\link[shiny]{reactive}},
#'     returns list of arguments that can be passed to
#'     \code{shiny::\link[shiny]{showNotification}}:
#'     \describe{
#'       \item{ui}{character, content of the message}
#'       \item{type}{character, type of the message, i.e. "message"}
#'     }
#'   }
#' }
#'
#' @seealso  \code{\link{sample_delim_sb_server}},
#'   \code{\link{sample_delim_ui}}
#'
#' @keywords internal
#' @export
#'
sample_delim_server <- function(input, output, session){

  ns <- session$ns

  ## functions ##
  ###############

  ## reactive sources ##
  ######################

  rctval <- shiny::reactiveValues(
    notification = NULL
  )

  ## reactive conductors ##
  #########################

  rct_text <- shiny::reactive({
    readr::read_file(input$select_delim)
  })

  rct_state <-
    shiny::reactive({
      list()
    })

  rct_notification <- shiny::reactive(rctval$notification)

  ## input-update observers ##
  ############################

  ## other observers ##
  #####################

  ## outputs ##
  #############

  output$text_preview <-
    shiny::renderUI(
      rct_text() %>%
      shinypod::text_html()
    )

  # return sp_srv object
  shinypod::sp_srv(
    rct_result = rct_text,
    rct_state = rct_state,
    rct_notification = rct_notification
  )
}
