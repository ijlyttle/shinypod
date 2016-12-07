#' Creates the UI elements for upload-text Shiny module
#'
#' It is unlikely that you would call this function directly
#' from your Shiny app. Instead, this function would be called
#' by the functions you use to compose the UI of your Shiny app,
#' for example  \code{\link{upload_text_sb_side}} and
#' \code{\link{upload_text_sb_main}}.
#'
#' @param id    character, id for the module
#'
#' @return list with three elements:
#' \describe{
#'   \item{input}{\code{shiny::\link[shiny]{tagList}}
#'     containing Shiny inputs:
#'     \describe{
#'       \item{file}{\code{shiny::\link[shiny]{fileInput}}
#'         used to upload file
#'       }
#'     }
#'   }
#'   \item{output}{\code{shiny::\link[shiny]{taglist}}
#'     containing Shiny outputs:
#'     \describe{
#'       \item{text_preview}{\code{shiny::\link[shiny]{htmlOutput}}
#'         used to display the first few lines of the uploaded text file
#'       }
#'     }
#'   }
#'   \item{misc}{empty \code{shiny::\link[shiny]{tagList}}}
#' }
#'
#' @seealso \code{\link{upload_text_sb_side}},
#'   \code{\link{upload_text_sb_main}}, \code{\link{upload_text_server}}
#'
#' @keywords internal
#' @export
#'
upload_text_ui <- function(id){

  ns <- shiny::NS(id)

  # returns a list with members: input, output, misc - each a shiny tagList
  ui <- sp_ui()

  ### input ###
  #############

  # file
  ui$input$file <-
    shiny::fileInput(
      inputId = ns("file"),
      label = "Upload text file",
      accept = c("text/csv", ".csv", "text/comma-separated-values", "text/plain")
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

#' Server function for upload-text Shiny module
#'
#' It is unlikely that you would call this function directly
#' from your Shiny app. Instead, this function would be called
#' by the server module that you call from your Shiny app,
#' for example  \code{\link{upload_text_sb_server}}.
#'
#' The component \code{rct_notification} makes the following notifications
#' available:
#'
#' \itemize{
#'   \item{a "message" notification upon successful file-upload,
#'     containing the name and size of the file
#'   }
#' }
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
#'     of information available within the server function:
#'     \describe{
#'       \item{has_file}{indicates if a file has been uploaded}
#'     }
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
#' @seealso  \code{\link{upload_text_sb_server}},
#'   \code{\link{upload_text_ui}}
#'
#' @keywords internal
#' @export
#'
upload_text_server <- function(input, output, session){

  ns <- session$ns

  ## functions ##
  ###############

  ## reactive sources ##
  ######################

  rctval <- shiny::reactiveValues(
    text = NULL,
    notification = NULL
  )

  ## reactive conductors ##
  #########################

  rct_text <- shiny::reactive({

    shiny::validate(
      shiny::need(rctval$text, message = "Please upload a file.")
    )

    rctval$text
  })

  rct_state <-
    shiny::reactive({
      list(
        has_file = shinypod::isValidy(input$file)
      )
    })

  rct_notification <- shiny::reactive(rctval$notification)

  ## input-update observers ##
  ############################

  ## other observers ##
  #####################

  # button
  shiny::observeEvent(
    eventExpr = input$file,
    handlerExpr = {
      # put the result in a reactive source
      rctval$text <- readr::read_file(input$file$datapath)

      str_size <-
        input$file$size %>%
        structure(class = "object_size") %>%
        format(units = "auto")

      rctval$notification <- list(
        ui = paste(
          "Uploaded", input$file$name,
          paste0("(", str_size, ")")
        ),
        type = "message"
      )
    }
  )

  ## outputs ##
  #############

  output$text_preview <-
    shiny::renderUI(
      rct_text() %>%
      shinypod::text_html()
    )

  # return sp_srv object
  sp_srv(
    rct_result = rct_text,
    rct_state = rct_state,
    rct_notification = rct_notification
  )
}
