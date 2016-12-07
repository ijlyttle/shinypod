#' Presentation UI-elements for upload-text module
#'
#' These functions are used to create the UI elements for a
#' sidebar presentation of the upload-text module.
#'
#' \describe{
#'   \item{upload_text_sb_side}{
#'     The only element in the sidebar panel is a
#'     \code{shiny::\link[shiny]{fileInput}}, used to
#'     upload a file from a local computer to the server.
#'   }
#'   \item{upload_text_sb_main}{
#'     The only element in the main panel is a
#'     \code{shiny::\link[shiny]{htmlOutput}}, used to
#'     display the first few lines of the uploaded file.
#'   }
#' }
#'
#' @param id    character, id for the module
#'
#' @return
#' \describe{
#'   \item{upload_text_sb_side}{
#'     \code{shiny::\link[shiny]{tagList}} of containing elements
#'     to put in a \code{shiny::\link[shiny]{sidebarPanel}}
#'   }
#'   \item{upload_text_sb_main}{
#'     \code{shiny::\link[shiny]{tagList}} of containing elements
#'     to put in a \code{shiny::\link[shiny]{mainPanel}}
#'   }
#' }
#'
#' @examples
#' library("shiny")
#' library("shinyjs")
#' library("shinypod")
#'
#' # also available by running sp_shiny_example("upload_text")
#'
#' ui <- shinyUI(
#'   fluidPage(
#'     useShinyjs(),
#'     titlePanel("Upload Text"),
#'     sidebarLayout(
#'       sidebarPanel(
#'         upload_text_sb_side("upload")
#'       ),
#'       mainPanel(
#'         upload_text_sb_main("upload")
#'       )
#'     )
#'   )
#' )
#'
#' server <- shinyServer(function(input, output, session) {
#'
#'   # rct_text is a reactive that can be used elsewhere in a server function
#'   rct_text <- callModule(module = upload_text_sb_server, id = "upload")
#'
#' })
#'
#' \dontrun{
#'   shinyApp(ui = ui, server = server)
#' }
#'
#' @seealso \code{\link{upload_text_ui}}, \code{\link{upload_text_sb_server}}
#' @export
#'
upload_text_sb_side <- function(id){

  sp_input(upload_text_ui(id))
}

#' @rdname upload_text_sb_side
#' @export
upload_text_sb_main <- function(id){

  sp_output(upload_text_ui(id))
}

#' Presentation server-function for upload-text module
#'
#' This is the server function for a
#' sidebar presentation of the upload-text module.
#'
#' The return value is a reactive that returns, as a character string,
#' the contents of the uploaded file.
#'
#' All the notifications provided by the foundation server-function,
#' \code{\link{upload_text_server}}, are passed along.
#'
#' @param input   \code{shiny::\link[shiny]{shinyServer}} input object
#' @param output  \code{shiny::\link[shiny]{shinyServer}} output object
#' @param session \code{shiny::\link[shiny]{shinyServer}} session object
#'
#' @return \code{shiny::\link[shiny]{reactive}} that returns the
#'   character string read from the uploaded file
#'
#' @examples
#' library("shiny")
#' library("shinyjs")
#' library("shinypod")
#'
#' # also available by running sp_shiny_example("upload_text")
#'
#' ui <- shinyUI(
#'   fluidPage(
#'     useShinyjs(),
#'     titlePanel("Upload Text"),
#'     sidebarLayout(
#'       sidebarPanel(
#'         upload_text_sb_side("upload")
#'       ),
#'       mainPanel(
#'         upload_text_sb_main("upload")
#'       )
#'     )
#'   )
#' )
#'
#' server <- shinyServer(function(input, output, session) {
#'
#'   # rct_text is a reactive that can be used elsewhere in a server function
#'   rct_text <- callModule(module = upload_text_sb_server, id = "upload")
#'
#' })
#'
#' \dontrun{
#'   shinyApp(ui = ui, server = server)
#' }
#'
#' @seealso \code{\link{upload_text_sb_side}}, \code{\link{upload_text_sb_main}},
#'   \code{\link{upload_text_server}}
#'
#' @export
#'
upload_text_sb_server <- function(input, output, session){

  ## reactives ##
  ###############

  upload_text <- upload_text_server(input, output, session)

  rct_text <- sp_rct_result(upload_text)
  rct_state <- sp_rct_state(upload_text)
  rct_notification <- sp_rct_notification(upload_text)

  ## observers ##
  ###############

  # notification
  shiny::observeEvent(
    rct_notification(),
    handlerExpr = {
      do.call(shiny::showNotification, rct_notification())
    }
  )

  rct_text
}
