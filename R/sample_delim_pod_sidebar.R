#' Presentation UI-elements for sample-delim module
#'
#' These functions are used to create the UI elements for a
#' sidebar presentation of the upload-text module.
#'
#' \describe{
#'   \item{sample_delim_sb_side}{
#'     The only element in the sidebar panel is a
#'     \code{shiny::\link[shiny]{selectizeInput}}, used to
#'     select from some sample files that contain delimited text.
#'   }
#'   \item{sample_delim_sb_main}{
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
#'   \item{sample_delim_sb_side}{
#'     \code{shiny::\link[shiny]{tagList}} of containing elements
#'     to put in a \code{shiny::\link[shiny]{sidebarPanel}}
#'   }
#'   \item{sample_delim_sb_main}{
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
#' # also available by running sp_shiny_example("upload_text") // not yet
#'
#' ui <- shinyUI(
#'   fluidPage(
#'     useShinyjs(),
#'     titlePanel("Select delimited-text file"),
#'     sidebarLayout(
#'       sidebarPanel(
#'         sample_delim_sb_side("sample_delim")
#'       ),
#'       mainPanel(
#'         sample_delim_sb_main("sample_delim")
#'       )
#'     )
#'   )
#' )
#'
#' server <- shinyServer(function(input, output, session) {
#'
#'   # rct_text is a reactive that can be used elsewhere in a server function
#'   rct_text <- callModule(module = sample_delim_sb_server, id = "sample_delim")
#'
#' })
#'
#' \dontrun{
#'   shinyApp(ui = ui, server = server)
#' }
#'
#' @seealso \code{\link{sample_delim_ui}}, \code{\link{sample_delim_sb_server}}
#' @export
#'
sample_delim_sb_side <- function(id){

  sp_input(sample_delim_ui(id))
}

#' @rdname sample_delim_sb_side
#' @export
sample_delim_sb_main <- function(id){

  sp_output(sample_delim_ui(id))
}

#' Presentation server-function for sample-delim module
#'
#' This is the server function for a
#' sidebar presentation of the sample-delim module.
#'
#' The return value is a reactive that returns, as a character string,
#' the contents of the selected file.
#'
#' All the notifications provided by the foundation server-function,
#' \code{\link{sample_delim_server}}, are passed along.
#'
#' @param input   \code{shiny::\link[shiny]{shinyServer}} input object
#' @param output  \code{shiny::\link[shiny]{shinyServer}} output object
#' @param session \code{shiny::\link[shiny]{shinyServer}} session object
#'
#' @return \code{shiny::\link[shiny]{reactive}} that returns the
#'   character string read from the selected file
#'
#' @examples
#' library("shiny")
#' library("shinyjs")
#' library("shinypod")
#'
#' # also available by running sp_shiny_example("upload_text") // not yet
#'
#' ui <- shinyUI(
#'   fluidPage(
#'     useShinyjs(),
#'     titlePanel("Select delimited-text file"),
#'     sidebarLayout(
#'       sidebarPanel(
#'         sample_delim_sb_side("sample_delim")
#'       ),
#'       mainPanel(
#'         sample_delim_sb_main("sample_delim")
#'       )
#'     )
#'   )
#' )
#'
#' server <- shinyServer(function(input, output, session) {
#'
#'   # rct_text is a reactive that can be used elsewhere in a server function
#'   rct_text <- callModule(module = sample_delim_sb_server, id = "sample_delim")
#'
#' })
#'
#' \dontrun{
#'   shinyApp(ui = ui, server = server)
#' }
#'
#' @seealso \code{\link{sample_delim_sb_side}}, \code{\link{sample_delim_sb_main}},
#'   \code{\link{sample_delim_server}}
#'
#' @export
#'
sample_delim_sb_server <- function(input, output, session){

  ## reactives ##
  ###############

  sample_text <- sample_delim_server(input, output, session)

  rct_text <- sp_rct_result(sample_text)
  rct_state <- sp_rct_state(sample_text)
  rct_notification <- sp_rct_notification(sample_text)

  ## observers ##
  ###############


  rct_text
}
