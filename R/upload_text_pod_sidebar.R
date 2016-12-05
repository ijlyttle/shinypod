
#' @export
upload_text_sb_side <- function(id){

  sp_ui_input(upload_text_ui(id))
}

#' @export
upload_text_sb_main <- function(id){

  sp_ui_output(upload_text_ui(id))
}

#' @export
upload_text_sb_server <- function(input, output, session){

  ## reactives ##
  ###############

  upload_text <- upload_text_server(input, output, session)

  rct_text <- sp_rct_result(upload_text)
  rct_state <- sp_rct_state(upload_text)
  rct_notification <- sp_rct_notification(upload_text)

  ## observers ##
  ###############
  shiny::observeEvent(
    rct_notification(),
    handlerExpr = {
      do.call(shiny::showNotification, rct_notification())
    }
  )

  rct_text
}
