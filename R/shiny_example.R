#' Runs (or lists) example shiny apps
#'
#' Use this function to find out what example shiny-apps are available
#' with this package, or run any of them.
#'
#' @param example_name  character, name of example app to run
#'
#' @return character, list of available apps - or NULL (called for side effect)
#' @examples
#' sp_shiny_example()
#' \dontrun{
#'   sp_shiny_example("upload_text")
#' }
#' @export
#'
sp_shiny_example <- function(example_name = NULL){

  examples <- list.files(system.file("shiny", package = "shinypod"))

  if (is.null(example_name)){
    return(examples)
  }

  if (!example_name %in% examples){
    stop(
      paste(
        "example",
        paste0("\"", example_name , "\""),
        "not found, please use sp_example_name() to list available examples"
      )
    )
  }

  shiny::runApp(system.file("shiny", example_name, package = "shinypod"))

  invisible(NULL)
}
