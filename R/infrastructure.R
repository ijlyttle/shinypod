#' create a shinypod from a template
#'
#' This function is used to write out a template function for a shinypod,
#' both for the functional layer, in \code{foo_pod.R}, and a presentation layer, in
#' \code{foo_pod_sidebar.R}
#'
#' This assumes that you are working in a directory in an R package; the files will be written to
#' the \code{R} directory.
#'
#' @param name        character, name to prepend to the filenames
#' @param description character, short description to use in the function documentation
#' @param overwrite   logical, indicates if an existing file can be overwritten
#'
#' @return list of TRUE values
#' @export
#
use_pod <- function(name, description, overwrite = FALSE){

  list_template_name <- c("pod.R", "pod_sidebar.R")

  fn_template <- function(template_name){
    use_template(
      template = template_name,
      save = file.path("R", paste(name, template_name, sep = "_")),
      data = list(name = name, description = description),
      overwrite = overwrite
    )
  }

  lapply(list_template_name, fn_template)

  invisible(TRUE)
}



# template function based on use_template from Hadley's devtools

use_template <- function(template, save_as, data = list(), overwrite = FALSE) {

  path <- file.path(rprojroot::find_root("DESCRIPTION"), save_as)
  if (file.exists(path) && !identical(overwrite, TRUE)) {
    stop("`", save_as, "` already exists.", call. = FALSE)
  }

  template_path <- system.file("templates", template, package = "shinypod",
                               mustWork = TRUE)
  template_out <- whisker::whisker.render(readLines(template_path), data)

  message("* Creating `", save_as, "` from template.")
  writeLines(template_out, path)

  invisible(TRUE)
}
