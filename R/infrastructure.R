#' Create a shinypod from a template.
#'
#' This function is used to write out a template function for a shinypod,
#' both for the functional layer, in \code{foo_pod.R}, and a presentation layer, in
#' \code{foo_pod_sidebar.R}
#'
#' This assumes that you are working in a directory in an R package; the files will be written to
#' the \code{R} directory.
#'
#' @param name        character, name to prepend to the filenames
#' @param description character, completes the phrase "module that ..."
#' @param is_blocking logical, indicates if this pod will have a blocking button
#' @param overwrite   logical, same usage as devtools; indicates if an existing file can be overwritten
#'
#' @return list of TRUE values
#' @export
#
use_pod <- function(name, description, is_blocking = TRUE, overwrite = FALSE){

  list_template <- list(
    `TRUE` = c("blocking_pod.R", "blocking_pod_sidebar.R"),
    `FALSE` = c("pod.R", "pod_sidebar.R")
  )
  template <- list_template[[as.character(is_blocking)]]

  fn_template <- function(template_name){

    file_name <- stringr::str_extract(template_name, "pod.*\\.R")

    use_template(
      template = template_name,
      save = file.path("R", paste(name, file_name, sep = "_")),
      data = list(name = name, description = description),
      overwrite = overwrite
    )
  }

  lapply(template, fn_template)

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
