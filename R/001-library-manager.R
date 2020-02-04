#' Library manager
#'
#' Loads packages from a specified requirements file and optionally writes
#' them out to a references file
#'
#' @param req_file file containing the list of packages to load
#' @param bib_file if not NULL, will write out the references to the given file
#'
#' @return None
#'
#' @examples
#' load_library("R-requirements.txt", "references/R-references.bib")

load_library <- function(req_file, bib_file = NULL) {
  packages <- readLines(req_file)
  new.packages <-
    packages[!(packages %in% installed.packages()[, "Package"])]
  
  if (length(new.packages))
    install.packages(new.packages)
  invisible(suppressMessages(suppressWarnings(
    lapply(packages, library, character.only = TRUE)
  )))
  
  
  if (!is.null(bib_file))
    knitr::write_bib(packages, file = bib_file)
  
  return(NULL)
}
