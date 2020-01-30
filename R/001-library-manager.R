# Library Manager
# Alwin Wang 2019

load_library <- function(packages = NULL, bib_file = "references/R-references.bib") {
  if (is.null(packages))
    packages <- c(
      "base",
      "bibtex",
      "tidyverse",
      "lubridate",
      "readxl",
      "knitr",
      "rmarkdown",
      "bookdown",
      "citr",
      "tinytex",
      "servr"
    )
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  invisible(lapply(packages, library, character.only = TRUE))
  
  knitr::write_bib(packages, file = bib_file)
  return(NULL)
}

# invisible(suppressMessages(suppressWarnings(load_library())))

