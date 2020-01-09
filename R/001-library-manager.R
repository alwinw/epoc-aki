# Library Manager
# Alwin Wang 2019

load_library <- function(packages = NULL) {
  if (is.null(packages))
    packages <- c(
      "base",
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
  
  citations <- lapply(packages, citation)
  names(citations) <- packages
  
  return(citations)
}

# invisible(suppressMessages(suppressWarnings(load_library())))

