# Library Manager
# Alwin Wang 2019

load_library <- function() {
  packages <- c(
    "epiR",
    "knitr",
    "tidyverse", 
    "readxl",
    "roxygen2",
    "styler"
  )
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  invisible(lapply(packages, library, character.only = TRUE))
}

suppressMessages(suppressWarnings(load_library()))

