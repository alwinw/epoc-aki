# Library Manager
# Alwin Wang 2019

packages <- c(
  "tidyverse", 
  "styler"
)
new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)

invisible(lapply(packages, library, character.only = TRUE))
rm(packages, new.packages)