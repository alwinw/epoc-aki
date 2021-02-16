rel_path <- "."
save_plots <- FALSE

# ---- Load Library Function ----
load_library <- function(req_file = "requirements.txt", bib_file = NULL) {
  packages <- readLines(req_file)
  new.packages <-
    packages[!(packages %in% utils::installed.packages()[, "Package"])]

  if (length(new.packages)) {
    install.packages(new.packages)
  }
  lapply(packages, library, character.only = TRUE)

  if (!is.null(bib_file)) {
    knitr::write_bib(unique(c(.packages(), packages)), file = bib_file)
  }
  return(NULL)
}

# ---- Environment Set up ----
load_library(
  file.path(rel_path, "requirements.txt"),
  file.path(rel_path, "doc/bib/R-references.bib")
)
theme_set(theme_bw())
options(knitr.table.format = "pipe")

rm(load_library)

# ---- General Utility Functions ----
uniqueN <- function(x) length(unique(x))
