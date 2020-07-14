rel_path = "."

# ---- load_library_function ----
load_library <- function(req_file = "requirements.txt", bib_file = NULL) {
      # Load and install necessary packages from text file
      packages <- readLines(req_file)
      new.packages <-
        packages[!(packages %in% installed.packages()[, "Package"])]

      if (length(new.packages))
        install.packages(new.packages)

      lapply(packages, library, character.only = TRUE)

      # update.packages()

      if (!is.null(bib_file))
        knitr::write_bib(packages, file = bib_file)

      return(NULL)
    }

# ---- environment_setup ----
load_library(
  file.path(rel_path, "requirements.txt"),
  file.path(rel_path, "doc/bib/R-references.bib")
)
theme_set(theme_bw())
