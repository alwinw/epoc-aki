rel_path <- "."

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

rm(load_library)

# ---- Plotting ----
if (.Platform$OS.type == "windows") {
  withAutoprint({
    windowsFonts(
      serif = windowsFont("Times New Roman"),
      sans = windowsFont("Arial")
    )
  })
}

theme_set(theme_bw() + theme(text = element_text(family = "sans")))
options(knitr.table.format = "pipe")

if (.Platform$OS.type == "unix") {
  theme_set(theme_bw() + theme(text = element_text(family = "Helvetica")))
}

save_plot <- function(filename, plot, width, height, scale) {
  if (.Platform$OS.type == "windows") {
    ggsave(paste0(filename, ".png"), plot,
      path = paste0(rel_path, "/doc/images/"),
      width = width, height = height, scale = scale, dpi = 300
    )
  } else if (.Platform$OS.type == "unix") {
    plot <- plot + theme(plot.title = element_blank())
    ggsave(paste0(filename, ".eps"), plot,
      path = paste0(rel_path, "/doc/images/"),
      width = width, height = height, scale = scale, dpi = 300
    )
  }
}


# ---- General Utility Functions ----
uniqueN <- function(x) length(unique(x))
