file_sources <- list.files(
  path = "R/",
  pattern = "^[0-9][0-9].*.R$",
  full.names = TRUE
)

sapply(
  file_sources,
  function(file) {
    cat(paste0("\n", file, "\n"))
    source(file, .GlobalEnv)
  }
)

rm(file_sources)
