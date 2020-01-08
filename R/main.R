# main entry point
# Alwin Wang 2019

# Source scripts ----
source("R/000-data-source.R")
source("R/001-library-manager.R")
source("R/002-merge-excel.R")

# setwd ----
# Must source script using ctrl+shift+enter
# More robust method to come
setwd(dirname(parent.frame(2)$ofile))

# Run ----
xlsx_data <- suppressMessages(load_excel_files())
xlsx_data <- convert_excel(xlsx_data)
xlsx_data  <-  data_collection_errors(xlsx_data)

analysis_data <- merge_xlsx_screening(xlsx_data)
analysis_data <- merge_xlsx_sheets(analysis_data, xlsx_data)
