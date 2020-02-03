# main entry point
# Alwin Wang 2019

# Source scripts ----
source("R/000-data-source.R")
source("R/001-library-manager.R")
source("R/002-merge-screening.R")


source("R/005-merge-collection.R")

# Run ----
invisible(suppressMessages(load_library()))

xlsx_data <- suppressMessages(load_excel_files())
xlsx_data <- convert_excel(xlsx_data)
xlsx_data <- data_collection_errors(xlsx_data)
screening_data <- merge_xlsx_screening(xlsx_data)

analysis_data <- merge_xlsx_creatinine_oliguria(screening_data, xlsx_data)
