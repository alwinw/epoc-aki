#============================= Main Analysis Runner ============================
#                                Alwin Wang 2019

#------------------------------- Source Scripts --------------------------------
source("R/000-data-source.R")
source("R/001-library-manager.R")
source("R/002-merge-data.R")

#-------------------------------- Run Analysis ---------------------------------
invisible(suppressMessages(
  load_library("R-requirements.txt", "references/R-references.bib")
))

xlsx_data <- suppressMessages(load_excel_files())
xlsx_data <- data_collection_errors(xlsx_data)

screening_data <- merge_xlsx_screening(xlsx_data)
merged_data <- merge_xlsx_creatinine_oliguria(screening_data, xlsx_data)
