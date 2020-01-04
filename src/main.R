# main entry point
# Alwin Wang 2019

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("library-manager.R")

oliguria_xlsx_path     = file.path("../data", "Creatinine change in oliguria 4.1.20.xlsx" ) # Refer to AKI Outcomes D120
creatinine_xlsx_path   = file.path("../data", "Small changes in creatinine 27.9.18.xlsx"  )
demographics_xlsx_path = file.path("../data", "Demographics pts screened out.xlsx"        )
creat_furo_xlsx_path   = file.path("../data", "ED_ICU_Creatinine_Furosemide.xlsx"         )

source("merge-excel.R")