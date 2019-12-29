# main entry point
# Alwin Wang 2019

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("library-manager.R")

oliguria_xlsx_path     = file.path("../data" ,"Creatinine change in oliguria 27.9.18.xlsx")
creatinine_xlsx_path   = file.path("../data" ,"Small changes in creatinine 27.9.18.xlsx")
demographics_xlsx_path = file.path("../data" ,"Demographics pts screened out.xlsx")

source("merge-excel.R")