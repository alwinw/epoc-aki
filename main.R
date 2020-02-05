#============================= Main Analysis Runner ============================
#                                Alwin Wang 2019

#------------------------------------ About ------------------------------------
# 1. Setup: Source data, check for errors, etc.
# 2. Build DFs: ID | Pt Info | Cre | Oli | Outcome
# 3. Build List: ID:{...}

#------------------------------- Source Scripts --------------------------------
source("R/001-set-up.R")
source("R/002-build-df.R")

#-------------------------------- Run Analysis ---------------------------------
# 1 Setup
invisible(suppressMessages(
  load_library("R-requirements.txt", "R-references.bib")
))

xlsx_data <- suppressMessages(load_excel_files())
names(xlsx_data)
xlsx_data <- data_collection_errors(xlsx_data)

# 2 Build DFs
screening_data <- merge_xlsx_screening(
  xlsx_data$creatinine$screen_log, xlsx_data$oliguria$screen_log)
glimpse(screening_data, width = 80)

pt_study_no <- generate_pt_study_no(screening_data)
print(pt_study_no)


merged_data <- merge_xlsx_creatinine_oliguria(screening_data, xlsx_data)
