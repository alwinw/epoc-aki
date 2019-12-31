# Merge Excel Files
# Alwin Wang 2019

suppressMessages(
oliguria_xlsx_data <- list(
  demographic = read_excel(oliguria_xlsx_path, "Patient Demographics"),
  data_set    = read_excel(oliguria_xlsx_path, "Data set"            ),
  outcomes    = read_excel(oliguria_xlsx_path, "AKI & outcomes"      ),
  screen_log  = read_excel(oliguria_xlsx_path, "Screening log"       )
))
suppressMessages(
creatinine_xlsx_data <- list(
  demographic = read_excel(creatinine_xlsx_path, "Patient Demographics"),
  data_set    = read_excel(creatinine_xlsx_path, "Data set"            ),
  outcomes    = read_excel(creatinine_xlsx_path, "AKI & outcomes"      ),
  screen_log  = read_excel(creatinine_xlsx_path, "Screening log"       )
))
suppressMessages(
demographics_xlsx_data <- list(
  num_creatinine = read_excel(demographics_xlsx_path, "no cr change"       ),
  num_oliguria   = read_excel(demographics_xlsx_path, "no oliguria"        ),
  neither_cr_ol  = read_excel(demographics_xlsx_path, "neither cr nor olig")
))

#

excel_date_to_character <- function(vector) {
  suppressWarnings(
  ifelse(
    grepl("/", vector), 
    vector, 
    as.character(as.Date(as.numeric(vector), origin = "1899-12-30"), format = "%d/%m/%y"))
  )
}

oliguria_xlsx_data$screen_log   %<>% arrange(`UR number`, Dates_screened) %>% 
  mutate(Dates_screened = excel_date_to_character(Dates_screened))

creatinine_xlsx_data$screen_log %<>% arrange(`UR number`, Dates_screened) %>% 
  mutate(Dates_screened = excel_date_to_character(Dates_screened))

#

data_collection_errors = 
  (creatinine_xlsx_data$screen_log$`UR number`      != oliguria_xlsx_data$screen_log$`UR number`     ) |
  (creatinine_xlsx_data$screen_log$Dates_screened   != oliguria_xlsx_data$screen_log$Dates_screened  ) |
  (creatinine_xlsx_data$screen_log$Excl_criteria_ok != oliguria_xlsx_data$screen_log$Excl_criteria_ok)

creatinine_collection_errors = creatinine_xlsx_data$screen_log[data_collection_errors,]
oliguria_collection_errors   = oliguria_xlsx_data  $screen_log[data_collection_errors,]

excluded_UR_numbers  =   creatinine_collection_errors$`UR number`
excluded_Pt_Study_no = discard(c(oliguria_collection_errors$Pt_Study_no, creatinine_collection_errors$Pt_Study_no), is.na)
cat(paste(
  "\n",
  "Discarded UR Numbers:       ", paste(excluded_UR_numbers , collapse = ", "), "\n",
  "Discarded Pt Study Numbers: ", paste(excluded_Pt_Study_no, collapse = ", "), "\n"
))

#

merge_columns = setdiff(
  intersect(colnames(creatinine_xlsx_data$screen_log), colnames(oliguria_xlsx_data$screen_log)),
  c("Incl_criteria_ok", "Pt_Study_no", tail(colnames(creatinine_xlsx_data$screen_log),1))
)
merged_xlsx_data = list(screen_log = 
    full_join(
      filter(creatinine_xlsx_data$screen_log, !(`UR number` %in% excluded_UR_numbers)),
      filter(oliguria_xlsx_data  $screen_log, !(`UR number` %in% excluded_UR_numbers)),
      by     = merge_columns,
      suffix = c("_cre", "_oli")
))
if (anyNA(merged_xlsx_data$screen_log$`UR number`)) {
  stop("NA in UR number of merged Excel sheets")
}
creatinine_xlsx_n_obs = nrow(filter(creatinine_xlsx_data$screen_log, !(`UR number` %in% excluded_UR_numbers)))
oliguria_xlsx_n_obs   = nrow(filter(oliguria_xlsx_data$screen_log  , !(`UR number` %in% excluded_UR_numbers)))
merged_xlsx_n_obs     = nrow(merged_xlsx_data$screen_log)
if (!all(sapply(list(creatinine_xlsx_n_obs, oliguria_xlsx_n_obs),
    function(x) x == merged_xlsx_n_obs))) {
  stop(paste0(
    "Inconsistent number of n_obs after UR numbers discarded. ",
    "Creatinine: ", creatinine_xlsx_n_obs, ", ",
    "Oliguria: "  , oliguria_xlsx_n_obs,   ", ",
    "Merged: "    , merged_xlsx_n_obs,     ", "))
}

