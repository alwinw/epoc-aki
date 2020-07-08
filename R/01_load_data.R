rel_path = "."

# ---- excel_date_to_character_function ----

excel_date_to_character <- function(vector) {
  suppressWarnings(ifelse(grepl("/", vector), vector,
    as.character(as.Date(as.numeric(vector), origin = "1899-12-30"), format = "%d/%m/%y")
  ))
}


# ---- last_column_as_comment_function ----
last_column_as_comment <- function(data) {
  names(data)[ncol(data)] <- "Comment"
  return(data)
}


# ---- load_excel ----
xlsx_paths = list(
  oliguria     = file.path(rel_path, "data/Creatinine change in oliguria 8.7.20.xlsx"),
  creatinine   = file.path(rel_path, "data/Small changes in creatinine 8.7.20.xlsx"),
  demographics = file.path(rel_path, "data/Demographics pts screened out.xlsx"),
  apd_extract  = file.path(rel_path, "data/COMET-Extract-APD 23 Oct 2018.xlsx"),
  creat_furo   = file.path(rel_path, "data/ED_ICU_Creatinine_Furosemide.xlsx")
)

xlsx_data <- list()

xlsx_data$oliguria <- list(
  demographic = read_excel(xlsx_paths$oliguria, "Patient Demographics"),
  data_set    = read_excel(xlsx_paths$oliguria, "Data set"),
  outcomes    = read_excel(xlsx_paths$oliguria, "AKI & outcomes"),
  screen_log  = read_excel(xlsx_paths$oliguria, "Screening log")
)
xlsx_data$creatinine <- list(
  demographic = read_excel(xlsx_paths$creatinine, "Patient Demographics"),
  data_set    = read_excel(xlsx_paths$creatinine, "Data set"),
  outcomes    = read_excel(xlsx_paths$creatinine, "AKI & outcomes"),
  screen_log  = read_excel(xlsx_paths$creatinine, "Screening log")
)
xlsx_data$screen_out <- list(
  no_creatinine = read_excel(xlsx_paths$demographics, "no cr change"),
  no_oliguria   = read_excel(xlsx_paths$demographics, "no oliguria"),
  neither_cr_ol  = read_excel(xlsx_paths$demographics, "neither cr nor olig")
)
xlsx_data$apd_extract <- list(
  apd_extract = read_excel(xlsx_paths$apd_extract, "Admissions")
)
xlsx_data$creat_furo <- list(
  blood_gas    = read_excel(xlsx_paths$creat_furo, "Blood Gas"),
  bio_chem     = read_excel(xlsx_paths$creat_furo, "BioChem"),
  lowest_creat = read_excel(xlsx_paths$creat_furo, "Lowest Creatinine Level"),
  furosemide   = read_excel(xlsx_paths$creat_furo, "Medication")
)

xlsx_data$creatinine$screen_log    <- last_column_as_comment(xlsx_data$creatinine$screen_log   )
xlsx_data$oliguria  $screen_log    <- last_column_as_comment(xlsx_data$oliguria  $screen_log   )
xlsx_data$creatinine$outcomes      <- last_column_as_comment(xlsx_data$creatinine$outcomes     )
xlsx_data$oliguria  $outcomes      <- last_column_as_comment(xlsx_data$oliguria  $outcomes     )
xlsx_data$screen_out$no_creatinine <- last_column_as_comment(xlsx_data$screen_out$no_creatinine)
xlsx_data$screen_out$no_oliguria   <- last_column_as_comment(xlsx_data$screen_out$no_oliguria  )
xlsx_data$screen_out$neither_cr_ol <- last_column_as_comment(xlsx_data$screen_out$neither_cr_ol)

xlsx_data$oliguria$screen_log <- xlsx_data$oliguria$screen_log %>%
  mutate(Dates_screened = excel_date_to_character(Dates_screened)) %>%
  group_by(`UR number`) %>%
  mutate(
    Admission = row_number(),
    Total_admissions = n()) %>%
  ungroup() %>%
  arrange(`UR number`, Dates_screened)
xlsx_data$creatinine$screen_log <- xlsx_data$creatinine$screen_log %>%
  mutate(Dates_screened = excel_date_to_character(Dates_screened)) %>%
  group_by(`UR number`) %>%
  mutate(
    Admission = row_number(),
    Total_admissions = n()) %>%
  ungroup() %>%
  arrange(`UR number`, Dates_screened)

rm(xlsx_paths, excel_date_to_character, last_column_as_comment)


# ---- data_collection_errors ----
errors_logi =
  (xlsx_data$creatinine$screen_log$`UR number` !=
     xlsx_data$oliguria$screen_log$`UR number`) |
  (xlsx_data$creatinine$screen_log$Dates_screened !=
     xlsx_data$oliguria$screen_log$Dates_screened)  |
  (xlsx_data$creatinine$screen_log$Excl_criteria_ok !=
     xlsx_data$oliguria$screen_log$Excl_criteria_ok)

creatinine_errors = xlsx_data$creatinine$screen_log[errors_logi, ]
oliguria_errors = xlsx_data$oliguria$screen_log[errors_logi, ]

xlsx_data$excluded_UR_numbers = creatinine_errors$`UR number`
xlsx_data$excluded_Pt_Study_no = discard(
  c(oliguria_errors$Pt_Study_no, creatinine_errors$Pt_Study_no),
  is.na)

cat(paste(
  # "Discarded UR Numbers:       ", paste(xlsx_data$excluded_UR_numbers , collapse = ", "), "\n",
  "Discarded Pt Study Numbers: ",
  paste(xlsx_data$excluded_Pt_Study_no, collapse = ", "), "\n"
))

knitr::kable(
  creatinine_errors[, c(13, 2:4, 12, 14)], caption = 'Creatinine Potential Errors',
  booktabs = TRUE
)
knitr::kable(
  oliguria_errors[, c(13, 2:4, 12, 14)], caption = 'Oliguria Potential Errors',
  booktabs = TRUE
)

# Correct data
xlsx_data$creatinine$screen_log[errors_logi, "Dates_screened"] =
  xlsx_data$oliguria$screen_log[errors_logi, "Dates_screened"]


xlsx_data$creatinine$screen_log$errors_logi = errors_logi
xlsx_data$creatinine$screen_log <- xlsx_data$creatinine$screen_log %>%
  mutate(
    Excl_criteria_ok = if_else(errors_logi, "N", Excl_criteria_ok),
    Already_AKI      = if_else(errors_logi, "Y", Already_AKI),
    Incl_criteria_ok = if_else(errors_logi, NA_character_, Incl_criteria_ok),
    Epis_cr_change   = if_else(errors_logi, NA_character_, Epis_cr_change),
    Pt_Study_no      = if_else(errors_logi, NA_character_, Pt_Study_no),
    Total_no_cr_epis = if_else(errors_logi, NA_real_, Total_no_cr_epis)
  ) %>%
  select(-errors_logi)

xlsx_data$oliguria$screen_log$errors_logi = errors_logi
xlsx_data$oliguria$screen_log <- xlsx_data$oliguria$screen_log %>%
  mutate(
    Excl_criteria_ok = if_else(errors_logi, "N", Excl_criteria_ok),
    Already_AKI      = if_else(errors_logi, "Y", Already_AKI),
    Incl_criteria_ok = if_else(errors_logi, NA_character_, Incl_criteria_ok),
    Epis_olig        = if_else(errors_logi, NA_character_, Epis_olig),
    Pt_Study_no      = if_else(errors_logi, NA_character_, Pt_Study_no),
    Total_no_olig_epis = if_else(errors_logi, NA_real_, Total_no_olig_epis)
  ) %>%
  select(-errors_logi)

knitr::kable(
  xlsx_data$creatinine$screen_log[errors_logi, c(13, 2:4, 12, 14)], caption = 'Creatinine Fixed Rows',
  booktabs = TRUE
)
knitr::kable(
  xlsx_data$oliguria$screen_log[errors_logi, c(13, 2:4, 12, 14)], caption = 'Oliguria Fixed Rows',
  booktabs = TRUE
)

remove_pt_study_no <- function(dataframe) {
  return(filter(dataframe, !(Pt_Study_no %in% xlsx_data$excluded_Pt_Study_no)))
}

xlsx_data$creatinine$demographic <- remove_pt_study_no(xlsx_data$creatinine$demographic)
xlsx_data$oliguria$demographic   <- remove_pt_study_no(xlsx_data$oliguria$demographic)

xlsx_data$creatinine$outcomes <- remove_pt_study_no(xlsx_data$creatinine$outcomes)
xlsx_data$oliguria$outcomes   <- remove_pt_study_no(xlsx_data$oliguria$outcomes)

rm(errors_logi, creatinine_errors, oliguria_errors)
