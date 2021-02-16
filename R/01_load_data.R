# ---- Conversion Functions ----
repair_as_comment <- function(x) {
  return(if_else(x == "", "Comment", x, NA_character_))
}

excel_col_to_date <- function(col) {
  # Fixes cases where a column or data is read as numeric not date
  col[!grepl("/", col)] <-
    as.character(
      as.Date(
        as.numeric(col[!grepl("/", col)]),
        origin = "1899-12-30"
      ),
      format = "%d/%m/%y"
    )
  return(col)
}
stopifnot(
  excel_col_to_date(
    c("5/2/18, 6/2/18", "43136", "11/7/18, 12/7/18", "43292")
  ) ==
    c("5/2/18, 6/2/18", "05/02/18", "11/7/18, 12/7/18", "11/07/18")
)

add_admission_id <- function(df) {
  df %>%
    mutate(Dates_screened = excel_col_to_date(Dates_screened)) %>%
    group_by(`UR number`) %>%
    mutate(
      Admission = row_number(),
      Total_admissions = n()
    ) %>%
    ungroup() %>%
    arrange(`UR number`, Dates_screened)
}

# ---- Load Excel Data ----
load_excel_data <- function(rel_path) {
  xlsx_data <- list()

  oliguria <- file.path(rel_path, "data/Creatinine change in oliguria 8.7.20.xlsx")
  creatinine <- file.path(rel_path, "data/Small changes in creatinine 8.7.20.xlsx")
  demographics <- file.path(rel_path, "data/Demographics pts screened out.xlsx")
  apd_extract <- file.path(rel_path, "data/COMET-Extract-APD 23 Oct 2018.xlsx")
  creat_furo <- file.path(rel_path, "data/ED_ICU_Creatinine_Furosemide.xlsx")

  xlsx_data$oliguria <- list(
    demographic = read_excel(oliguria, "Patient Demographics"),
    data_set = read_excel(oliguria, "Data set"),
    outcomes = read_excel(oliguria, "AKI & outcomes", .name_repair = repair_as_comment),
    screen_log = read_excel(oliguria, "Screening log", .name_repair = repair_as_comment) %>%
      add_admission_id(.)
  )
  xlsx_data$creatinine <- list(
    demographic = read_excel(creatinine, "Patient Demographics"),
    data_set = read_excel(creatinine, "Data set"),
    outcomes = read_excel(creatinine, "AKI & outcomes", .name_repair = repair_as_comment),
    screen_log = read_excel(creatinine, "Screening log", .name_repair = repair_as_comment) %>%
      add_admission_id(.)
  )
  xlsx_data$screen_out <- list(
    no_creatinine = read_excel(demographics, "no cr change", .name_repair = repair_as_comment),
    no_oliguria = read_excel(demographics, "no oliguria", .name_repair = repair_as_comment),
    neither_cr_ol = read_excel(demographics, "neither cr nor olig", .name_repair = repair_as_comment)
  )
  xlsx_data$apd_extract <- list(
    apd_extract = read_excel(apd_extract, "Admissions")
  )
  xlsx_data$creat_furo <- list(
    blood_gas = read_excel(creat_furo, "Blood Gas"),
    bio_chem = read_excel(creat_furo, "BioChem"),
    lowest_creat = read_excel(creat_furo, "Lowest Creatinine Level"),
    furosemide = read_excel(creat_furo, "Medication")
  )
  return(xlsx_data)
}


# ---- data_collection_errors ----
errors_logi <-
  (xlsx_data$creatinine$screen_log$`UR number` !=
    xlsx_data$oliguria$screen_log$`UR number`) |
    (xlsx_data$creatinine$screen_log$Dates_screened !=
      xlsx_data$oliguria$screen_log$Dates_screened) |
    (xlsx_data$creatinine$screen_log$Excl_criteria_ok !=
      xlsx_data$oliguria$screen_log$Excl_criteria_ok)

creatinine_errors <- xlsx_data$creatinine$screen_log[errors_logi, ]
oliguria_errors <- xlsx_data$oliguria$screen_log[errors_logi, ]

xlsx_data$excluded_UR_numbers <- creatinine_errors$`UR number`
xlsx_data$excluded_Pt_Study_no <- discard(
  c(oliguria_errors$Pt_Study_no, creatinine_errors$Pt_Study_no),
  is.na
)

cat(paste(
  # "Discarded UR Numbers:       ", paste(xlsx_data$excluded_UR_numbers , collapse = ", "), "\n",
  "Discarded Pt Study Numbers: ",
  paste(xlsx_data$excluded_Pt_Study_no, collapse = ", "), "\n"
))

kable(
  creatinine_errors[, c(13, 2:4, 12, 14)],
  caption = "Creatinine Potential Errors",
  booktabs = TRUE
)
kable(
  oliguria_errors[, c(13, 2:4, 12, 14)],
  caption = "Oliguria Potential Errors",
  booktabs = TRUE
)

# Correct data
xlsx_data$creatinine$screen_log[errors_logi, "Dates_screened"] <-
  xlsx_data$oliguria$screen_log[errors_logi, "Dates_screened"]

xlsx_data$creatinine$screen_log$errors_logi <- errors_logi
xlsx_data$creatinine$screen_log <- xlsx_data$creatinine$screen_log %>%
  mutate(
    Excl_criteria_ok = if_else(errors_logi, "N", Excl_criteria_ok),
    Already_AKI = if_else(errors_logi, "Y", Already_AKI),
    Incl_criteria_ok = if_else(errors_logi, NA_character_, Incl_criteria_ok),
    Epis_cr_change = if_else(errors_logi, NA_character_, Epis_cr_change),
    Pt_Study_no = if_else(errors_logi, NA_character_, Pt_Study_no),
    Total_no_cr_epis = if_else(errors_logi, NA_real_, Total_no_cr_epis)
  ) %>%
  select(-errors_logi)

xlsx_data$oliguria$screen_log$errors_logi <- errors_logi
xlsx_data$oliguria$screen_log <- xlsx_data$oliguria$screen_log %>%
  mutate(
    Excl_criteria_ok = if_else(errors_logi, "N", Excl_criteria_ok),
    Already_AKI = if_else(errors_logi, "Y", Already_AKI),
    Incl_criteria_ok = if_else(errors_logi, NA_character_, Incl_criteria_ok),
    Epis_olig = if_else(errors_logi, NA_character_, Epis_olig),
    Pt_Study_no = if_else(errors_logi, NA_character_, Pt_Study_no),
    Total_no_olig_epis = if_else(errors_logi, NA_real_, Total_no_olig_epis)
  ) %>%
  select(-errors_logi)

kable(
  xlsx_data$creatinine$screen_log[errors_logi, c(13, 2:4, 12, 14)],
  caption = "Creatinine Fixed Rows",
  booktabs = TRUE
)
kable(
  xlsx_data$oliguria$screen_log[errors_logi, c(13, 2:4, 12, 14)],
  caption = "Oliguria Fixed Rows",
  booktabs = TRUE
)

remove_excl_pt_study_no <- function(dataframe) {
  return(filter(dataframe, !(Pt_Study_no %in% xlsx_data$excluded_Pt_Study_no)))
}

xlsx_data$creatinine$demographic <- remove_excl_pt_study_no(xlsx_data$creatinine$demographic)
xlsx_data$oliguria$demographic <- remove_excl_pt_study_no(xlsx_data$oliguria$demographic)

xlsx_data$creatinine$outcomes <- remove_excl_pt_study_no(xlsx_data$creatinine$outcomes)
xlsx_data$oliguria$outcomes <- remove_excl_pt_study_no(xlsx_data$oliguria$outcomes)

rm(errors_logi, creatinine_errors, oliguria_errors, remove_excl_pt_study_no)
