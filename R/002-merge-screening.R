# Merge Excel screening log
# Alwin Wang 2019

# To do: search for "read_excel" in Paper_analysis.R

# Load Excel Files ----
load_excel_files <- function(path = NULL) {
  if (!is.null(path)){
    oliguria_xlsx_path     = file.path(path, oliguria_xlsx_path    )
    creatinine_xlsx_path   = file.path(path, creatinine_xlsx_path  )
    demographics_xlsx_path = file.path(path, demographics_xlsx_path)
    creat_furo_xlsx_path   = file.path(path, creat_furo_xlsx_path  )
  }
  oliguria_xlsx <- list(
    demographic = read_excel(oliguria_xlsx_path, "Patient Demographics"),
    data_set    = read_excel(oliguria_xlsx_path, "Data set"            ),
    outcomes    = read_excel(oliguria_xlsx_path, "AKI & outcomes"      ),
    screen_log  = read_excel(oliguria_xlsx_path, "Screening log"       )
  )
  creatinine_xlsx <- list(
    demographic = read_excel(creatinine_xlsx_path, "Patient Demographics"),
    data_set    = read_excel(creatinine_xlsx_path, "Data set"            ),
    outcomes    = read_excel(creatinine_xlsx_path, "AKI & outcomes"      ),
    screen_log  = read_excel(creatinine_xlsx_path, "Screening log"       )
  )
  demographics_xlsx <- list(
    num_creatinine = read_excel(demographics_xlsx_path, "no cr change"       ),
    num_oliguria   = read_excel(demographics_xlsx_path, "no oliguria"        ),
    neither_cr_ol  = read_excel(demographics_xlsx_path, "neither cr nor olig")
  )
  apd_extract_xlsx <- list(
    apd_extract = read_excel(apd_extract_xlsx_path, "Admissions")
  )
  creat_furo_xlsx <- list(
    blood_gas    = read_excel(creat_furo_xlsx_path, "Blood Gas"              ),
    bio_chem     = read_excel(creat_furo_xlsx_path, "BioChem"                ),
    lowest_creat = read_excel(creat_furo_xlsx_path, "Lowest Creatinine Level"),
    furosemide   = read_excel(creat_furo_xlsx_path, "Medication"             )
  )
  
  names(creatinine_xlsx$screen_log)[ncol(creatinine_xlsx$screen_log)] <- "Comment"
  names(oliguria_xlsx  $screen_log)[ncol(oliguria_xlsx  $screen_log)] <- "Comment"
  
  xlsx_data <- list(
    oliguria     = oliguria_xlsx    ,
    creatinine   = creatinine_xlsx  ,
    demographics = demographics_xlsx,
    apd_extract  = apd_extract_xlsx ,
    time_series  = creat_furo_xlsx
  )
  return(xlsx_data)
}

excel_date_to_character <- function(vector) {
  suppressWarnings(
  ifelse(
    grepl("/", vector), 
    vector, 
    as.character(as.Date(as.numeric(vector), origin = "1899-12-30"), format = "%d/%m/%y"))
  )
}

convert_excel <- function(xlsx_data) {
  xlsx_data$oliguria$screen_log   %<>% arrange(`UR number`, Dates_screened) %>% 
    mutate(Dates_screened = excel_date_to_character(Dates_screened))
  xlsx_data$creatinine$screen_log %<>% arrange(`UR number`, Dates_screened) %>% 
    mutate(Dates_screened = excel_date_to_character(Dates_screened))
  return(xlsx_data)
}

# Data collection errors ----
data_collection_errors <- function(xlsx_data) {
  errors_logi = 
    (xlsx_data$creatinine$screen_log$`UR number`      != xlsx_data$oliguria$screen_log$`UR number`     ) |
    (xlsx_data$creatinine$screen_log$Dates_screened   != xlsx_data$oliguria$screen_log$Dates_screened  ) |
    (xlsx_data$creatinine$screen_log$Excl_criteria_ok != xlsx_data$oliguria$screen_log$Excl_criteria_ok)
  creatinine_errors = xlsx_data$creatinine$screen_log[errors_logi, ]
  oliguria_errors   = xlsx_data$oliguria  $screen_log[errors_logi, ]
  
  xlsx_data$excluded_UR_numbers  = creatinine_errors$`UR number`
  xlsx_data$excluded_Pt_Study_no = discard(c(oliguria_errors$Pt_Study_no, creatinine_errors$Pt_Study_no), is.na)
  
  cat("Creatinine xlsx sheet\n")
  print(creatinine_errors[, c(13, 2:4)])
  cat("\nOliguria xlsx sheet\n"  )
  print(oliguria_errors  [, c(13, 2:4)])
  
  cat(paste("\n",
    # "Discarded UR Numbers:       ", paste(xlsx_data$excluded_UR_numbers , collapse = ", "), "\n",
    "Discarded Pt Study Numbers: ", paste(xlsx_data$excluded_Pt_Study_no, collapse = ", "), "\n"
  ))
  
  # Discard data from xlsx_data
  xlsx_data$creatinine$screen_log[errors_logi, "Dates_screened"] = 
    xlsx_data$oliguria$screen_log[errors_logi, "Dates_screened"]
  
  xlsx_data$creatinine$screen_log[errors_logi, "Excl_criteria_ok"  ] = "N"
  xlsx_data$creatinine$screen_log[errors_logi, "Incl_criteria_ok"  ] = NA
  xlsx_data$creatinine$screen_log[errors_logi, "Already_AKI"       ] = "Y"
  xlsx_data$creatinine$screen_log[errors_logi, "Epis_cr_change"    ] = NA
  xlsx_data$creatinine$screen_log[errors_logi, "Total_no_cr_epis"  ] = NA
  
  xlsx_data$oliguria  $screen_log[errors_logi, "Excl_criteria_ok"  ] = "N"
  xlsx_data$oliguria  $screen_log[errors_logi, "Incl_criteria_ok"  ] = NA
  xlsx_data$oliguria  $screen_log[errors_logi, "Already_AKI"       ] = "Y"
  xlsx_data$oliguria  $screen_log[errors_logi, "Epis_olig"         ] = NA
  xlsx_data$oliguria  $screen_log[errors_logi, "Total_no_olig_epis"] = NA
  
  return(xlsx_data)
}

# Merge screening logs -----
merge_xlsx_screening <- function(xlsx_data) {
  merge_columns = setdiff(
    intersect(colnames(xlsx_data$creatinine$screen_log), 
              colnames(xlsx_data$oliguria$screen_log)),
    c("Incl_criteria_ok", "Pt_Study_no", "Comment")
  )
  analysis_data = full_join(
    xlsx_data$creatinine$screen_log,
    xlsx_data$oliguria  $screen_log,
    by     = merge_columns,
    suffix = c("_crch", "_olig")
    )
  
  if (anyNA(analysis_data$`UR number`)) {
    stop("NA in UR number of merged Excel sheets")
  }
  
  creatinine_n_obs = nrow(xlsx_data$creatinine$screen_log)
  oliguria_n_obs   = nrow(xlsx_data$oliguria  $screen_log)
  analysis_n_obs   = nrow(analysis_data)
  if (length(unique(c(creatinine_n_obs, oliguria_n_obs, analysis_n_obs))) != 1) {
    stop(paste0(
      "Inconsistent number of n_obs after UR numbers discarded. ",
      "Creatinine: ", creatinine_n_obs, ", ",
      "Oliguria: "  , oliguria_n_obs  , ", ",
      "Merged: "    , analysis_n_obs  , ", "))
  }
  
  logi_colnames <- colnames(analysis_data)[
    !grepl("UR number|Dates_screened|Pt_Study_no|Total_no_|Comment", colnames(analysis_data))]
  analysis_data <- analysis_data %>% 
    mutate_at(logi_colnames, function(x) if_else(x == "N", FALSE, TRUE))

  return(analysis_data)
}

# Flow chart here
