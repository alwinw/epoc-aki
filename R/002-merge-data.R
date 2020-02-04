#====================== Merge Excel data sources together ======================
#                                Alwin Wang 2019

#------------------------------- Load Excel Data -------------------------------

#' Convert Excel date to formatted date string
#' 
#' Some columns will be a mix of "43294" and "13/7/18, 16/7/18" characters. 
#' The "43294" columns will be converted into "13/7/18 format
#' 
#' @param vector A vector containing a mix of Excel dates and strings as strings
#' 
#' @return A vector of human readable dates
#' 
#' @examples 
#' excel_date_to_character(c("43294", "13/7/18"))

excel_date_to_character <- function(vector) {
  suppressWarnings(ifelse(
    grepl("/", vector),
    vector,
    as.character(as.Date(as.numeric(vector), origin = "1899-12-30"),
                 format = "%d/%m/%y")
  ))
}

#' Load Excel Files
#'
#' Loads each of the excel files into a single xlsx list
#'
#' @param path add an optional path to the (global) xlsx paths
#'
#' @return List of list of dataframes containing data from xlsx files
#' 
#' @examples
#' xlsx_data <- load_excel_files()

load_excel_files <- function(path = NULL) {
  if (!is.null(path)) {
    oliguria_xlsx_path     = file.path(path, oliguria_xlsx_path)
    creatinine_xlsx_path   = file.path(path, creatinine_xlsx_path)
    demographics_xlsx_path = file.path(path, demographics_xlsx_path)
    creat_furo_xlsx_path   = file.path(path, creat_furo_xlsx_path)
    apd_extract_xlsx_path  = file.path(path, apd_extract_xlsx_path)
  }
  oliguria_xlsx <- list(
    demographic = read_excel(oliguria_xlsx_path, "Patient Demographics"),
    data_set    = read_excel(oliguria_xlsx_path, "Data set"),
    outcomes    = read_excel(oliguria_xlsx_path, "AKI & outcomes"),
    screen_log  = read_excel(oliguria_xlsx_path, "Screening log")
  )
  creatinine_xlsx <- list(
    demographic = read_excel(creatinine_xlsx_path, "Patient Demographics"),
    data_set    = read_excel(creatinine_xlsx_path, "Data set"),
    outcomes    = read_excel(creatinine_xlsx_path, "AKI & outcomes"),
    screen_log  = read_excel(creatinine_xlsx_path, "Screening log")
  )
  demographics_xlsx <- list(
    num_creatinine = read_excel(demographics_xlsx_path, "no cr change"),
    num_oliguria   = read_excel(demographics_xlsx_path, "no oliguria"),
    neither_cr_ol  = read_excel(demographics_xlsx_path, "neither cr nor olig")
  )
  apd_extract_xlsx <- list(
    apd_extract = read_excel(apd_extract_xlsx_path, "Admissions")
  )
  creat_furo_xlsx <- list(
    blood_gas    = read_excel(creat_furo_xlsx_path, "Blood Gas"),
    bio_chem     = read_excel(creat_furo_xlsx_path, "BioChem"),
    lowest_creat = read_excel(creat_furo_xlsx_path, "Lowest Creatinine Level"),
    furosemide   = read_excel(creat_furo_xlsx_path, "Medication")
  )
  
  names(creatinine_xlsx$screen_log)[ncol(creatinine_xlsx$screen_log)] <-
    "Comment"
  names(oliguria_xlsx$screen_log)[ncol(oliguria_xlsx$screen_log)] <-
    "Comment"
  
  oliguria_xlsx$screen_log   %<>% arrange(`UR number`, Dates_screened) %>%
    mutate(Dates_screened = excel_date_to_character(Dates_screened))
  creatinine_xlsx$screen_log %<>% arrange(`UR number`, Dates_screened) %>%
    mutate(Dates_screened = excel_date_to_character(Dates_screened))
  
  xlsx_data <- list(
    oliguria     = oliguria_xlsx,
    creatinine   = creatinine_xlsx,
    demographics = demographics_xlsx,
    apd_extract  = apd_extract_xlsx,
    time_series  = creat_furo_xlsx
  )
  return(xlsx_data)
}

#---------------------------- Data Collection Check ----------------------------

#' Check for data collection errors
#' 
#' Compares creatinine and oliguria data to look for discrepencies in the 
#' UR number, Dates screened and Excl criteria ok
#' 
#' @param xlsx_data A list containing all the xlsx data
#' 
#' @return Prints out possible errors. Returns corrected xlsx data and the 
#' exlucluded patients
#' 
#' @examples
#' xlsx_data <- data_collection_errors(xlsx_data)

data_collection_errors <- function(xlsx_data) {
  errors_logi =
    (xlsx_data$creatinine$screen_log$`UR number`      != xlsx_data$oliguria$screen_log$`UR number`)     |
    (xlsx_data$creatinine$screen_log$Dates_screened   != xlsx_data$oliguria$screen_log$Dates_screened)  |
    (xlsx_data$creatinine$screen_log$Excl_criteria_ok != xlsx_data$oliguria$screen_log$Excl_criteria_ok)
  creatinine_errors = xlsx_data$creatinine$screen_log[errors_logi, ]
  oliguria_errors = xlsx_data$oliguria$screen_log[errors_logi, ]
  
  xlsx_data$excluded_UR_numbers = creatinine_errors$`UR number`
  xlsx_data$excluded_Pt_Study_no = discard(
    c(oliguria_errors$Pt_Study_no, creatinine_errors$Pt_Study_no),
    is.na)
  
  cat("Creatinine xlsx sheet\n")
  print(creatinine_errors[, c(13, 2:4)])
  cat("\nOliguria xlsx sheet\n")
  print(oliguria_errors  [, c(13, 2:4)])
  
  cat(paste(
    "\n",
    # "Discarded UR Numbers:       ", paste(xlsx_data$excluded_UR_numbers , collapse = ", "), "\n",
    "Discarded Pt Study Numbers: ",
    paste(xlsx_data$excluded_Pt_Study_no, collapse = ", "),
    "\n"
  ))
  
  # Discard data from xlsx_data
  xlsx_data$creatinine$screen_log[errors_logi, "Dates_screened"] =
    xlsx_data$oliguria$screen_log[errors_logi, "Dates_screened"]
  
  xlsx_data$creatinine$screen_log[errors_logi, "Excl_criteria_ok"] = "N"
  xlsx_data$creatinine$screen_log[errors_logi, "Incl_criteria_ok"] = NA
  xlsx_data$creatinine$screen_log[errors_logi, "Already_AKI"] = "Y"
  xlsx_data$creatinine$screen_log[errors_logi, "Epis_cr_change"] = NA
  xlsx_data$creatinine$screen_log[errors_logi, "Total_no_cr_epis"] = NA
  
  xlsx_data$oliguria$screen_log[errors_logi, "Excl_criteria_ok"] = "N"
  xlsx_data$oliguria$screen_log[errors_logi, "Incl_criteria_ok"] = NA
  xlsx_data$oliguria$screen_log[errors_logi, "Already_AKI"] = "Y"
  xlsx_data$oliguria$screen_log[errors_logi, "Epis_olig"] = NA
  xlsx_data$oliguria$screen_log[errors_logi, "Total_no_olig_epis"] = NA
  
  return(xlsx_data)
}

#---------------------------- Merge Screening Logs -----------------------------

