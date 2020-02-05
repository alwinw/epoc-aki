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
  
  cat("Creatinine xlsx sheet\n")
  print(creatinine_errors[, c(13, 2:4)])
  cat("\nOliguria xlsx sheet\n")
  print(oliguria_errors[, c(13, 2:4)])
  cat(paste(
    "\n",
    # "Discarded UR Numbers:       ", paste(xlsx_data$excluded_UR_numbers , collapse = ", "), "\n",
    "Discarded Pt Study Numbers: ",
    paste(xlsx_data$excluded_Pt_Study_no, collapse = ", "),
    "\n"
  ))
  
  # Correct data  
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

#' Merge the screening log Excel sheets from creatinine and oliguria
#' 
#' Takes the xlsx data and merges the creatinine and oliguria screening logs 
#' together using all columns except incl criteria, pt study number and comment.
#' Turns binary data into TRUE / FALSE / NA
#' 
#' @param xlsx_data A list containing all xlsx data
#' 
#' @return A dataframe of screening logs combined from creatinine and oliguria
#' 
#' @examples 
#' screening_data <- merge_xlsx_screening(xlsx_data)

merge_xlsx_screening <- function(xlsx_data) {
  # Merge all columns except incl criteria, pt study number and comment
  merge_columns = setdiff(intersect(
    colnames(xlsx_data$creatinine$screen_log),
    colnames(xlsx_data$oliguria$screen_log)
  ),
  c("Incl_criteria_ok", "Pt_Study_no", "Comment"))
  
  screening_data = full_join(
    xlsx_data$creatinine$screen_log,
    xlsx_data$oliguria$screen_log,
    by     = merge_columns,
    suffix = c("_crch", "_olig")
  )
  
  # Check output
  if (anyNA(screening_data$`UR number`)) {
    stop("NA in UR number of merged Excel sheets")
  }
  creatinine_n_obs = nrow(xlsx_data$creatinine$screen_log)
  oliguria_n_obs   = nrow(xlsx_data$oliguria$screen_log)
  screening_n_obs  = nrow(screening_data)
  if (length(unique(c(creatinine_n_obs, oliguria_n_obs, screening_n_obs))) != 1) {
    stop(paste0(
      "Inconsistent number of n_obs after UR numbers discarded. ",
      "Creatinine: ", creatinine_n_obs, ", ",
      "Oliguria: "  , oliguria_n_obs  , ", ",
      "Merged: "    , screening_n_obs , ", "
      ))
  }
  
  # Turn columns into TRUE/FALSE/NA logicals except some
  logi_colnames <- colnames(screening_data)[!grepl(
    "UR number|Dates_screened|Pt_Study_no|Total_no_|Comment",
    colnames(screening_data)
  )]
  screening_data <- screening_data %>%
    mutate_at(logi_colnames, function(x)
      if_else(x == "N", FALSE, TRUE))
  
  return(screening_data)
}

#--------------------------- Screening Log Flowchart ---------------------------

# TBC

#------------------------- Merge Excel Data Collection -------------------------

#' Find column names
#' 
#' Find columns containing a text phrase at the start or end
#' 
#' @param text A string containing the search phrase
#' @param replace A string to replace the text search phrase
#' @param colnames A vector of column names to search through
#' 
#' @return A dataframe of column number, original column name found and the
#' replaced matching column name
#' 
#' @examples 
#' find_cols("date", "DateTime", c("date_start", "end_date", "mid_date_"))

find_cols <- function(text, replace, colnames) {
  cols <- data.frame(
    i = grep(paste0("^", text, "|", text, "$"), colnames, ignore.case = TRUE),
    j = grep(paste0("^", text, "|", text, "$"), colnames, ignore.case = TRUE, value = TRUE),
    stringsAsFactors = FALSE
  ) %>% 
    mutate(k = gsub(text, replace, j, ignore.case = TRUE))
  colnames(cols) <- c(paste0(text, "_i"), paste0(text), "match")
  
  return(cols)
}

#' Merge xlsx file sheets together
#' 
#' Takes creatinine or oliguria and joins the demographics and outcomes sheets
#' together. Then, merges date and time columns into a single DateTime column
#' 
#' @param data Is either creatinine or oliguria
#' @param excluded_Pt_Study_no Used to filter out any data collection errors
#' @param data_name Automatically grabs the name of the data passesd in
#' 
#' @return Dataframe of combined sheets
#' 
#' @examples 
#' creatinine = merge_xlsx_sheets(xlsx_data$creatinine, xlsx_data$excluded_Pt_Study_no)

merge_xlsx_sheets <- function(data, excluded_Pt_Study_no,
                              data_name = deparse(substitute(data))) {
  combined_sheets <- data$data_set %>% 
    fill(Pt_Study_no) %>% 
    full_join(., data$demographic, by = "Pt_Study_no") %>% 
    full_join(., data$outcomes   , by = "Pt_Study_no")
  
  if (nrow(combined_sheets) != nrow(data$data_set)) {
    stop(paste0(
      "Inconsistent number of rows after merging data_set with demographics and outcomes. ",
      "Data: "         , data_name          , ", ",
      "Data set rows: ", nrow(data$data_set), ", ",
      "Merged rows: "  , nrow(combined_sheets), ", "
    ))
  }
  
  dttm_col = inner_join(
    find_cols("date", "DateTime", colnames(combined_sheets)),
    find_cols("time", "DateTime", colnames(combined_sheets)), 
    by = "match") %>% 
    select(date, time, match)
  # Instead, consider using mutate_at to convert date and time columns and paste
  # then into the date columns. Then use rename at to conver to a datetime name
  # and then remove the time columns.
  
  combined_sheets <- combined_sheets %>% 
    select(-last_col()) %>% 
    filter(!(Pt_Study_no %in% excluded_Pt_Study_no)) %>% 
    pivot_longer(
      cols = c(dttm_col$date, dttm_col$time),
      names_to = "dttm_name",
      values_to = "dttm"
    ) %>% 
    mutate(
      type = if_else(grepl("date", dttm_name, ignore.case = TRUE), "date", "time"),
      dttm_name = gsub("date|time", "DateTime", dttm_name, ignore.case = TRUE)
    ) %>% 
    pivot_wider(
      names_from = "type",
      values_from = "dttm"
    ) %>% 
    mutate(
      datetime = ifelse(
        (is.na(date) | is.na(time)),
        NA,
        paste(
          format(date, format = "%Y-%m-%d"),
          format(time, format = "%H:%M:%S")
        )),
      date = NULL,
      time = NULL
    ) %>% 
    pivot_wider(
      names_from = "dttm_name",
      values_from = "datetime"
    )
  
  return(combined_sheets)
}

#' Merge creatinine and oliguria data
#' 
#' After each of the demographics and outcomes sheets are merged, merge the 
#' creatinine and oliguria together and with the screening data
#' 
#' @param screening_data Merged creatinine and oliguria screening data
#' @param xlsx_data A list containing all xlsx data
#' 
#' @return A dataframe of merged variables
#' 
#' @examples 
#' merged_data <- merge_xlsx_creatinine_oliguria(screening_data, xlsx_data)

merge_xlsx_creatinine_oliguria <- function(screening_data, xlsx_data) {
  creatinine = merge_xlsx_sheets(xlsx_data$creatinine, xlsx_data$excluded_Pt_Study_no)
  oliguria   = merge_xlsx_sheets(xlsx_data$oliguria  , xlsx_data$excluded_Pt_Study_no)
  
  creatinine <- creatinine %>% 
    mutate(
      ICU_LOS              = as.duration(DateTime_ICU_admit  %--% DateTime_ICU_dc ) / ddays (1),
      Hosp_LOS             = as.duration(DateTime_hosp_admit %--% DateTime_hosp_dc) / ddays (1),
      ICUadmtoAKIDx        = as.duration(DateTime_ICU_admit  %--% DateTime_AKI_Dx ) / dhours(1),
      Time_betw_ABG        = as.duration(`T-4_ABG_DateTime`  %--% T0_ABG_DateTime ) / dhours(1),
      Time_betw_cr_AKI     = as.duration(T0_ABG_DateTime     %--% DateTime_AKI_Dx ) / dhours(1),
      Time_betw_ICU_cr     = as.duration(DateTime_ICU_admit  %--% T0_ABG_DateTime ) / dhours(1),
      
      crchange             = (T0_ABG_Cr - `T-4_ABG_Cr`),
      delta_cr             =  crchange  / Time_betw_ABG,
      percent_delta_cr     = (crchange  / `T-4_ABG_Cr`) * 100 / Time_betw_ABG,
      
      akistagesv2          = ifelse(AKI_ICU == 0, 0, AKI_stage),
      aki_stg2or3          = ifelse(akistagesv2 >= 2, 1, 0),
      cr_aki_in12h         = ifelse(Time_betw_cr_AKI > 12 | AKI_ICU == 0, 0, 1),
      cr_aki_in24h         = ifelse(Time_betw_cr_AKI > 24 | AKI_ICU == 0, 0, 1),
      cr_aki_in48h         = ifelse(Time_betw_cr_AKI > 48 | AKI_ICU == 0, 0, 1),
      cr_aki_2or3_in12h    = ifelse(aki_stg2or3 == 1 & cr_aki_in12h == 1, 1, 0),
      cr_aki_2or3_in24h    = ifelse(aki_stg2or3 == 1 & cr_aki_in24h == 1, 1, 0),
      cr_aki_2or3_in48h    = ifelse(aki_stg2or3 == 1 & cr_aki_in48h == 1, 1, 0),
      
      craki_stg2or3        = ifelse(Cr_defined_AKI_stage >= 2, 1, 0),
      cr_craki_in12h       = ifelse(Time_betw_cr_AKI > 12 | Cr_defined_AKI == 0, 0, 1),
      cr_craki_in24h       = ifelse(Time_betw_cr_AKI > 24 | Cr_defined_AKI == 0, 0, 1),
      cr_craki_in48h       = ifelse(Time_betw_cr_AKI > 48 | Cr_defined_AKI == 0, 0, 1),
      cr_craki_2or3_in12h  = ifelse(craki_stg2or3 == 1 & cr_craki_in12h == 1, 1, 0),
      cr_craki_2or3_in24h  = ifelse(craki_stg2or3 == 1 & cr_craki_in24h == 1, 1, 0),
      cr_craki_2or3_in48h  = ifelse(craki_stg2or3 == 1 & cr_craki_in48h == 1, 1, 0)
    )
  
  oliguria <- oliguria %>% 
    mutate(
      ICU_LOS              = as.duration(DateTime_ICU_admit  %--% DateTime_ICU_dc   ) / ddays (1),
      Hosp_LOS             = as.duration(DateTime_hosp_admit %--% DateTime_hosp_dc  ) / ddays (1),
      ICUadmtoAKIDx        = as.duration(DateTime_ICU_admit  %--% DateTime_AKI_Dx   ) / dhours(1),
      Time_betw_ABG        = as.duration(`T-4_ABG_DateTime`  %--% T0_ABG_DateTime   ) / dhours(1),
      Time_betw_oli_AKI    = as.duration(DateTime_olig_epis  %--% DateTime_AKI_Dx   ) / dhours(1),
      Time_betw_ICU_oli    = as.duration(DateTime_ICU_admit  %--% DateTime_olig_epis) / dhours(1),
      
      crchange             = (T0_ABG_Cr - `T-4_ABG_Cr`),
      delta_cr             =  crchange  / Time_betw_ABG,
      percent_delta_cr     = (crchange  / `T-4_ABG_Cr`) * 100 / Time_betw_ABG,
      T0_UO_wtadjusted     = (T0_UO / 4)/Wt,
      
      akistagesv2          = ifelse(AKI_ICU == 0, 0, AKI_stage),
      aki_stg2or3          = ifelse(akistagesv2 >= 2, 1, 0),
      oli_aki_in12h        = ifelse(Time_betw_oli_AKI > 12 | AKI_ICU == 0, 0, 1),
      oli_aki_in24h        = ifelse(Time_betw_oli_AKI > 24 | AKI_ICU == 0, 0, 1),
      oli_aki_in48h        = ifelse(Time_betw_oli_AKI > 48 | AKI_ICU == 0, 0, 1),
      oli_aki_2or3_in12h   = ifelse(aki_stg2or3 == 1 & oli_aki_in12h == 1, 1, 0),
      oli_aki_2or3_in24h   = ifelse(aki_stg2or3 == 1 & oli_aki_in24h == 1, 1, 0),
      oli_aki_2or3_in48h   = ifelse(aki_stg2or3 == 1 & oli_aki_in48h == 1, 1, 0),
      
      craki_stg2or3        = ifelse(Cr_defined_AKI_stage >= 2, 1, 0),
      oli_craki_in12h      = ifelse(Time_betw_oli_AKI > 12 | Cr_defined_AKI == 0, 0, 1),
      oli_craki_in24h      = ifelse(Time_betw_oli_AKI > 24 | Cr_defined_AKI == 0, 0, 1),
      oli_craki_in48h      = ifelse(Time_betw_oli_AKI > 48 | Cr_defined_AKI == 0, 0, 1),
      oli_craki_2or3_in12h = ifelse(craki_stg2or3 == 1 & oli_craki_in12h == 1, 1, 0),
      oli_craki_2or3_in24h = ifelse(craki_stg2or3 == 1 & oli_craki_in24h == 1, 1, 0),
      oli_craki_2or3_in48h = ifelse(craki_stg2or3 == 1 & oli_craki_in48h == 1, 1, 0)
    )
  
  colnames(creatinine)[1] = "Pt_Study_no_crch"
  colnames(oliguria  )[1] = "Pt_Study_no_olig"
  
  both <- suppressMessages(full_join(creatinine, oliguria))
  
  Total_no_cr_epis = sum(screening_data$Total_no_cr_epis, na.rm = TRUE)
  if (length(unique(c(
    nrow(creatinine), length(which(!is.na(both$Pt_Study_no_crch))), Total_no_cr_epis))) != 1) 
  {
    stop(paste0(
      "Number of creatinine episodes is not consistent. ", 
      "Merged xlsx creatinine data: "   , nrow(creatinine), ", ",
      "Merged creatinine and oliguria: ", length(which(!is.na(both$Pt_Study_no_crch))), ", ",
      "Merged screening log number: "   , Total_no_cr_epis))
  }
  Total_no_olig_epis = sum(screening_data$Total_no_olig_epis, na.rm = TRUE)
  if (length(unique(c(
    nrow(oliguria), length(which(!is.na(both$Pt_Study_no_olig))), Total_no_olig_epis))) != 1) 
  {
    stop(paste0(
      "Number of oliguria episodes is not consistent. ", 
      "Merged xlsx oliguria data: "     , nrow(oliguria), ", ",
      "Merged creatinine and oliguria: ", length(which(!is.na(both$Pt_Study_no_olig))), ", ",
      "Merged screening log number: "   , Total_no_olig_epis))
  }
  
  # nrow(filter(screening_data, is.na(Total_no_cr_epis) & is.na(Total_no_olig_epis)))
  # nrow(filter(screening_data, !is.na(Total_no_cr_epis) & !is.na(Total_no_olig_epis)))
  # Appears that patients with both olig and cr_ch have duplicate records?
  
  # temp = inner_join(screening_data, both)
  # temp_full = full_join(screening_data, both)
  # length(which(!is.na(temp_full$Cr_epis_no)))
  # length(which(!is.na(temp_full$Olig_epis_no)))
  
  merged_data <- suppressMessages(full_join(screening_data, both))
  
  # Need to devise a robust check or check in flow chart of merged_data
  
  return(merged_data)
}

#--------------------------- Screening Log Flowchart ---------------------------

# TBC

#------------------------------ Merge APACHE Data ------------------------------

