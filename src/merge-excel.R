# Merge Excel Files
# Alwin Wang 2019

# Read Excel Files
load_excel_files <- function() {
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
    time_series  = creat_furo_xlsx
  )
  return(xlsx_data)
}

xlsx_data <- suppressMessages(load_excel_files())
# str(xlsx_data)

# Convert Excel Dates
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

xlsx_data <- convert_excel(xlsx_data)

# Data collection errors
data_collection_errors <- function(xlsx_data) {
  errors_logi = 
    (xlsx_data$creatinine$screen_log$`UR number`      != xlsx_data$oliguria$screen_log$`UR number`     ) |
    (xlsx_data$creatinine$screen_log$Dates_screened   != xlsx_data$oliguria$screen_log$Dates_screened  ) |
    (xlsx_data$creatinine$screen_log$Excl_criteria_ok != xlsx_data$oliguria$screen_log$Excl_criteria_ok)
  creatinine_errors = xlsx_data$creatinine$screen_log[errors_logi, ]
  oliguria_errors   = xlsx_data$oliguria  $screen_log[errors_logi, ]
  
  xlsx_data$excluded_UR_numbers  = creatinine_errors$`UR number`
  xlsx_data$excluded_Pt_Study_no = discard(c(oliguria_errors$Pt_Study_no, creatinine_errors$Pt_Study_no), is.na)
  
  cat("\nCreatinine xlsx sheet\n")
  print(creatinine_errors[, c(13, 2:4)])
  cat("\nOliguria xlsx sheet\n"  )
  print(oliguria_errors  [, c(13, 2:4)])
  
  cat(paste("\n",
    # "Discarded UR Numbers:       ", paste(xlsx_data$excluded_UR_numbers , collapse = ", "), "\n",
    "Discarded Pt Study Numbers: ", paste(xlsx_data$excluded_Pt_Study_no, collapse = ", "), "\n"
  ))
  
  xlsx_data$creatinine$screen_log[errors_logi, "Dates_screened"] = 
    xlsx_data$oliguria$screen_log[errors_logi, "Dates_screened"]
  xlsx_data$creatinine$screen_log[errors_logi, "Excl_criteria_ok"] = "N"
  xlsx_data$creatinine$screen_log[errors_logi, "Already_AKI"]     = "Y"
  xlsx_data$oliguria  $screen_log[errors_logi, "Excl_criteria_ok"] = "N"
  xlsx_data$oliguria  $screen_log[errors_logi, "Already_AKI"]     = "Y"
  
  return(xlsx_data)
}

xlsx_data = data_collection_errors(xlsx_data)

# Merge screening logs
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
    suffix = c("_cre", "_oli")
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

analysis_data <- merge_xlsx_screening(xlsx_data)

print("flowchart here")

list_analysis_data <- function(analysis_data) {
  analysis_data <- split(analysis_data, analysis_data$`UR number`)
}

# merge data sets

dttm_as_posixct <- function(date, time) {
  if (is.na(date) | is.na(time)) return(NA)
  else return(as.POSIXct(paste(date, format(time, format = "%H:%M:%S"))))
}


merge_data_set_demo_outcomes <- function(data,
                                         excluded_Pt_Study_no,
                                         data_name = deparse(substitute(data)))
  
  {
  combined_data <-data$data_set %>% 
    fill(Pt_Study_no) %>% 
    full_join(., data$demographic, by = "Pt_Study_no") %>% 
    full_join(., data$outcomes   , by = "Pt_Study_no")
  if (nrow(combined_data) != nrow(data$data_set)) {
    stop(paste0(
      "Inconsistent number of rows after merging data_set with demographics and outcomes",
      "Data: "         , data_name          , ", ",
      "Data set rows: ", nrow(data$data_set), ", ",
      "Merged rows: "  , nrow(combined_data), ", "
    ))
  }
  
  dt_cols <- data.frame(
    i = grep("^date|date$", colnames(combined_data), ignore.case = TRUE),
    d = grep("^date|date$", colnames(combined_data), ignore.case = TRUE, value = TRUE)
  )
  tm_cols <- data.frame(
    i = grep("^time|time$", colnames(combined_data), ignore.case = TRUE),
    t = grep("^time|time$", colnames(combined_data), ignore.case = TRUE, value = TRUE)
  )
  
  combined_data <- combined_data %>% 
    select(-last_col()) %>% 
    filter(!(Pt_Study_no %in% excluded_Pt_Study_no))
  
  return(combined_data)
}

merge_data_set_demo_outcomes(xlsx_data$creatinine, xlsx_data$excluded_Pt_Study_no)
