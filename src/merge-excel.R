# Merge Excel Files
# Alwin Wang 2019

# To do: search for "read_excel" in Paper_analysis.R

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

analysis_data <- merge_xlsx_screening(xlsx_data)

print("flowchart here")

# merge data sets

dttm_cols <- function(text, colnames) {
  cols <- data.frame(
    i = grep(paste0("^", text, "|", text, "$"), colnames, ignore.case = TRUE),
    j = grep(paste0("^", text, "|", text, "$"), colnames, ignore.case = TRUE, value = TRUE),
    stringsAsFactors = FALSE
  ) %>% 
    mutate(k = gsub(text, "DateTime", j, ignore.case = TRUE))
  colnames(cols) <- c(paste0(text, "_i"), paste0(text), "match")
  
  return(cols)
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
  
  dttm_col = inner_join(
    dttm_cols("date", colnames(combined_data)),
    dttm_cols("time", colnames(combined_data)), by = "match") %>% 
    select(date, time, match)
  # Instead, consider using mutate_at to convert date and time columns and paste
  # then into the date columns. Then use rename at to conver to a datetime name
  # and then remove the time columns.
  
  combined_data <- combined_data %>% 
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
  
  return(combined_data)
}

merge_xlsx_creatinine_oliguria <- function(analysis_data, xlsx_data) {
  creatinine = merge_data_set_demo_outcomes(xlsx_data$creatinine, xlsx_data$excluded_Pt_Study_no)
  oliguria   = merge_data_set_demo_outcomes(xlsx_data$oliguria  , xlsx_data$excluded_Pt_Study_no)
  
  colnames(creatinine)[1] = "Pt_Study_no_crch"
  colnames(oliguria  )[1] = "Pt_Study_no_olig"
  
  both <- suppressMessages(full_join(creatinine, oliguria))
  
  Total_no_cr_epis = sum(analysis_data$Total_no_cr_epis, na.rm = TRUE)
  if (length(unique(c(
    nrow(creatinine), length(which(!is.na(both$Pt_Study_no_crch))), Total_no_cr_epis))) != 1) 
    {
    stop(paste0(
      "Number of creatinine episodes is not consistent", 
      "Merged xlsx creatinine data: "   , nrow(creatinine), ", ",
      "Merged creatinine and oliguria: ", length(which(!is.na(both$Pt_Study_no_crch))), ", ",
      "Merged screening log number: "   , Total_no_cr_epis))
  }
  Total_no_olig_epis = sum(analysis_data$Total_no_olig_epis, na.rm = TRUE)
  if (length(unique(c(
    nrow(oliguria), length(which(!is.na(both$Pt_Study_no_olig))), Total_no_olig_epis))) != 1) 
    {
    stop(paste0(
      "Number of oliguria episodes is not consistent", 
      "Merged xlsx oliguria data: "     , nrow(oliguria), ", ",
      "Merged creatinine and oliguria: ", length(which(!is.na(both$Pt_Study_no_olig))), ", ",
      "Merged screening log number: "   , Total_no_olig_epis))
  }
  
  both <- both %>% 
    mutate(
      ICU_LOS           = as.duration(DateTime_ICU_admit  %--% DateTime_ICU_dc   ) / ddays (1),
      Hosp_LOS          = as.duration(DateTime_hosp_admit %--% DateTime_hosp_dc  ) / ddays (1),
      
      ICUadmtoAKIDx     = as.duration(DateTime_ICU_admit  %--% DateTime_AKI_Dx   ) / dhours(1),
      Time_betw_ABG     = as.duration(DateTime_pre_ABG    %--% DateTime_post_ABG ) / dhours(1),
      Time_betw_cr_AKI  = as.duration(DateTime_post_ABG   %--% DateTime_AKI_Dx   ) / dhours(1),
      Time_betw_oli_AKI = as.duration(DateTime_olig_epis  %--% DateTime_AKI_Dx   ) / dhours(1),
      Time_betw_ICU_cr  = as.duration(DateTime_ICU_admit  %--% DateTime_post_ABG ) / dhours(1),
      Time_betw_ICU_oli = as.duration(DateTime_ICU_admit  %--% DateTime_olig_epis) / dhours(1),
      
      crchange          = (T0_ABG_Cr - `T-4_ABG_Cr`),
      delta_cr          =  crchange  / Time_betw_ABG,
      percent_delta_cr  = (crchange  / `T-4_ABG_Cr`) * 100 / Time_betw_ABG,
      T0_UO_wtadjusted  = (T0_UO / 4)/Wt,
      
      akistagesv2       = ifelse(AKI_ICU == 0, 0, AKI_stage),
      aki2or3           = ifelse(akistagesv2 >= 2, 1, 0),
      
      craki12h          = ifelse(Time_betw_cr_AKI > 12 | AKI_ICU == 0, 0, 1),
      craki24h          = ifelse(Time_betw_cr_AKI > 24 | AKI_ICU == 0, 0, 1),
      craki48h          = ifelse(Time_betw_cr_AKI > 48 | AKI_ICU == 0, 0, 1),
      craki2or3in12h    = ifelse(craki2or3 == 1 & craki12h ==1, 1, 0),
      craki2or3in24h    = ifelse(craki2or3 == 1 & craki24h ==1, 1, 0),
      craki2or3in48h    = ifelse(craki2or3 == 1 & craki48h ==1, 1, 0),
      craki2or3         = ifelse(Cr_defined_AKI_stage >=2, 1, 0),
      craki12h          = ifelse(Time_betw_cr_AKI > 12 | Cr_defined_AKI == 0, 0, 1),
      craki24h          = ifelse(Time_betw_cr_AKI > 24 | Cr_defined_AKI == 0, 0, 1),
      craki48h          = ifelse(Time_betw_cr_AKI > 48 | Cr_defined_AKI == 0, 0, 1),
      craki2or3in12h    = ifelse(craki2or3 == 1 & craki12h ==1, 1, 0),
      craki2or3in24h    = ifelse(craki2or3 == 1 & craki24h ==1, 1, 0),
      craki2or3in48h    = ifelse(craki2or3 == 1 & craki48h ==1, 1, 0),
      
      oliaki12h         = ifelse(Time_betw_oli_ep_AKI > 12 | AKI_ICU == 0, 0, 1),
      oliaki24h         = ifelse(Time_betw_oli_ep_AKI > 24 | AKI_ICU == 0, 0, 1),
      oliaki48h         = ifelse(Time_betw_oli_ep_AKI > 48 | AKI_ICU == 0, 0, 1),
      oliaki2or3in12h   = ifelse(aki2or3 == 1 & aki12h ==1, 1, 0),
      oliaki2or3in24h   = ifelse(aki2or3 == 1 & aki24h ==1, 1, 0),
      oliaki2or3in48h   = ifelse(aki2or3 == 1 & aki48h ==1, 1, 0),
      oliaki2or3in12h   = ifelse(oliaki2or3 == 1 & oliaki12h ==1, 1, 0),
      oliaki2or3in24h   = ifelse(oliaki2or3 == 1 & oliaki24h ==1, 1, 0),
      oliaki2or3in48h   = ifelse(oliaki2or3 == 1 & oliaki48h ==1, 1, 0)
    )
  
  analysis_data <- suppressMessages(full_join(analysis_data, both))
  
  return(analysis_data)
}

analysis_data = merge_xlsx_creatinine_oliguria(analysis_data, xlsx_data)

# GENERATE FLOW CHART AGAIN AND COMPARE THE PAIR
