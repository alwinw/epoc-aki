#========================= Build Individual Dataframes =========================
#                                Alwin Wang 2019

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
#' Note there will also be duplicate UR numbers in here due to readmissions
#' 
#' @param creatinine_screen_log Creatinine screening log
#' @param oliguria_screen_log Oliguria screening log
#' 
#' @return A dataframe of screening logs combined from creatinine and oliguria
#' 
#' @examples 
#' screening_data <- merge_xlsx_screening(xlsx_data$creatinine$screen_log,
#'                                        xlsx_data$oliguria$screen_log)

merge_xlsx_screening <- function(creatinine_screen_log,
                                 oliguria_screen_log) {
  # Merge all columns except incl criteria, pt study number and comment
  merge_columns = setdiff(intersect(
    colnames(creatinine_screen_log),
    colnames(oliguria_screen_log)
  ),
  c("Incl_criteria_ok", "Pt_Study_no", "Comment"))
  
  screening_data = full_join(
    creatinine_screen_log,
    oliguria_screen_log,
    by     = merge_columns,
    suffix = c("_crch", "_olig")
  )
  
  # Check output
  if (anyNA(screening_data$`UR number`)) {
    stop("NA in UR number of merged Excel sheets")
  }
  creatinine_n_obs = nrow(creatinine_screen_log)
  oliguria_n_obs   = nrow(oliguria_screen_log)
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
  
  # Add excluded Pt Study No
  # Could be smarter and have LX1, LX2 in chronological order
  screening_data <- screening_data %>% 
    mutate(
      Pt_Study_no_excl = is.na(Pt_Study_no_crch) & is.na(Pt_Study_no_olig)) %>% 
    arrange(-Pt_Study_no_excl) %>% 
    mutate(
      Pt_Study_no_excl = ifelse(Pt_Study_no_excl, paste0("LX", 1:n()), NA)
    ) %>% 
    arrange(`UR number`)
  
  return(screening_data)
}

#----------------------------- Patient Study Number ----------------------------

#' Generate Pt Study Number
#' 
#' Generates a dataframe of patient study number, UR number and Dates Screened
#' information
#' 
#' @param screening_data The screening log dataframe
#' 
#' @return Dataframe of reduced screening_data with no 
#' 
#' @examples 
#' pt_study_no <- generate_pt_study_no(screening_data)

generate_pt_study_no <- function(screening_data) {
  pt_study_no <- screening_data %>% 
    select(`UR number`, Pt_Study_no_crch, Pt_Study_no_olig, 
           Pt_Study_no_excl, Dates_screened)
  
  pt_study_no <- distinct(pt_study_no)
  
  return(pt_study_no)
}

#-------------------------- Merge Patient Information --------------------------

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

#' Convert columns into DateTime
#' 
#' Finds and converts the relevant columns into DateTime
#' 
#' @param dataframe A input dataframe
#' 
#' @return A dataframe with converted columns
#' 
#' @examples 
#' date_time_cols(pt_info)

date_time_cols <- function(dataframe) {
  dttm_col = inner_join(
    find_cols("date", "DateTime", colnames(dataframe)),
    find_cols("time", "DateTime", colnames(dataframe)), 
    by = "match") %>% 
    select(date, time, match)
  # Instead, consider using mutate_at to convert date and time columns and paste
  # then into the date columns. Then use rename at to conver to a datetime name
  # and then remove the time columns.
  
  modified_df <- dataframe %>% 
    select(-last_col()) %>% 
    # filter(!(Pt_Study_no %in% excluded_Pt_Study_no)) %>% 
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
  
  return(modified_df)
}

#' Merge patient information
#' 
#' Merge patient demographics from various sources
#' 
#' @param creatinine_demographic Creatinine demographics
#' @param oliguria_demographic Oliguria demographics
#' @param pt_study_no Patient study number IDs
#' 
#' @return A dataframe of patient information
#' 
#' @example 
#' pt_info = merge_xlsx_pt_info(xlsx_data$creatinine$demographic,
#'                              xlsx_data$oliguria$demographic,
#'                              pt_study_no)


merge_xlsx_pt_info <- function(creatinine_demographic,
                               oliguria_demographic,
                               pt_study_no) {
  pt_info_crch <- full_join(
    pt_study_no, 
    rename(creatinine_demographic, Pt_Study_no_crch = Pt_Study_no),
    by = "Pt_Study_no_crch")
  pt_info_olig <- full_join(
    pt_study_no,
    rename(oliguria_demographic, Pt_Study_no_olig = Pt_Study_no),
    by = "Pt_Study_no_olig"
  )
  pt_info_full <- full_join(
    pt_info_crch,
    pt_info_olig,
    by = colnames(pt_info_crch)
  ) %>% 
    group_by(temp_id = interaction(`UR number`, Dates_screened)) %>% 
    arrange(`UR number`) %>% 
    mutate(n = n()) %>% 
    filter(n == 1 | (n > 1 & !is.na(Age))) %>%  # Remove missing data
    mutate(n = n())
  
  # cat(paste("Number of pt_info rows is", nrow(pt_info_full),
  #           "whereas number of pt_study_no rows is", nrow(pt_study_no), "\n"))
  
  demographics_errors <- pt_info_full %>% 
    filter(n > 1) %>%  # Errors
    select(-APACHE_II, -APACHE_III, -Dc_destination) %>%   # Common errors
    select(-Time_ICU_dc) %>%  # Another error
    unique(.) %>% 
    mutate(n = n()) %>% 
    filter(n > 1)
  
  if (nrow(demographics_errors) > 1) {
    stop(paste0(
      "Additional demographics errors found. Duplicate rows: ",
      nrow(demographics_errors)
      ))
    print(demographics_errors)
  }
  
  pt_info <- pt_info_full %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(-temp_id, -n) %>% 
    arrange(`UR number`)
  
  pt_info <- date_time_cols(pt_info)
  
  pt_info <- pt_info %>% 
    mutate(
      ICU_LOS  = as.duration(DateTime_ICU_admit  %--% DateTime_ICU_dc ) / ddays(1),
      Hosp_LOS = as.duration(DateTime_hosp_admit %--% DateTime_hosp_dc) / ddays(1),
    )
  
  return(pt_info)
}

#--------------------------- Merge Patient Outcomes ----------------------------

merge_xlsx_outcomes <- function(creatinine_outcomes, 
                                oliguria_outcomes, 
                                pt_study_no) {
  # First section of code should be moved into a helper function for pt_info
  # and for outcomes
  outcomes_crch <- full_join(
    pt_study_no, 
    rename(creatinine_outcomes, Pt_Study_no_crch = Pt_Study_no),
    by = "Pt_Study_no_crch")
  outcomes_olig <- full_join(
    pt_study_no,
    rename(oliguria_outcomes, Pt_Study_no_olig = Pt_Study_no),
    by = "Pt_Study_no_olig"
  )
  outcomes_full <- full_join(
    outcomes_crch,
    outcomes_olig,
    by = intersect(names(outcomes_crch), names(outcomes_olig))
  ) %>% 
    group_by(temp_id = interaction(`UR number`, Dates_screened)) %>% 
    arrange(`UR number`) %>% 
    mutate(n = n())
}
