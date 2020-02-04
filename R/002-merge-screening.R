





# Data collection errors ----
data_collection_errors <- function(xlsx_data) {
  errors_logi =
    (
      xlsx_data$creatinine$screen_log$`UR number`      != xlsx_data$oliguria$screen_log$`UR number`
    ) |
    (
      xlsx_data$creatinine$screen_log$Dates_screened   != xlsx_data$oliguria$screen_log$Dates_screened
    ) |
    (
      xlsx_data$creatinine$screen_log$Excl_criteria_ok != xlsx_data$oliguria$screen_log$Excl_criteria_ok
    )
  creatinine_errors = xlsx_data$creatinine$screen_log[errors_logi, ]
  oliguria_errors   = xlsx_data$oliguria$screen_log[errors_logi, ]
  
  xlsx_data$excluded_UR_numbers  = creatinine_errors$`UR number`
  xlsx_data$excluded_Pt_Study_no = discard(c(
    oliguria_errors$Pt_Study_no,
    creatinine_errors$Pt_Study_no
  ),
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

# Merge screening logs -----
merge_xlsx_screening <- function(xlsx_data) {
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
  
  if (anyNA(screening_data$`UR number`)) {
    stop("NA in UR number of merged Excel sheets")
  }
  
  creatinine_n_obs = nrow(xlsx_data$creatinine$screen_log)
  oliguria_n_obs   = nrow(xlsx_data$oliguria$screen_log)
  screening_n_obs  = nrow(screening_data)
  if (length(unique(c(
    creatinine_n_obs, oliguria_n_obs, screening_n_obs
  ))) != 1) {
    stop(
      paste0(
        "Inconsistent number of n_obs after UR numbers discarded. ",
        "Creatinine: ",
        creatinine_n_obs,
        ", ",
        "Oliguria: "  ,
        oliguria_n_obs  ,
        ", ",
        "Merged: "    ,
        screening_n_obs  ,
        ", "
      )
    )
  }
  
  logi_colnames <- colnames(screening_data)[!grepl(
    "UR number|Dates_screened|Pt_Study_no|Total_no_|Comment",
    colnames(screening_data)
  )]
  screening_data <- screening_data %>%
    mutate_at(logi_colnames, function(x)
      if_else(x == "N", FALSE, TRUE))
  
  return(screening_data)
}

# Flow chart here
