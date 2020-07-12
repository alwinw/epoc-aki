# ---- check-merge-data-fun ----
check_merge_data <- function(vector1, vector2, error_msg) {
  if (!setequal(vector1, vector2) | (length(vector1) != length(vector2))) {
    stop(paste("Error", error_msg,
               "Actual:", length(vector1), "Expected:", length(vector2),
               "Difference:", paste(setdiff(vector1, vector2), collapse = ", ")
    ))
  }
  return(invisible(NULL))
}


# ---- merge-obs-data ----
# Pivot longer to expand L and LTs into separate rows
screening_data <- screening_log %>%
  pivot_longer(
    starts_with("Pt_Study_no"),
    names_to = "Pt_Study_no_type",
    values_to = "Pt_Study_no",
  ) %>%
  select(-Pt_Study_no_type) %>%
  distinct() %>%
  mutate(
    duplicate = is.na(Pt_Study_no) & (Event == "Olig only" | Event == "Cr change only")
  ) %>%
  filter(!duplicate) %>%
  mutate(
    Epis_cr_change = if_else(grepl("LT[0-9]", Pt_Study_no), "Y", NA_character_),
    Epis_olig      = if_else(grepl("L[0-9]",  Pt_Study_no), "Y", NA_character_)
  ) %>%
  select(`UR number`:Pt_Study_no, Dates_screened, Event, starts_with("Epis_"))

check_merge_data(
  grep("L[0-9]", screening_data$Pt_Study_no, value = TRUE),
  as.vector(na.omit(screening_log$Pt_Study_no_olig)),
  "Number of olig events different!"
)
check_merge_data(
  grep("LT[0-9]", screening_data$Pt_Study_no, value = TRUE),
  as.vector(na.omit(screening_log$Pt_Study_no_crch)),
  "Number of cr change events different!"
)
check_merge_data(
  filter(screening_data, is.na(Pt_Study_no))$`UR number`,
  filter(screening_log, Event == "Neither")$`UR number`,
  "Number of neither olig or cr change events different!"
)
check_merge_data(
  screening_data$`UR number`,
  c(filter(screening_log, Event != "Both")$`UR number`,
    rep(filter(screening_log, Event == "Both")$`UR number`, 2)),
  "Total number of events has changed!"
)

# Join in the data_set
obs_data <- full_join(
  screening_data,
  data_set,
  by = c("Pt_Study_no", "Epis_cr_change", "Epis_olig")
)

check_merge_data(
  grep("L[0-9]", obs_data$Pt_Study_no, value = TRUE),
  grep("L[0-9]", data_set$Pt_Study_no, value = TRUE),
  "Number of olig events different!"
)
check_merge_data(
  grep("LT[0-9]", obs_data$Pt_Study_no, value = TRUE),
  grep("LT[0-9]", data_set$Pt_Study_no, value = TRUE),
  "Number of cr change events different!"
)
check_merge_data(
  filter(obs_data, Event == "Neither")$`UR number`,
  filter(screening_data, Event == "Neither")$`UR number`,
  "Number of neither olig or cr change events different!"
)

if (nrow(obs_data) != nrow(data_set) + nrow(filter(screening_log, Event == "Neither"))){
  stop("Number of total events has changed!")
}

# ---- add-outcomes ----

# Various outcomes here
# Check individual columns next...
# Sort columns by UR, PTSn, Admission, Event Type, etc


# ---- clean-binary-columns ----
epoc_aki <- obs_data %>%
  mutate(AdmissionID = paste(`UR number`, Admission, sep = ".")) %>%
  group_by(`UR number`) %>%
  group_by(Pt_Study_no) %>%
  mutate(
    Epis_cr_change_no = cumsum(Epis_cr_change == "Y"),
    Epis_olig_no      = cumsum(Epis_olig      == "Y")
  ) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(Comment = paste(unique(na.omit(c(Comment_crch, Comment_olig, Comment_out, Comment))), collapse = ", ")) %>%
  ungroup() %>%
  group_by(AdmissionID) %>%
  mutate(
    Vasopressor = case_when(
      `T-4_Norad` > 0       ~ 1,
      `T-4_Metaraminol` > 0 ~ 1,
      T0_Norad > 0          ~ 1,
      T0_Metaraminol > 0    ~ 1,
      grepl("Vasopressors", Mx_other) ~ 1,
      TRUE ~ NA_real_
    ),
    Vasopressor = max(Vasopressor, 0, na.rm =  TRUE),
    Pt_Study_nos = paste(unique(na.omit(Pt_Study_no)), collapse = ", ")
  ) %>%
  ungroup() %>%
  mutate_at(
    vars(
      contains("criteria"),
      "Epis_cr_change", "Epis_olig", "Already_AKI", "EOLC",
      "ESKD", "No_IDC", "Kidney_transplant", "Admit_weekend", "Child",
      "Rx_limited", "Rx_withdrawn"
    ),
    function(x) case_when(
      x == "Y" | x == "y" | x == "1" ~ 1,
      x == "N" | x == "n" | x == "0" ~ 0,
      is.na(x) ~ NA_real_,
      TRUE     ~ NaN
    )
  ) %>%
  select(
    # PT INFO
    `UR number`,
    AdmissionID, Admission, Total_admissions,
    DateTime_hosp_admit:DateTime_ICU_admit, Date_ICU_dc:Dc_destination,
    Excl_criteria_ok, Event,
    # EPIS
    Pt_Study_nos, Pt_Study_no,
    Incl_criteria_ok_crch, Incl_criteria_ok_olig,
    Epis_cr_change, Epis_olig,
    Epis_cr_change_no, Epis_olig_no, Epis_no,
    Total_no_cr_epis, Total_no_olig_epis,
    # SCREENING LOG
    APACHE_II:APACHE_III,
    Already_AKI:Mecvenadm,
    Vasopressor,
    # DATA SET
    DateTime_epis:Cause_death,
    # COMMENTS
    Dates_screened,
    Comment
  ) %>%
  arrange(AdmissionID)

glimpse(epoc_aki)
# setdiff(colnames(obs_data), colnames(epoc_aki))


# ---- admission-data-errors ----
# TODO Add additional checks
epoc_aki_check <- epoc_aki %>%
  select(`UR number`:AdmissionID, Pt_Study_no:Total_no_olig_epis) %>%
  # Check incl / excl criteria
  group_by(AdmissionID)

any(is.nan(epoc_aki$Rx_withdrawn))
any(is.nan(epoc_aki$`Criteria for stage of AKI`))  # FIXME check original data

epoc_aki_check
any(is.na(epoc_aki_check$`UR number`))


epoc_aki %>%
  select(
    `UR number`, AdmissionID, Pt_Study_nos,
    Max_Cr_ICU, `Highest Cr UEC`, Max_Cr_DateTime, Baseline_Cr,
    Mx_diuretics, Mx_IVF
  ) %>%
  distinct() %>%
  group_by(AdmissionID) %>%
  mutate(duplicates = n()) %>%
  filter(duplicates > 1) %>%
  arrange(desc(duplicates)) %>%
  ungroup() %>%
  select(-`UR number`, -AdmissionID) %>%
  kable(., caption = "Errors between L and LT obs data", booktabs = TRUE)
# Reason: Cr and Olig epis happen at different times
# If there is a large enough difference, then the obs data will be different

# ---- admission-data ----
admission_data <- epoc_aki %>%
  select(
    -Pt_Study_no:-Total_no_olig_epis,
    -DateTime_epis:-T0_Metaraminol,
  ) %>%
  group_by(AdmissionID) %>%
  # TODO think of a more generic way to solve the duplicates problem
  mutate(
    Mx_diuretics = max(Mx_diuretics),  # No rm.na
    Mx_IVF = max(Mx_IVF),
    Baseline_Cr = min(Baseline_Cr)
  ) %>%
  distinct() %>%
  top_n(1, if_else(is.na(Max_Cr_ICU), 0, Max_Cr_ICU)) %>%   # TODO replace with slice_max() in the future
  ungroup()

check_merge_data(
  admission_data$`UR number`,
  screening_log$`UR number`,
  "Number of admissions is different!"
)
