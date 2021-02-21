# ---- Merge Screening and Obs Data ----
create_admission_data <- function(screen_log, data_set) {
  # Pivot longer to expand L and LTs into separate rows
  screening_data <- screen_log %>%
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
      Epis_olig = if_else(grepl("L[0-9]", Pt_Study_no), "Y", NA_character_)
    ) %>%
    select(`UR number`:Pt_Study_no, Dates_screened, Event, starts_with("Epis_"))

  stopifnot(all.equal(
    grep("L[0-9]", screening_data$Pt_Study_no, value = TRUE),
    as.vector(na.omit(screen_log$Pt_Study_no_olig))
  ))
  stopifnot(all.equal(
    grep("LT[0-9]", screening_data$Pt_Study_no, value = TRUE),
    as.vector(na.omit(screen_log$Pt_Study_no_crch))
  ))
  stopifnot(all.equal(
    filter(screening_data, is.na(Pt_Study_no))$`UR number`,
    filter(screen_log, Event == "Neither")$`UR number`
  ))
  stopifnot(all.equal(
    sort(screening_data$`UR number`),
    sort(c(
      filter(screen_log, Event != "Both")$`UR number`,
      rep(filter(screen_log, Event == "Both")$`UR number`, 2)
    ))
  ))

  # Join in the data_set
  obs_data <- full_join(
    screening_data,
    data_set,
    by = c("Pt_Study_no", "Epis_cr_change", "Epis_olig")
  )
  stopifnot(all.equal(
    sort(grep("L[0-9]", obs_data$Pt_Study_no, value = TRUE)),
    sort(grep("L[0-9]", data_set$Pt_Study_no, value = TRUE))
  ))
  stopifnot(all.equal(
    sort(grep("LT[0-9]", obs_data$Pt_Study_no, value = TRUE)),
    sort(grep("LT[0-9]", data_set$Pt_Study_no, value = TRUE))
  ))
  stopifnot(all.equal(
    sort(filter(obs_data, Event == "Neither")$`UR number`),
    sort(filter(screening_data, Event == "Neither")$`UR number`)
  ))
  stopifnot(
    nrow(obs_data) == nrow(data_set) + nrow(filter(screen_log, Event == "Neither"))
  )

  return(obs_data)
}


# ---- Add outcomes from data_set ----

# Various outcomes here
# Check individual columns next...
# Sort columns by UR, PTSn, Admission, Event Type, etc


# ---- Clean Admission Data ----
tidy_admission_data <- function(obs_data) {
  raw_data <- obs_data %>%
    # Create Admission data
    mutate(AdmissionID = paste(`UR number`, Admission, sep = ".")) %>%
    group_by(`UR number`, Pt_Study_no) %>%
    mutate(
      Epis_cr_change_no = cumsum(Epis_cr_change == "Y"),
      Epis_olig_no = cumsum(Epis_olig == "Y")
    ) %>%
    rowwise() %>%
    mutate(Comment = paste(
      unique(na.omit(c(Comment_crch, Comment_olig, Comment_out, Comment))),
      collapse = ", "
    )) %>%
    select(-starts_with("Comment_")) %>%
    # Add vasopressor variable
    group_by(AdmissionID) %>%
    mutate(
      Vasopressor = case_when(
        `T-4_Norad` > 0 ~ 1,
        `T-4_Metaraminol` > 0 ~ 1,
        T0_Norad > 0 ~ 1,
        T0_Metaraminol > 0 ~ 1,
        grepl("Vasopressors", Mx_other) ~ 1,
        TRUE ~ NA_real_
      ),
      Vasopressor = max(Vasopressor, 0, na.rm = TRUE),
      Pt_Study_nos = paste(unique(na.omit(Pt_Study_no)), collapse = ", ")
    ) %>%
    ungroup() %>%
    # Add other co-morbidities
    mutate(
      Diabetes = grepl("T2DM|T1DM|IDDM|insulin", Comorbidities),
      AF = grepl("AF|pAF", Comorbidities),
      IHD = grepl("IHD|CABG|CAD|CAGS|NSTEMI", Comorbidities),
      HF = grepl("\\bHF\\b|hypertrophy|CCF|cardiomyopathy|heart failure|LVH", Comorbidities),
      HT = grepl("^(?=.*\\bHT\\b)(?!.*portal)(?!.*pulm)", Comorbidities, perl = TRUE),
      PVD = grepl("PVD|arteritis|pop bypass|id steno|stents", Comorbidities),
      Chronic_liver_disease = grepl(paste0(
        "chronic liver disease|portal HT|varice|ETOH|",
        "HCC|NASH|CLD|ESLD|awaiting OLTx|SMV|ascites|SBP|HCC|cirrho|",
        "Hepatosplenomegaly|Cirrho|hepatic encephalopathy"
      ), Comorbidities)
    ) %>%
    mutate_at(vars(Diabetes:Chronic_liver_disease), as.double) %>%
    # Add discharge information
    mutate(
      Dc_ICU_Alive = case_when(
        Dc_ICU_Alive == 1 ~ 1,
        DateTime_ICU_dc == DateTime_death ~ 0,
        DateTime_ICU_dc == DateTime_hosp_dc & grepl("Mortuary", Dc_destination) ~ 0,
        TRUE ~ Dc_ICU_Alive
      ),
      Dc_Hosp_Alive = case_when(
        Dc_Hosp_Alive == 1 ~ 1,
        grepl("Mortuary", Dc_destination) ~ 0,
        TRUE ~ Dc_Hosp_Alive
      ),
      LOS_ICU_days = as.numeric(as.duration(DateTime_ICU_dc - DateTime_ICU_admit), "days"),
      LOS_Hosp_days = as.numeric(as.duration(DateTime_hosp_dc - DateTime_hosp_admit), "days"),
    )

  tidy_data <- raw_data %>%
    # Clean names
    rename_with(~ gsub(" ", "_", .x, )) %>%
    select(
      # Patient Information
      UR_number,
      AdmissionID, Admission, Total_admissions,
      DateTime_ICU_admit, DateTime_ICU_dc, DateTime_hosp_admit, DateTime_hosp_dc,
      Dc_destination, Dc_ICU_Alive, Dc_Hosp_Alive,
      LOS_ICU_days, LOS_Hosp_days,
      Excl_criteria_ok, Event,
      # Episode Information
      Pt_Study_nos, Pt_Study_no,
      Incl_criteria_ok_crch, Incl_criteria_ok_olig,
      Epis_cr_change, Epis_olig,
      Epis_cr_change_no, Epis_olig_no, Epis_no,
      Total_no_cr_epis, Total_no_olig_epis,
      # Demographics
      Already_AKI, EOLC, ESKD, No_IDC, Kidney_transplant,
      Admit_weekend, Child, Age, Male, Wt, Wtmeasured, HOPC,
      Mecvenadm, APACHE_II, APACHE_III,
      # Admission
      Surgadmission, starts_with("PCs_"), starts_with("PCm_"),
      # Comorbidities
      Comorbidities, HT, Diabetes, AF, IHD, HF, PVD, Chronic_liver_disease,
      Vasopressor, RRT,
      # Obs Data
      ICU_readmit, DateTime_epis,
      starts_with("T-4"), starts_with("T0"),
      T_corresp_check,

      # Baseline_Cr, Baseline_Cr_DateTime, Base_Cr_UEC,
      # Outcomes

      # Comments
      Dates_screened, Comment
    )

  setdiff(colnames(raw_data), colnames(tidy_data))
}


# ---- clean-obs-binary-columns ----
epoc_aki <- obs_data %>%
  mutate_at(
    vars(
      contains("criteria"),
      "Epis_cr_change", "Epis_olig", "Already_AKI", "EOLC",
      "ESKD", "No_IDC", "Kidney_transplant", "Admit_weekend", "Child",
      "Rx_limited", "Rx_withdrawn"
    ),
    function(x) {
      case_when(
        x == "Y" | x == "y" | x == "1" ~ 1, # Should really change to a factor of the column name
        x == "N" | x == "n" | x == "0" ~ 0,
        is.na(x) ~ NA_real_,
        TRUE ~ NaN # TODO check if any NaN later
      )
    }
  ) %>%
  select(
    # PT INFO
    `UR number`,
    AdmissionID, Admission, Total_admissions,
    DateTime_hosp_admit:DateTime_hosp_dc, Dc_destination, LOS_ICU_days:LOS_Hosp_days,
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
    Vasopressor, Diabetes:Chronic_liver_disease,
    # DATA SET
    DateTime_epis:Cause_death,
    # COMMENTS
    Dates_screened,
    Comment
  ) %>%
  arrange(AdmissionID)

# glimpse(epoc_aki)
# setdiff(colnames(obs_data), colnames(epoc_aki))


# ---- admission-data-errors ----
# FIXME Add additional checks
epoc_aki_check <- epoc_aki %>%
  select(`UR number`:AdmissionID, Pt_Study_no:Total_no_olig_epis) %>%
  # Check incl / excl criteria
  group_by(AdmissionID)

any(is.nan(epoc_aki$Rx_withdrawn))
any(is.nan(epoc_aki$`Criteria for stage of AKI`)) # FIXME check original data

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

rm(epoc_aki_check)

# ---- admission-data ----
admission_data <- epoc_aki %>%
  select(
    -Pt_Study_no:-Total_no_olig_epis,
    -DateTime_epis:-T0_Metaraminol,
  ) %>%
  group_by(AdmissionID) %>%
  # TODO think of a more generic way to solve the duplicates problem
  mutate(
    Mx_diuretics = max(Mx_diuretics), # No rm.na
    Mx_IVF = max(Mx_IVF),
    Baseline_Cr = min(Baseline_Cr)
  ) %>%
  distinct() %>%
  top_n(1, if_else(is.na(Max_Cr_ICU), 0, Max_Cr_ICU)) %>% # TODO replace with slice_max() in the future
  ungroup()

check_merge_data(
  admission_data$`UR number`,
  screen_log$`UR number`,
  "Number of admissions is different!"
)

rm(check_merge_data)
