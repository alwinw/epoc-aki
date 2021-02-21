# ---- Merge Screening and Obs Data ----
create_obs_data <- function(screen_log, data_set) {
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


# ---- Clean Observation Data ----
tidy_obs_data <- function(obs_data) {
  raw_data <- obs_data %>%
    # Create Observation data
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
      # All others
      Baseline_Cr:Cause_death,
      # Comments
      Dates_screened, Comment
    )

  stopifnot(
    "Missing column names" =
      length(setdiff(
        gsub(" ", "_", colnames(raw_data)),
        c(colnames(tidy_data), "UR number")
      )) == 0
  )

  factored_data <- tidy_data %>%
    mutate(across(
      c(
        Dc_ICU_Alive, Dc_Hosp_Alive, Excl_criteria_ok,
        Incl_criteria_ok_crch, Incl_criteria_ok_olig,
        Epis_cr_change, Epis_olig,
        Already_AKI, EOLC, ESKD, No_IDC, Kidney_transplant, Admit_weekend,
        Child, Male, Wtmeasured, Mecvenadm,
        Surgadmission, starts_with("PCs_"), starts_with("PCm_"),
        HT, Diabetes, AF, IHD, HF, PVD, Chronic_liver_disease, Vasopressor,
        RRT, ICU_readmit,
        AKI_ICU, AKI_Dx_Cr_1.5_times, AKI_Dx_oliguria, CrdxAKIUEC,
        Cr_defined_AKI, Mx_diuretics, Mx_IVF, Mx_other, ICU_dc_RRT,
        AKI_ward_48h
      ),
      function(x) {
        b <- case_when(
          tolower(x) == "y" | tolower(x) == "1" ~ 1,
          tolower(x) == "n" | tolower(x) == "0" ~ 0,
          is.na(x) ~ NA_real_,
          TRUE ~ NaN
        )
        factor(b, c(0, 1), paste0(c("Not_", ""), cur_column()), ordered = TRUE)
      }
    ))

  stopifnot(
    "Unusual entry data found" =
      !any(is.nan(as.matrix(factored_data)))
  )
  stopifnot(!any(is.na(factored_data$UR_number)))

  return(factored_data)
}

if (FALSE) {
  # factored_data %>%
  #   # select(
  #   #   UR_number, AdmissionID, Pt_Study_nos,
  #   #   Max_Cr_ICU, Highest_Cr_UEC, Max_Cr_DateTime, Baseline_Cr,
  #   #   Mx_diuretics, Mx_IVF
  #   # ) %>%
  #   distinct() %>%
  #   group_by(AdmissionID) %>%
  #   mutate(duplicates = n()) %>%
  #   filter(duplicates > 1) %>%
  #   arrange(desc(duplicates)) %>%
  #   ungroup() %>%
  #   select(-UR_number, -AdmissionID) %>%
  #   kable(., caption = "Errors between L and LT obs data", booktabs = TRUE)
  # # Reason: Cr and Olig epis happen at different times
  # # If there is a large enough difference, then the obs data will be different


  # TODO think about a better solution
  admin_data <- factored_data %>%
    # select(
    #   -Pt_Study_no:-Total_no_olig_epis,
    #   -DateTime_epis:-T0_Metaraminol,
    # ) %>%
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
  # What about Admission ID?

  stopifnot(all.equal(
    sort(unique(obs_data$`UR number`)),
    sort(unique(admin_data$UR_number))
  ))

  return(admin_data)
}
