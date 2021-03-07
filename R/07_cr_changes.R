# ---- Combinations ----
generate_cr_ch <- function(UR_number_, DateTime_ICU_admit, DateTime_ICU_dc,
                           AKI_ICU, DateTime_AKI_Dx, cr_data) {
  cr_ts <- cr_data %>%
    ungroup() %>%
    filter(
      UR_number == UR_number_, # could use get() instead to avoid column vs global
      Pathology_Result_DTTM > DateTime_ICU_admit,
      Pathology_Result_DTTM < DateTime_ICU_dc
    ) %>%
    select(Pathology_Result_DTTM, Creatinine_level) %>%
    unique(.) # To remove any duplicate DTTM entries

  if (nrow(cr_ts) < 2) {
    return(data.frame(
      DateTime_Pathology_Result = as_datetime(NA_real_),
      del_t_ch = as.duration(NA_real_),
      del_t_aki = as.duration(NA_real_),
      del_cr = NA_real_,
      cr = NA_real_
    ))
  }
  # Consider filtering out ones post AKI here?

  combns <- combn(nrow(cr_ts), 2)
  Ti_1 <- cr_ts[combns[1, ], ]
  Ti <- cr_ts[combns[2, ], ]

  if (AKI_ICU == 0 | is.na(AKI_ICU)) {
    del_t_aki <- rep(as.duration(NA_real_), nrow(Ti))
  } else {
    # Start of AKI to end of Cr ch ep
    del_t_aki <- as.duration(DateTime_AKI_Dx - Ti$Pathology_Result_DTTM)
  }

  return(data.frame(
    DateTime_Pathology_Result = Ti$Pathology_Result_DTTM,
    del_t_ch = as.duration(Ti$Pathology_Result_DTTM - Ti_1$Pathology_Result_DTTM),
    del_t_aki = del_t_aki,
    del_cr = Ti$Creatinine_level - Ti_1$Creatinine_level,
    cr = Ti$Creatinine_level
  ))
}


# ---- Generate Cr Change ----
generate_cr_changes <- function(admission_data, cr_data) {
  cr_ch_ts_all <- admission_data %>%
    filter(
      Excl_criteria_ok == 1,
    ) %>%
    mutate(
      DateTime_ICU_dc = DateTime_ICU_dc + hours(23) + minutes(59) + seconds(59)
    ) %>%
    select(
      UR_number, AdmissionID, Admission,
      Pt_Study_nos, Event, Excl_criteria_ok,
      Age, Male, APACHE_II, APACHE_III, Mecvenadm,
      Baseline_Cr, PCs_cardio, Vasopressor, Diabetes, AF, IHD, HF,
      HT, PVD, Chronic_liver_disease,
      AKI_ICU, AKI_stage,
      DateTime_ICU_admit, DateTime_ICU_dc,
      Baseline_Cr, Baseline_Cr_DateTime, Max_Cr_ICU, Max_Cr_DateTime,
      DateTime_AKI_Dx, AKI_Dx_Cr_26.5, AKI_Dx_Cr_1.5_times, AKI_Dx_oliguria,
      Criteria_for_stage_of_AKI, Cr_defined_AKI
    ) %>%
    mutate(
      Olig_defined_AKI = if_else(AKI_Dx_oliguria == 1, 1, 0, 0)
    ) %>%
    rowwise() %>%
    do(data.frame(., generate_cr_ch(
      .$UR_number, .$DateTime_ICU_admit, .$DateTime_ICU_dc,
      .$AKI_ICU, .$DateTime_AKI_Dx,
      cr_data
    ))) %>%
    ungroup()


  neither_ts <- cr_ch_ts_all %>% # Neither from initial data study
    filter(is.na(AKI_ICU)) %>%
    filter(!is.na(cr)) %>% # Only one measurement in ICU
    group_by(AdmissionID) %>%
    mutate(
      # No need to check for olig definition of AKI, else would have had "oliguria episode"
      Baseline_Cr = min(cr),
      AKI_ICU = if_else(cr > Baseline_Cr * 1.5, 1, 0), # FIXME NEED TO PROGRAM IN THE RISE CASE TOO
      AKI_stage = case_when( # TODO Add other gradient definition too!
        cr > Baseline_Cr * 3 ~ 3,
        cr > Baseline_Cr * 2 ~ 2,
        cr > Baseline_Cr * 1.5 ~ 1,
        TRUE ~ 0
      ),
      Max_Cr_ICU = max(cr),
      Cr_defined_AKI = AKI_ICU, # Since these are all defined on AKI_ICU here
      Olig_defined_AKI = 0 # Since only Cr considered
    ) %>%
    arrange(AdmissionID, desc(cr)) %>%
    mutate(
      Max_Cr_DateTime = first(DateTime_Pathology_Result)
    ) %>%
    arrange(AdmissionID, desc(AKI_ICU), DateTime_Pathology_Result) %>%
    mutate(
      DateTime_AKI_Dx = if_else(first(AKI_ICU) == 1, first(DateTime_Pathology_Result), as_datetime(NA_real_))
    ) %>%
    arrange(AdmissionID, DateTime_Pathology_Result) %>%
    mutate(
      # Must be done AFTER arrange() and mutate()
      AKI_ICU = max(AKI_ICU, na.rm = TRUE),
      AKI_stage = max(AKI_stage, na.rm = TRUE),
      AKI_stage = if_else(AKI_stage == 0, NA_real_, AKI_stage),
      del_t_aki = if_else(AKI_ICU == 1, as.duration(DateTime_AKI_Dx - DateTime_Pathology_Result), as.duration(NA_real_))
    ) %>%
    ungroup()

  insufficient_cr <- cr_ch_ts_all %>%
    filter(is.na(AKI_ICU)) %>%
    filter(is.na(cr)) %>%
    mutate(Baseline_Cr = median(neither_ts$Baseline_Cr, na.rm = TRUE)) %>% ## FIXME
    mutate(AKI_ICU = 0, AKI_stage = 0, Cr_defined_AKI = 0, Olig_defined_AKI = 0)

  cr_ch_ts <- rbind(
    cr_ch_ts_all %>% filter(!is.na(AKI_ICU)), # No issues
    neither_ts, # Had to check for AKI
    insufficient_cr # Not enough cr changes
  ) %>%
    mutate(
      del_t_ch_hr = as.numeric(del_t_ch, "hours"),
      del_t_aki_hr = as.numeric(del_t_aki, "hours"),
      per_cr_change = del_cr / cr * 100
    ) %>%
    mutate(
      AKI_2or3 = if_else(AKI_stage >= 2, 1, 0, 0),
      Cr_defined_AKI_2or3 = if_else(Cr_defined_AKI == 1, AKI_2or3, 0, 0),
      Olig_defined_AKI_2or3 = if_else(Olig_defined_AKI == 1, AKI_2or3, 0, 0)
    ) %>%
    group_by(AdmissionID) %>%
    mutate( # TODO work out why this is required..
      Cr_defined_AKI = max(Cr_defined_AKI),
      Cr_defined_AKI_2or3 = max(Cr_defined_AKI_2or3),
      Olig_defined_AKI = max(Olig_defined_AKI),
      Olig_defined_AKI_2or3 = max(Olig_defined_AKI_2or3)
    ) %>%
    select(-del_t_ch, -del_t_aki)

  if (nrow(cr_ch_ts) != nrow(cr_ch_ts_all)) {
    stop("Differring row numbers")
  }
  # TODO Add more checks on number of rows, etc

  return(cr_ch_ts)
}


# ---- cr_changes_overview ----
if (FALSE) {
  cr_ch_ts %>%
    filter(abs(del_cr) < 50) %>%
    mutate(
      t_AKI = if_else(is.na(del_t_aki_hr) | del_t_aki_hr > 0, "Before", "After")
    ) %>%
    select(AdmissionID, AKI_ICU, DateTime_Pathology_Result, t_AKI) %>%
    # unique(.) %>%
    group_by(AKI_ICU, t_AKI) %>%
    summarise(
      cr_measurements = n_distinct(DateTime_Pathology_Result, na.rm = TRUE),
      cr_ch_epis = n(),
      .groups = "drop"
    ) %>%
    arrange(AKI_ICU, desc(t_AKI)) %>%
    adorn_totals() %>%
    kable(., caption = "All cr measurements (measurement errors included)")


  cr_ch_ts %>%
    filter(abs(del_cr) < 50) %>%
    mutate(
      t_AKI = if_else(is.na(del_t_aki_hr) | del_t_aki_hr > 0, "Before", "After")
    ) %>%
    select(AdmissionID, AKI_ICU, t_AKI) %>%
    group_by(AdmissionID) %>%
    unique(.) %>%
    group_by(AKI_ICU, t_AKI) %>%
    summarise(
      admissions = n(),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = t_AKI, values_from = admissions) %>%
    adorn_totals(c("col")) %>%
    kable(., caption = "Admission breakdown")
}

# ---- summary_plots ----
# ggplot(cr_ch_ts, aes(x = del_t_ch_hr)) +
#   geom_histogram(bins = 100, fill = "cyan", colour = "blue") +
#   xlim(0, 48)
# # Add another plot based on admissions?
#
# ggplot(cr_ch_ts, aes(x = del_cr)) +
#   geom_histogram(bins = 50, fill = "cyan", colour = "blue") +
#   xlim(0, 100)
#
# ggplot(cr_ch_ts, aes(x = del_t_ch_hr, y = del_cr)) +
#   geom_hex(bins = 100) +
#   xlim(0, 48) + ylim(-100, 100) +
#   coord_cartesian(expand = FALSE) +
#   scale_fill_viridis_c()
