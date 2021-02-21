# ---- Create Admission Data ----
create_admin_data <- function(obs_data) {
  dup_errors <- obs_data %>%
    select(
      UR_number, AdmissionID, Pt_Study_nos,
      Max_Cr_ICU, Highest_Cr_UEC, Max_Cr_DateTime, Baseline_Cr,
      Mx_diuretics, Mx_IVF, Criteria_for_stage_of_AKI
    ) %>%
    distinct() %>%
    group_by(AdmissionID) %>%
    mutate(duplicates = n()) %>%
    filter(duplicates > 1) %>%
    arrange(desc(duplicates)) %>%
    ungroup() %>%
    select(-UR_number, -AdmissionID)

  dup_errors[seq(1, nrow(dup_errors), by = 2), ] ==
    dup_errors[seq(2, nrow(dup_errors), by = 2), ]

  dup_errors %>%
    kable(., caption = "Errors between L and LT obs data", booktabs = TRUE)
  # Reason: Cr and Olig epis happen at different times
  # If there is a large enough difference, then the obs data will be different


  # TODO think about a better solution
  admin_data <- obs_data %>%
    select(
      -c(
        # Episode Information
        Pt_Study_no,
        Incl_criteria_ok_crch, Incl_criteria_ok_olig,
        Epis_cr_change, Epis_olig,
        Epis_cr_change_no, Epis_olig_no, Epis_no,
        Total_no_cr_epis, Total_no_olig_epis,
        # Obs Data
        DateTime_epis, starts_with("T-4"), starts_with("T0"),
      )
    ) %>%
    group_by(AdmissionID) %>%
    # TODO think of a more generic way to solve the duplicates problem
    mutate(
      Mx_diuretics = max(Mx_diuretics), # No rm.na
      Mx_IVF = max(Mx_IVF),
      Baseline_Cr = min(Baseline_Cr),
      Criteria_for_stage_of_AKI = first(Criteria_for_stage_of_AKI)
    ) %>%
    distinct() %>%
    top_n(1, if_else(is.na(Max_Cr_ICU), 0, Max_Cr_ICU)) %>% # TODO replace with slice_max() in the future
    ungroup()

  # What about Admission ID?

  stopifnot(all.equal(
    sort(unique(obs_data$UR_number)),
    sort(unique(admin_data$UR_number))
  ))
  stopifnot(all.equal(
    sort(unique(obs_data$AdmissionID)),
    sort(admin_data$AdmissionID) # NO unique()
  ))

  return(admin_data)
}
