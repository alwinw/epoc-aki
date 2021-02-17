# ---- Merge Data Set and Outcomes ----
create_data_set <- function(cr_data, olig_data, excl_Pt_Study_no) {
  creatinine_data <- cr_data$data_set %>%
    rename(
      Epis_no = Cr_epis_no,
      Date_epis = Date_Cr_epis,
      Time_epis = Time_Cr_epis,
    ) %>%
    mutate(
      Epis_cr_change = "Y",
      Epis_olig = NA
    ) %>%
    fill(Pt_Study_no, .direction = "down") %>%
    filter(!(Pt_Study_no %in% excl_Pt_Study_no))

  oliguria_data <- olig_data$data_set %>%
    rename(
      Epis_no = Olig_epis_no,
      Date_epis = Date_olig_epis,
      Time_epis = Time_olig_epis,
    ) %>%
    mutate(
      Epis_cr_change = NA,
      Epis_olig = "Y"
    ) %>%
    fill(Pt_Study_no, .direction = "down") %>%
    filter(!(Pt_Study_no %in% excl_Pt_Study_no))


  creatinine_outcomes <- cr_data$outcomes %>%
    mutate(
      Epis_cr_change = "Y",
      Epis_olig = NA
    )
  oliguria_outcomes <- olig_data$outcomes %>%
    rename(AKI_ward_48h = AKI_ward) %>%
    mutate(
      Epis_cr_change = NA,
      Epis_olig = "Y"
    )

  creatinine_joint <- full_join(
    creatinine_data, creatinine_outcomes,
    by = c("Pt_Study_no", "Epis_cr_change", "Epis_olig")
  )
  oliguria_joint <- full_join(
    oliguria_data, oliguria_outcomes,
    by = c("Pt_Study_no", "Epis_cr_change", "Epis_olig")
  )

  data_set <- rbind(creatinine_joint, oliguria_joint) %>%
    combine_date_time_cols() %>%
    mutate(
      T0_corresp_DateTime = ceiling_date(DateTime_epis, "hour"),
      `T-4_corresp_DateTime` = T0_corresp_DateTime - hours(4),
      T_corresp_check =
        format(T0_corresp_time, format = "%H:%M:%S") == format(T0_corresp_DateTime, format = "%H:%M:%S") &
          format(`T-4_corresp_DateTime`, format = "%H:%M:%S") == format(`T-4_corresp_DateTime`, format = "%H:%M:%S")
    ) %>%
    filter(!(Pt_Study_no %in% xlsx_data$excluded_Pt_Study_no))
  # TODO Add time markers - not sure what I meant here...
  stopifnot(
    "Found inconsistent times in T0" =
      data_set$T_corresp_check
  )
  # TODO additional validation of the number of rows, e.g. total epis count
  # all.equal for 1 ep, 2 ep, 3, etc for cr and olig

  return(data_set)
}


# ---- Episode Overview ----
overview_data_set <- function(data_set) {
  data_set %>%
    group_by(Epis_cr_change, Epis_olig) %>%
    summarise(Episodes = n(), .groups = "drop") %>%
    pivot_longer(
      starts_with("Epis_"),
      names_to = "Epis",
      values_to = "EpisValue"
    ) %>%
    filter(!is.na(EpisValue)) %>%
    select(Epis, Episodes) %>%
    kable(., caption = "All Episodes") %>%
    print(.)

  data_set %>%
    group_by(Epis_cr_change, Epis_olig, Pt_Study_no) %>%
    top_n(1, Epis_no) %>%
    group_by(Epis_cr_change, Epis_olig, Epis_no) %>%
    summarise(Episodes = n(), .groups = "drop") %>%
    pivot_longer(
      c(Epis_cr_change, Epis_olig),
      names_to = "Epis",
      values_to = "EpisValue"
    ) %>%
    filter(!is.na(EpisValue)) %>%
    select(Epis, Epis_no, Episodes) %>%
    kable(., caption = "Number of Episodes") %>%
    print(.)
}
