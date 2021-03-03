# ---- Analysis Data ----
# consider having col for ABG vs BioChem here and apply a filter !!IMPORTANT
create_analysis_data <- function(cr_ch_ts) {
  analysis_df <- cr_ch_ts %>%
    select(
      UR_number:Admission, Pt_Study_nos, Event,
      Age:Chronic_liver_disease,
      AKI_ICU,
      DateTime_Pathology_Result:AKI_2or3,
      Cr_defined_AKI, Cr_defined_AKI_2or3, Olig_defined_AKI, Olig_defined_AKI_2or3
    ) %>%
    mutate(
      APACHE_II = if_else(APACHE_II == 0, NA_real_, APACHE_II),
      APACHE_III = if_else(APACHE_III == 0, NA_real_, APACHE_III)
    ) %>%
    group_by(AKI_ICU) %>%
    mutate(
      APACHE_II = if_else(is.na(APACHE_II), median(APACHE_II, na.rm = TRUE), APACHE_II),
      APACHE_III = if_else(is.na(APACHE_III), median(APACHE_III, na.rm = TRUE), APACHE_III)
    ) %>% # FIXME Replace with REAL data
    mutate(
      cr_before_aki = if_else(is.na(del_t_aki_hr) | del_t_aki_hr >= 0, 1, 0)
    ) %>%
    ungroup()

  measurements_df <- analysis_df %>%
    mutate(temp = is.na(cr)) %>%
    select(-del_cr, -per_cr_change, -del_t_ch_hr:-del_t_aki_hr) %>%
    unique(.) %>%
    mutate(
      del_t_ch_hr = 0, # consider changing to median or something later
      del_t_aki_hr = 0,
      del_cr = temp
    ) %>%
    group_by(AdmissionID, cr_before_aki) %>%
    mutate(
      n_measurements = n()
    ) %>%
    ungroup() %>%
    select(-temp)

  baseline_df <- measurements_df %>%
    select(-DateTime_Pathology_Result:-cr) %>%
    group_by(AdmissionID) %>%
    mutate(n_measurements = if_else(cr_before_aki == 1, n_measurements, NA_integer_)) %>%
    fill(n_measurements, .direction = "updown") %>%
    # mutate(n_measurements = if_else(is.na(n_measurements), as.integer(0), n_measurements)) %>%
    select(-cr_before_aki) %>%
    ungroup() %>%
    unique(.)

  # if (anyNA(baseline_df)) stop("There is missing data in baseline_df")
  stopifnot(length(unique(baseline_df$AdmissionID)) == nrow(baseline_df))
  stopifnot(baseline_df %>% filter(AKI_ICU == 0, Cr_defined_AKI == 1) %>% nrow(.) == 0)

  # Compare with admissions data
  stopifnot(nrow(filter(baseline_df, AKI_ICU == 1)) >= nrow(filter(admission_data, AKI_ICU == 1)))
  stopifnot(nrow(filter(baseline_df, Cr_defined_AKI == 1)) >= nrow(filter(admission_data, Cr_defined_AKI == 1)))
  stopifnot(nrow(filter(baseline_df, Olig_defined_AKI == 1)) >= nrow(filter(admission_data, AKI_Dx_oliguria == 1)))

  # Remove pts that didn't have enough Cr measurements
  baseline_df <- filter(baseline_df, !del_cr)
  analysis_df <- filter(analysis_df, !is.na(del_cr))

  return(list(
    analysis = analysis_df,
    measurements = measurements_df,
    baseline = baseline_df
  ))
}

# --- time_to_aki_plot ----
# analysis_df %>%
#   filter(AKI_ICU == 1) %>%
#   group_by(AdmissionID) %>%
#   summarise(max_t_aki = max(del_t_aki_hr)) %>%
#   ggplot(., aes(max_t_aki)) +
#   geom_histogram() +
#   xlim(0, 30)
#
# analysis_df %>%
#   group_by(AdmissionID) %>%
#   summarise(min_t_ch = min(del_t_ch_hr), max_t_ch = max(del_t_ch_hr)) %>%
#   ggplot(., aes(max_t_aki)) +
#   geom_histogram() +
#   xlim(0, 30)
