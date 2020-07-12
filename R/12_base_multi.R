
baseline_df <- admission_data %>%
  filter(Excl_criteria_ok == 1) %>%
  filter(Event != "Neither") %>%   # TODO In the future, this should not be an exclusion
  select(
    `UR number`:Admission, Pt_Study_nos, Event,
    Age, APACHE_II, APACHE_III, Baseline_Cr, PCs_cardio, Vasopressor,
    AKI_ICU, AKI_stage)
