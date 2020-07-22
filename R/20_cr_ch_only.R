# ---- baseline_df ----
logit_df <- admission_ts %>%
  select(
    `UR number`:Admission, Pt_Study_nos, Event,
    Age, APACHE_II, APACHE_III, Baseline_Cr, PCs_cardio, Vasopressor:Chronic_liver_disease,
    AKI_ICU,
    del_t_ch:cr_i
  ) %>%
  mutate(
    APACHE_II  = if_else(APACHE_II  == 0, NA_real_, APACHE_II),
    APACHE_III = if_else(APACHE_III == 0, NA_real_, APACHE_III)
  ) %>%
  group_by(AKI_ICU) %>%
  mutate(
    APACHE_II  = if_else(is.na(APACHE_II),  median(APACHE_II,  na.rm = TRUE),  APACHE_II),
    APACHE_III = if_else(is.na(APACHE_III), median(APACHE_III,na.rm = TRUE), APACHE_III)
  ) %>%   # FIXME Replace with REAL data
  ungroup() %>%
  filter(abs(del_cr) < 50)

# ---- cr_ch_function ----
cr_ch_model <- function(lower_hr_del_t_ch, upper_hr_del_t_ch, hr_before_aki, plot = FALSE) {
  logit_ts <- logit_df %>%
    filter(
      duration(hour = lower_hr_del_t_ch) < del_t_ch,
      del_t_ch <= duration(hour = upper_hr_del_t_ch),
      is.na(del_t_aki) | del_t_aki >= duration(minute = hr_before_aki)
    )
  if(nrow(logit_ts) == 0 | lower_hr_del_t_ch < 0){
    return(data.frame(
      AUC              = 0,
      sensitivity      = 0,
      specificity      = 0,
      optimal_cutpoint = 0,
      n_admissions     = 0,
      n_UR             = 0,
      n                = 0,
      n_pos            = 0,
      n_neg            = 0
    ))
  }

  logit_model <- glm(
    AKI_ICU ~ Age + APACHE_II + APACHE_III + Baseline_Cr + PCs_cardio +
      Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease +
      del_cr + cr_i,
    family = "binomial",
    data = logit_ts)

  logit_ts$model = predict(logit_model, type = "response")
  logit_cut <- cutpointr(
    logit_ts, model, AKI_ICU,
    use_midpoints = TRUE,
    direction = ">=", pos_class = 1, neg_class = 0,
    method = maximize_metric, metric = youden)

  if (plot) {
    print(summary(logit_model))
    print(summary(logit_cut))
    print(plot(logit_cut))
  }

  per_admin_in = length(unique(logit_ts$AdmissionID))/length(unique(logit_df$AdmissionID))

  return(data.frame(
    AUC              = logit_cut$AUC,
    sensitivity      = logit_cut$sensitivity[[1]],
    specificity      = logit_cut$specificity[[1]],
    optimal_cutpoint = logit_cut$optimal_cutpoint,
    n_admissions     = length(unique(logit_ts$AdmissionID)),
    n_UR             = length(unique(logit_ts$`UR number`)),
    n                = nrow(logit_ts),
    n_pos            = sum(logit_ts$AKI_ICU == 1),
    n_neg            = sum(logit_ts$AKI_ICU == 0)
  ))
}

# ---- example_1 ----
crch_window = 1
crch_centre = 4
cr_ch_model(crch_centre - crch_window/2, crch_centre + crch_window/2, 8)
