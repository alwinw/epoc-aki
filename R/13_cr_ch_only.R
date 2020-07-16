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
    print(plot(logit_cut))  #geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed")
  }

  return(data.frame(
    AUC              = logit_cut$AUC,
    sensitivity      = logit_cut$sensitivity[[1]],
    specificity      = logit_cut$specificity[[1]],
    optimal_cutpoint = logit_cut$optimal_cutpoint,
    n                = nrow(logit_ts),
    n_pos            = sum(logit_ts$AKI_ICU == 1),
    n_neg            = sum(logit_ts$AKI_ICU == 0)
  ))
}

# ---- generator_function ----
gen_cr_ch_model <- function(lower, step, upper, hr_before_aki) {

}

cr_ch_model(lower_hr_del_t_ch = 3.5, upper_hr_del_t_ch = 4.5, hr_before_aki = 1/60)
cr_ch_model(lower_hr_del_t_ch = 4.5, upper_hr_del_t_ch = 5.5, hr_before_aki = 1/60)
cr_ch_model(lower_hr_del_t_ch = 5.5, upper_hr_del_t_ch = 6.5, hr_before_aki = 1/60)
cr_ch_model(lower_hr_del_t_ch = 6.5, upper_hr_del_t_ch = 7.5, hr_before_aki = 1/60)
cr_ch_model(lower_hr_del_t_ch = 7.5, upper_hr_del_t_ch = 8.5, hr_before_aki = 1/60)
cr_ch_model(lower_hr_del_t_ch = 8.5, upper_hr_del_t_ch = 9.5, hr_before_aki = 1/60)

cr_ch_model(lower_hr_del_t_ch = 5.5, upper_hr_del_t_ch = 8.5, hr_before_aki = 1/60)


cr_ch_model(lower_hr_del_t_ch = 6.5, upper_hr_del_t_ch = 12.5, hr_before_aki = 1/60)


cr_ch_steps = seq(0, 20, by = 1)
cr_ch_steps_df = data.frame(
  lower_hr_del_t_ch = head(cr_ch_steps, -1),
  upper_hr_del_t_ch = tail(cr_ch_steps, -1),
  hr_before_aki     = 12
) %>%
  mutate(del_t_ch_range = paste0("[", lower_hr_del_t_ch, ", ", upper_hr_del_t_ch, "]"))

cr_ch_steps_df %>%
  rowwise() %>%
  do(data.frame(., cr_ch_model(.$lower_hr_del_t_ch, .$upper_hr_del_t_ch, .$hr_before_aki))) %>%
  ungroup() %>%
  arrange(desc(AUC))

cr_ch_steps = seq(0.5, 20, by = 1)
cr_ch_steps_df = data.frame(
  lower_hr_del_t_ch = head(cr_ch_steps, -1),
  upper_hr_del_t_ch = tail(cr_ch_steps, -1),
  hr_before_aki     = 12
) %>%
  mutate(del_t_ch_range = paste0("[", lower_hr_del_t_ch, ", ", upper_hr_del_t_ch, "]"))

cr_ch_steps_df %>%
  rowwise() %>%
  do(data.frame(., cr_ch_model(.$lower_hr_del_t_ch, .$upper_hr_del_t_ch, .$hr_before_aki))) %>%
  ungroup() %>%
  arrange(desc(AUC))


cr_ch_steps = seq(0, 20, by = 0.5)
cr_ch_steps_df = data.frame(
  lower_hr_del_t_ch = head(cr_ch_steps, -1),
  upper_hr_del_t_ch = tail(cr_ch_steps, -1),
  hr_before_aki     = 12
) %>%
  mutate(del_t_ch_range = paste0("[", lower_hr_del_t_ch, ", ", upper_hr_del_t_ch, "]"))

cr_ch_steps_df %>%
  rowwise() %>%
  do(data.frame(., cr_ch_model(.$lower_hr_del_t_ch, .$upper_hr_del_t_ch, .$hr_before_aki))) %>%
  ungroup() %>%
  arrange(desc(AUC))
