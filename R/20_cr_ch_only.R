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

# ---- cr_ch_heatmap ----
# ggplot(logit_df, aes(x = del_t_ch, y = del_cr)) +
#   geom_hex()

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
      # heuristic        = 0,
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
    print(plot(logit_cut))  #geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed")
  }

  per_admin_in = length(unique(logit_ts$AdmissionID))/length(unique(logit_df$AdmissionID))

  return(data.frame(
    # heuristic        = (logit_cut$AUC * 3 + per_admin_in^0.2)/4,
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

# ---- generator_function ----
gen_cr_ch_model <- function(lower, upper, step, hr_before_aki) {
  cr_ch_steps = seq(lower, upper, by = step)
  cr_ch_steps_df = data.frame(
    lower_hr_del_t_ch = head(cr_ch_steps, -1),
    upper_hr_del_t_ch = tail(cr_ch_steps, -1),
    hr_before_aki     = hr_before_aki
  ) %>%
    mutate(del_t_ch_range = paste0("[", lower_hr_del_t_ch, ", ", upper_hr_del_t_ch, "]")) %>%
    rowwise() %>%
    do(data.frame(., cr_ch_model(.$lower_hr_del_t_ch, .$upper_hr_del_t_ch, .$hr_before_aki))) %>%
    ungroup() %>%
    arrange(desc(heuristic), desc(AUC))

  return(cr_ch_steps_df)
}

# ---- example_models ----
cr_ch_steps = gen_cr_ch_model(0, 20, 1, 12)

ggplot(cr_ch_steps, aes())




plot_cr_ch = expand.grid(seq(0, 30, by = 0.1), seq(0, 3, by = 0.1)) %>%
  rename(centre = Var1, tol = Var2) %>%
  mutate(
    lower = centre - tol,
    upper = centre + tol) %>%
  filter(upper > lower, lower > 0) %>%
  rowwise() %>%
  do(data.frame(., cr_ch_model(.$lower, .$upper, 12))) %>%
  ungroup()

# Consider parallising above^
# cluster <- new_cluster(4)
# cluster_assign(cluster, logit_df = logit_df, cr_ch_model = cr_ch_model)
# partition(cluster)

head(plot_cr_ch %>% arrange(desc(heuristic), desc(AUC)))
head(plot_cr_ch %>% mutate(heuristic = (AUC*3 + (n_admissions/313))) %>% arrange(desc(heuristic), desc(AUC)), 20)

ggplot(plot_cr_ch %>% filter(AUC > 0), aes(centre, tol, fill = AUC)) +
  geom_tile() +
  geom_contour(aes(z = AUC, colour = after_stat(level)),
               binwidth = 0.01) + #, colour = after_stat(level))) +
  coord_fixed() +
  scale_fill_viridis_c() +
  scale_colour_viridis_c()

ggplot(plot_cr_ch %>% filter(AUC > 0), aes(centre, tol, fill = heuristic)) +
  geom_tile() +
  geom_contour(aes(z = heuristic), colour = "white", binwidth = 0.025) + #, colour = after_stat(level))) +
  coord_fixed() +
  scale_fill_viridis_c() +
  scale_colour_viridis_c()

ggplot(plot_cr_ch %>% filter(AUC > 0), aes(centre, tol, fill = n_admissions)) +
  geom_tile() +
  geom_contour(aes(z = n_admissions), colour = "white", binwidth = 20) + #, colour = after_stat(level))) +
  geom_text_contour(aes(z = n_admissions), colour = "white") +
  coord_fixed() +
  scale_fill_viridis_c() +
  scale_colour_viridis_c()

ggplot(plot_cr_ch %>% filter(AUC > 0), aes(centre, tol)) +
  geom_tile(aes(fill = AUC)) +
  geom_contour(aes(z = n_admissions), colour = "white", binwidth = 20) + #, colour = after_stat(level))) +
  geom_text_contour(aes(z = n_admissions), colour = "white") +
  # geom_text_contour(aes(z = AUC), colour = "white") +
  coord_fixed() +
  scale_fill_viridis_c() +
  scale_colour_viridis_c()

# ---- optimisation ----
optim(
  c(10, 11),
  function(x) -cr_ch_model(x[1], x[2], 12)$heuristic,
  lower = c(0, 0),
  upper = c(200, 200),
  method = "L-BFGS-B"
)
