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
      n_admissions_pos = 0,
      n_admissions_neg = 0,
      n_UR             = 0,
      n                = 0,
      n_event_pos      = 0,
      n_event_neg      = 0
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

  # TODO consider using summarise to generate pos/neg breakdown

  return(data.frame(
    AUC              = logit_cut$AUC,
    sensitivity      = logit_cut$sensitivity[[1]],
    specificity      = logit_cut$specificity[[1]],
    optimal_cutpoint = logit_cut$optimal_cutpoint,
    n_admissions     = length(unique(logit_ts$AdmissionID)),
    n_admissions_pos = length(unique(logit_ts$AdmissionID[logit_ts$AKI_ICU == 1])),
    n_admissions_neg = length(unique(logit_ts$AdmissionID[logit_ts$AKI_ICU == 0])),
    n_UR             = length(unique(logit_ts$`UR number`)),
    n                = nrow(logit_ts),
    n_event_pos      = sum(logit_ts$AKI_ICU == 1),
    n_event_neg      = sum(logit_ts$AKI_ICU == 0)
  ))
}

# ---- example_1 ----
crch_window = 1
crch_centre = 4
hr_before_aki = 8
result_df <- cr_ch_model(crch_centre - crch_window/2, crch_centre + crch_window/2, hr_before_aki) %>%
  pivot_longer(
    ends_with("pos")|ends_with("neg"),
    names_to = c("admission", "heatmap"), values_to = "count",
    names_pattern = "n_?(.*)_(.*)"
  ) %>%
  pivot_wider(
    names_from = admission,
    values_from = count
  ) %>%
  mutate(
    heatmap = if_else(heatmap == "neg", "No AKI", paste0("T_AKI in ", hr_before_aki, "+hrs after cr_ch"))
  )

heatmap_all <- admission_ts %>%
  filter(is.na(del_t_aki) | del_t_aki > hr_before_aki) %>%
  mutate(
    heatmap = case_when(
      is.na(del_t_aki)          ~ "No AKI",
      del_t_aki > hr_before_aki ~ paste0("T_AKI in ", hr_before_aki, "+hrs after cr_ch"),
      TRUE                      ~ NA_character_
    ),
  )

heatmap_count <- heatmap_all %>%
  group_by(heatmap) %>%
  summarise(n_cr = n(), n_admission = n_distinct(AdmissionID), .groups = "keep")

heatmap_ts <- heatmap_all %>%
  mutate(del_t_ch = as.numeric(del_t_ch, "hours")) %>%
  filter(del_t_ch < 13, abs(del_cr) < 50) %>%
  select(del_t_ch, del_cr, heatmap)

ggplot(heatmap_ts, aes(x = del_t_ch, y = del_cr)) +
  geom_density_2d_filled(contour_var = "density") +
  geom_hline(yintercept = 0, colour = "white", linetype = "dotted") +
  annotate("tile", x = crch_centre, y = 2.5, width = crch_window, height = 55,
    fill = "white", colour = NA, alpha = 0.1, size = 0.2
  ) +
  annotate(
    "segment", 
    x    = c(crch_centre - crch_window/2, crch_centre + crch_window/2), 
    xend = c(crch_centre - crch_window/2, crch_centre + crch_window/2), 
    y = c(-22, -22), 
    yend = c(22, 22),
    colour = "white"
  ) + 
  geom_vline(xintercept = crch_centre, colour = "white", linetype = "dotted") +
  geom_text(
    aes(x = crch_centre - crch_window/2 - 0.5, y = 22,
        label = paste0("Captured cr_ch: ", crch_centre - crch_window/2, " < \u0394 t < ", crch_centre + crch_window/2,
                       "\nn(Admissions): ", admissions,
                       "\nAUC: ", round(AUC, 4))),
    data = result_df,
    colour = "white", hjust = -0, vjust = -0.1,
  ) +
  geom_text(
    aes(x = 0.2, y = -24,
        label = paste0("All cr_ch:\nn(Admissions): ", n_admission, "\nn(cr_ch events): ", n_cr)),
    data = heatmap_count,
    colour = "white", hjust = 0, vjust = 0
  ) +
  facet_wrap(~heatmap, nrow = 1) +
  scale_x_continuous(breaks = seq(0, 12, by = 2)) +
  coord_cartesian(xlim = c(0, 12), ylim = c(-25, 30), expand = FALSE) +
  ggtitle("Creatinine Changes") +
  xlab(expression("Duration of small change in Cr epis: "*Delta*"t"["cr_ch"]*" (hours)")) +
  ylab(expression("Change in Cr during epis: "*Delta*"cr"*" ("*mu*"mol/L)")) +
  scale_fill_viridis_d("Density") +
  theme(panel.spacing = unit(0.8, "lines"))
