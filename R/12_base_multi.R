
baseline_df <- admission_data %>%
  filter(Excl_criteria_ok == 1) %>%
  filter(Event != "Neither") %>%   # TODO In the future, this should not be an exclusion
  select(
    `UR number`:Admission, Pt_Study_nos, Event,
    Age, APACHE_II, APACHE_III, Baseline_Cr, PCs_cardio, Vasopressor,
    AKI_ICU, AKI_stage
  ) %>%
  arrange(APACHE_II)

ggplot(baseline_df, aes(
  x = APACHE_II,
  y = as.factor(AKI_ICU),
  fill=factor(stat(quantile)))
) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantile_lines = TRUE, quantiles = c(0.025, 0.25, 0.50, 0.75, 0.975),
    jittered_points = TRUE, scale = 0.7, alpha = 0.2,
    point_size = 1, point_alpha = 1,
    position = position_raincloud(adjust_vlines = TRUE)
  ) +
  scale_fill_viridis_d(name = "APACHE_II Quartiles")

ggplot(baseline_df, aes(
    x = APACHE_III,
    y = as.factor(AKI_ICU),
    fill=factor(stat(quantile)))
  ) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantile_lines = TRUE, quantiles = c(0.025, 0.25, 0.50, 0.75, 0.975),
    jittered_points = TRUE, scale = 0.7, alpha = 0.2,
    point_size = 1, point_alpha = 1,
    position = position_raincloud(adjust_vlines = TRUE)
  ) +
  scale_fill_viridis_d(name = "APACHE_III Quartiles")

ggplot(baseline_df, aes(x = APACHE_III, y = as.factor(AKI_ICU))) +
  geom_density_ridges(
    jittered_points = TRUE,
    position = position_points_jitter(width = 0.05, height = 0),
    point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7,
  )

ggplot(baseline_df, aes(x = APACHE_II, y = APACHE_III)) +
  geom_point()


## THERE IS STILL ONE WITH 0 APACHE from APD extract

median(baseline_df$APACHE_II, na.rm = TRUE)
median(baseline_df$APACHE_III, na.rm = TRUE)
