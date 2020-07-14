
baseline_df <- admission_data %>%
  filter(Excl_criteria_ok == 1) %>%
  filter(Event != "Neither") %>%   # TODO In the future, this should not be an exclusion
  select(
    `UR number`:Admission, Pt_Study_nos, Event,
    Age, APACHE_II, APACHE_III, Baseline_Cr, PCs_cardio, Vasopressor:Chronic_liver_disease,
    AKI_ICU, AKI_stage
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
  ungroup()


baseline_model <- glm(
  AKI_ICU ~ Age + APACHE_II + APACHE_III + Baseline_Cr + PCs_cardio +
    Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease,
  family = "binomial", data = baseline_df)

print(summary(baseline_model))
publish(baseline_model)

baseline_prediction <- predict(baseline_model, type = "response")
baseline_roc <- roc(baseline_df$AKI_ICU ~ baseline_prediction)
plot(baseline_roc)

pROC::ggroc(baseline_roc) +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed") +
  coord_fixed()

baseline_roc_list = roc(AKI_ICU ~ Age + APACHE_II + APACHE_III + Baseline_Cr + PCs_cardio +
                          Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease, data = baseline_df)
pROC::ggroc(baseline_roc_list) +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed") +
  coord_fixed()

baseline_interaction_model <- glm(
  AKI_ICU ~ Age + APACHE_II*APACHE_III + Baseline_Cr + PCs_cardio + Vasopressor,
  family = "binomial", data = baseline_df)

print(summary(baseline_interaction_model))
publish(baseline_interaction_model)

baseline_interaction_prediction <- predict(baseline_interaction_model, type = "response")
baseline_interaction_roc <- roc(baseline_df$AKI_ICU ~ baseline_prediction)
ggroc(baseline_interaction_roc) +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed") +
  coord_fixed()

ggplot(baseline_df, aes(x = PCs_cardio, y = AKI_ICU)) +
  geom_point(shape=1, position=position_jitter(width=.05,height=.05)) +
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

ggplot(baseline_df, aes(x = Chronic_liver_disease, y = AKI_ICU)) +
  geom_point(shape=1, position=position_jitter(width=.05,height=.05)) +
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

ggplot(baseline_df, aes(x = Age, y = AKI_ICU)) +
  geom_point() +
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

ggplot(mtcars, aes(x = wt, y = vs)) +
  geom_point() +
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)


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
  geom_point() +
  geom_smooth(method = lm)
