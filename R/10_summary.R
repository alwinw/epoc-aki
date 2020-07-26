# Graphical summaries
# Example: https://www.guru99.com/r-generalized-linear-model.html

epoc_aki_admissions <- epoc_aki %>%
  group_by(AdmissionID) %>%
  filter(row_number()==n()) %>%  # Consider a better method
  ungroup()



epoc_aki %>%
  group_by(Total_admissions) %>%
  summarise(
    Events = n(),
    `Admissions` = n_distinct(AdmissionID),
    `Unique Patients` = n_distinct(`UR number`)
  ) %>%
  adorn_totals("row")

# Included vs excluded


# Admissions vs Cr
# Admissions vs olig

# Should have just used the screening log here
# Cr vs olig
epoc_aki_admissions %>%
  filter(Excl_criteria_ok == 1) %>%  # TODO If no filter, there is an 'extra' one
  select(`UR number`, starts_with("Total_no_")) %>%
  mutate(
    Total_no_cr_epis = if_else(
      is.na(Total_no_cr_epis), " 0 cr epis", sprintf("%2d cr epis", Total_no_cr_epis)),
    Total_no_olig_epis = if_else(
      is.na(Total_no_olig_epis), " 0 olig epis", sprintf("%2d olig epis", Total_no_olig_epis)),
  ) %>%
  group_by(Total_no_cr_epis, Total_no_olig_epis) %>%
  summarise(
    Admissions = n(),
  ) %>%
  ungroup() %>%
  pivot_wider(names_from = Total_no_olig_epis, values_from = Admissions) %>%
  adorn_totals(c("row", "col")) %>%
  rename(Epis = Total_no_cr_epis)
# Turn this into a heat map with marginal distribution plot on the side


# Pyramid plot for male/female


baseline_roc_list = pROC::roc(AKI_ICU ~ Age + APACHE_II + APACHE_III + Baseline_Cr + PCs_cardio +
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
baseline_interaction_roc <- pROC::roc(baseline_df$AKI_ICU ~ baseline_prediction)
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
