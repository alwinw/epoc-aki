# ---- combine_blood_gas_bio_chem ----
UR_number_list <- unique(admission_data$`UR number`)
blood_gas_adjust <- 2 # FIXME Estimated!! Would need something that matches the mean AND the variance

blood_gas_ts <- xlsx_data$creat_furo$blood_gas %>%
  select(
    `UR number`, TC_ICU_ADMISSION_DTTM, TC_ICU_DISCHARGE_DTTM, Pathology_Result_DTTM,
    `Blood Gas Creatinine`
  ) %>%
  mutate(Pathology = "Blood Gas", `Bio Chem Creatinine` = NA)
bio_chem_ts <- xlsx_data$creat_furo$bio_chem %>%
  select(`UR number`, TC_ICU_ADMISSION_DTTM, TC_ICU_DISCHARGE_DTTM, Pathology_Result_DTTM,
    `Bio Chem Creatinine` = `Creatinine Level`
  ) %>%
  mutate(Pathology = "Bio Chem", `Blood Gas Creatinine` = NA)

creatinine_ts <- rbind(blood_gas_ts, bio_chem_ts) %>%
  arrange(`UR number`, TC_ICU_ADMISSION_DTTM, Pathology_Result_DTTM) %>%
  filter(
    `UR number` %in% UR_number_list,
    Pathology_Result_DTTM > as.Date("2018-01-01")
  ) %>%
  mutate(
    Creatinine_level = if_else(Pathology == "Bio Chem", `Bio Chem Creatinine`, `Blood Gas Creatinine` + blood_gas_adjust)
  ) %>%
  filter(!is.na(Creatinine_level)) %>%
  mutate_at(
    vars(ends_with("DTTM")),
    force_tz,
    tzone = "Australia/Melbourne"
  ) %>%
  group_by(`UR number`) %>%
  mutate(ICU_Admission = cumsum(TC_ICU_ADMISSION_DTTM != lag(TC_ICU_ADMISSION_DTTM, default = as.POSIXct("1990-01-01")))) %>% # Arbitrarily chosen
  arrange(`UR number`, Pathology_Result_DTTM)

rm(blood_gas_ts, bio_chem_ts, UR_number_list)


# ---- example_creatinine_plot ----
UR_number <- creatinine_ts %>%
  arrange(-ICU_Admission) %>%
  select(`UR number`) %>%
  unique(.) %>%
  .[[3, 1]]
Baseline_Cr <- admission_data %>%
  filter(`UR number` == UR_number) %>%
  select(Baseline_Cr) %>%
  .[[1, 1]]
example_cr_plot <- filter(creatinine_ts, `UR number` == UR_number) %>% mutate(ICU_Admission = paste0("ICU Admission ", ICU_Admission))

ggplot(
  example_cr_plot,
  aes(
    x = Pathology_Result_DTTM,
    y = Creatinine_level,
    group = ICU_Admission,
  )
) +
  geom_line(linetype = "dashed", colour = "grey") +
  geom_point(aes(colour = Pathology)) +
  geom_hline(yintercept = Baseline_Cr * c(1, 1.5, 2, 3)) +
  geom_label(aes(max(example_cr_plot$Pathology_Result_DTTM), Baseline_Cr, label = "Baseline Cr"), hjust = 1) +
  geom_label(aes(max(example_cr_plot$Pathology_Result_DTTM), Baseline_Cr * 1.5, label = "AKI Stage 1"), hjust = 1) +
  facet_wrap(vars(ICU_Admission), nrow = 1, scales = "free_x") +
  coord_cartesian(
    ylim = c(
      max(min(example_cr_plot$Creatinine_level), Baseline_Cr),
      min(max(example_cr_plot$Creatinine_level), Baseline_Cr * 3)
    )
  )

rm(UR_number, Baseline_Cr, example_cr_plot)


# ---- plot_blood_gas_vs_bio_chem ----
bio_chem_blood_gas <- creatinine_ts %>%
  select(-TC_ICU_ADMISSION_DTTM, -TC_ICU_DISCHARGE_DTTM, -`Blood Gas Creatinine`, -`Bio Chem Creatinine`) %>%
  group_by(`UR number`, ICU_Admission) %>%
  mutate(
    Delta_t = as.double(Pathology_Result_DTTM - lag(Pathology_Result_DTTM, default = as_datetime(0)), units = "mins"),
    Delta_in = Delta_t < 45
  ) %>%
  filter(Delta_in | lead(Delta_in) & !is.na(Delta_in)) %>%
  mutate(
    Delta_cr = if_else(Delta_in, Creatinine_level - lag(Creatinine_level), 0),
    Pathology_first = if_else(Delta_in, lag(Pathology), ""),
    Pathology_last = if_else(Delta_in, Pathology, ""),
    Creatinine_first = if_else(Delta_in, lag(Creatinine_level), 0),
    Creatinine_last = if_else(Delta_in, Creatinine_level, 0),
    Pathology_first_DTTM = lag(Pathology_Result_DTTM),
    Pathology_type = paste0("First Cr reading: ", Pathology_first, ", Second Cr reading: ", Pathology_last)
  ) %>%
  filter(Delta_in) %>%
  select(
    `UR number`, ICU_Admission, Pathology_first_DTTM, Delta_t, Pathology_type,
    Pathology_first, Creatinine_first,
    Pathology_last, Creatinine_last,
    Delta_cr
  ) %>%
  filter(abs(Delta_cr) < 30)

ggplot(bio_chem_blood_gas, aes(x = Delta_t, y = Delta_cr, colour = Pathology_type)) +
  geom_hline(yintercept = c(-10, 10), linetype = "dashed", colour = "grey") +
  geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  facet_wrap(vars(Pathology_type), ncol = 1) +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(ncol = 1))

rm(bio_chem_blood_gas, blood_gas_adjust)
