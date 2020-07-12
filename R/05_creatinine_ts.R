# ---- combine-blood-gas-bio-chem ----
UR_number_list <- unique(filter(admission_data, Event != "Neither")$`UR number`)
blood_gas_adjust = 2  # FIXME Estimated!! Would need something that matches the mean AND the variance

blood_gas_ts <- xlsx_data$creat_furo$blood_gas %>%
  select(`UR number`, TC_ICU_ADMISSION_DTTM, TC_ICU_DISCHARGE_DTTM, Pathology_Result_DTTM,
         `Blood Gas Creatinine`) %>%
  mutate(Pathology = "Blood Gas", `Bio Chem Creatinine` = NA)
bio_chem_ts <- xlsx_data$creat_furo$bio_chem %>%
  select(`UR number`, TC_ICU_ADMISSION_DTTM, TC_ICU_DISCHARGE_DTTM, Pathology_Result_DTTM,
         `Bio Chem Creatinine` = `Creatinine Level`) %>%
  mutate(Pathology = "Bio Chem", `Blood Gas Creatinine` = NA)

creatinine_ts <- rbind(blood_gas_ts, bio_chem_ts) %>%
  arrange(`UR number`, TC_ICU_ADMISSION_DTTM, Pathology_Result_DTTM) %>%
  filter(
    `UR number` %in% UR_number_list,
    Pathology_Result_DTTM > as.Date('2018-01-01')
  ) %>%
  mutate(
    Creatinine_level = if_else(Pathology == "Bio Chem", `Bio Chem Creatinine`, `Blood Gas Creatinine` + blood_gas_adjust)
  ) %>%
  mutate_at(
    vars(ends_with("DTTM")),
    force_tz,
    tzone = "Australia/Melbourne"
  ) %>%
  group_by(`UR number`) %>%
  mutate(ICU_Admission = cumsum(TC_ICU_ADMISSION_DTTM != lag(TC_ICU_ADMISSION_DTTM, default = as.POSIXct("1990-01-01")))) %>%  # Arbitrarily chosen
  arrange(`UR number`, ICU_Admission, Pathology_Result_DTTM)

rm(blood_gas_ts, bio_chem_ts, blood_gas_adjust, UR_number_list)

# ---- example-creatinine-plot ----
UR_number_list = creatinine_ts %>% arrange(-ICU_Admission) %>% select(`UR number`) %>% unique(.)
UR_number = UR_number_list[3,]

ggplot(
  filter(creatinine_ts, `UR number` == UR_number),
  aes(x = Pathology_Result_DTTM,
      y = Creatinine_level,
      group = ICU_Admission,
  )
) +
  geom_line(linetype = "dashed", colour = "grey") +
  geom_point(aes(colour = Pathology)) +
  facet_wrap(vars(ICU_Admission), nrow = 1, scales = "free_x")

rm(UR_number, UR_number_list)

# ---- plot-blood-gas-vs-bio-chem ----
bio_chem_blood_gas <- creatinine_ts %>%
  select(-TC_ICU_ADMISSION_DTTM, -TC_ICU_DISCHARGE_DTTM, -`Blood Gas Creatinine`, -`Bio Chem Creatinine`) %>%
  group_by(`UR number`, ICU_Admission) %>%
  mutate(
    Delta_t = as.double(Pathology_Result_DTTM - lag(Pathology_Result_DTTM, default = as.POSIXct("1990-01-01")), units = "mins"),
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
    Delta_cr) %>%
  filter(abs(Delta_cr) < 30)

ggplot(bio_chem_blood_gas, aes(x = Delta_t, y = Delta_cr, colour = Pathology_type)) +
  geom_hline(yintercept = c(-10, 10), linetype = "dashed", colour = "grey") +
  geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  facet_wrap(vars(Pathology_type), ncol = 1)

# ---- creatinine_ts_screening_log ----

screening_ts <- admission_data %>%
  filter(Excl_criteria_ok == 1) %>%
  mutate(
    DateTime_ICU_dc = Date_ICU_dc + hours(23) + minutes(59) + seconds(59)
  ) %>%
  select(
    AdmissionID, `UR number`, Admission,
    DateTime_ICU_admit, DateTime_ICU_dc,
    Baseline_Cr:Cr_defined_AKI_stage
  )

screening_ts_list <- split(screening_ts, screening_ts$AdmissionID)

screening_ts_list = screening_ts_list[1:3]

lapply(screening_ts_list, function(screening_event) {
  cr_ts = creatinine_ts %>%
    filter(
      `UR number` == screening_event$`UR number`,
      Pathology_Result_DTTM > screening_event$DateTime_ICU_admit,
      Pathology_Result_DTTM < screening_event$DateTime_ICU_dc
    )
  return(list(event = screening_event, cr_ts = cr_ts))
})
