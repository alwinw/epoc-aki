UR_number_list <- unique(filter(screening_log, Event !=0)$`UR number`)

frusemide_ts <- xlsx_data$creat_furo$furosemide 
# Apply correction of administered method


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
    Creatinine_level = if_else(Pathology == "Bio Chem", `Bio Chem Creatinine`, `Blood Gas Creatinine`) 
  ) %>% 
  group_by(`UR number`) %>% 
  mutate(ICU_Admission = cumsum(TC_ICU_ADMISSION_DTTM != lag(TC_ICU_ADMISSION_DTTM, default = 0))) %>% 
  arrange(-ICU_Admission, `UR number`, ICU_Admission, Pathology_Result_DTTM)

summary(creatinine_ts %>% select(`Bio Chem Creatinine`, `Blood Gas Creatinine`))

bio_chem_blood_gas <- creatinine_ts %>% 
  select(-TC_ICU_ADMISSION_DTTM, -TC_ICU_DISCHARGE_DTTM, -`Blood Gas Creatinine`, -`Bio Chem Creatinine`) %>% 
  group_by(`UR number`, ICU_Admission) %>% 
  mutate(
    Delta_t = as.double(Pathology_Result_DTTM - lag(Pathology_Result_DTTM, 1, 0), units = "mins"),
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








ggplot(
  creatinine_delta,
  aes(x = Pathology_Result_DTTM, 
      y = Creatinine_level, 
      group = Group,
      fill = Pathology)
  ) +
  geom_line()


UR <- UR_number_list[2]
UR <- unique(xlsx_data$creat_furo$blood_gas$`UR number`)[1]  # Clear outlier
UR <- unique(xlsx_data$creat_furo$blood_gas$`UR number`)[2]  # Three admissions


ggplot(
  filter(creatinine_ts, `UR number` == UR), 
  aes(x = Pathology_Result_DTTM, 
      y = Creatinine_level, 
      group = ICU_Admission,
      colour = Pathology,
      fill = Pathology
      )
  ) +
  geom_line() +
  geom_point() +
  geom_smooth(method='loess', formula = 'y ~x', span = 0.3, se = FALSE, colour = "grey", linetype = "dashed") +
  # geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) +
  facet_wrap(vars(ICU_Admission), nrow = 1, scales = "free_x")

# Need to fix time zones!


# Consider median filter e.g. robfilter

test <- filter(creatinine_ts, `UR number` == UR)

test_zoo <- zoo(test$Creatinine_level, test$Pathology_Result_DTTM)
