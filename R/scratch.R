
frusemide_ts <- xlsx_data$creat_furo$furosemide 
# Apply correction of administered method


blood_gas_ts <- xlsx_data$creat_furo$blood_gas %>% 
  select(`UR number`, TC_ICU_ADMISSION_DTTM, TC_ICU_DISCHARGE_DTTM, Pathology_Result_DTTM,
         Creatinine_level = `Blood Gas Creatinine`) %>% 
  mutate(Pathology = "Blood Gas")
bio_chem_ts <- xlsx_data$creat_furo$bio_chem %>% 
  select(`UR number`, TC_ICU_ADMISSION_DTTM, TC_ICU_DISCHARGE_DTTM, Pathology_Result_DTTM,
         Creatinine_level = `Creatinine Level`) %>% 
  mutate(Pathology = "Bio Chem")

UR_number_list <- unique(filter(screening_log, Event !=0)$`UR number`)

creatinine_ts <- rbind(blood_gas_ts, bio_chem_ts) %>% 
  arrange(`UR number`, TC_ICU_ADMISSION_DTTM, Pathology_Result_DTTM) %>% 
  filter(
    `UR number` %in% UR_number_list,
    Pathology_Result_DTTM > as.Date('2018-01-01')
    ) %>% 
  # apply correction if  bio chem or blood gas
  group_by(`UR number`) %>% 
  mutate(ICU_Admission = cumsum(TC_ICU_ADMISSION_DTTM != lag(TC_ICU_ADMISSION_DTTM, default = 0))) %>% 
  arrange(-ICU_Admission, `UR number`, TC_ICU_ADMISSION_DTTM, Pathology_Result_DTTM)

UR <- UR_number_list[2]
UR <- unique(xlsx_data$creat_furo$blood_gas$`UR number`)[1]  # Clear outlier
UR <- unique(xlsx_data$creat_furo$blood_gas$`UR number`)[2]  # Three admissions


ggplot(
  filter(creatinine_ts, `UR number` == UR), 
  aes(x = Pathology_Result_DTTM, 
      y = Creatinine_level, 
      group = TC_ICU_ADMISSION_DTTM,
      colour = Pathology,
      fill = Pathology
      )
  ) +
  geom_line() +
  geom_point() +
  geom_smooth(method='loess', formula = 'y ~x', span = 0.5, se = TRUE) +
  # geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) +
  facet_wrap(vars(ICU_Admission), nrow = 1, scales = "free_x")

# Need to fix time zones!

# Consider median filter e.g. robfilter