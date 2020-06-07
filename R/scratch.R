colnames(screening_log)

colnames(xlsx_data$apd_extract$apd_extract)

apd_extract <- xlsx_data$apd_extract$apd_extract %>% 
  select(
    `HRN/NIH`, DOB, Sex, 
    HOSP_ADM_DTM, ICU_ADM_DTM,
    AP2score, AP3score,
    Diabetes, CREATHI, CREATLO) %>% 
  rename(`UR number` = `HRN/NIH`)

nrow(apd_extract)
length(unique(apd_extract$`UR number`))

screening_log_thin <- screening_log %>% 
  select(`UR number`, Admission, Event, contains("_admit"), starts_with("APACHE")) %>% 
  filter(!is.na(Date_ICU_admit)) %>% 
  mutate(
    DateTime_hosp_admit = paste(
      format(Date_hosp_admit, format = "%Y-%m-%d"), 
      format(Time_hosp_admit, format = "%H:%M:%S")),
    DateTime_ICU_admit = paste(
      format(Date_ICU_admit, format = "%Y-%m-%d"), 
      format(Time_ICU_admit, format = "%H:%M:%S"))
    ) %>% 
  select(-starts_with("Time_"), -starts_with("Date_")) %>% 
  mutate_at(
    vars(DateTime_hosp_admit, DateTime_ICU_admit),
    as_datetime,
    tz = "Australia/Melbourne"
  ) %>% 
  mutate(
    DT_start = DateTime_ICU_admit - hours(30),
    DT_end   = DateTime_ICU_admit + hours(30)
  )

nrow(screening_log_thin)
length(intersect(screening_log_thin$`UR number`, apd_extract$`UR number`))


library(fuzzyjoin)

test <- fuzzy_left_join(
  screening_log_thin, apd_extract,
  by = c(
    "UR number" = "UR number",
    "DT_start"  = "ICU_ADM_DTM",
    "DT_end"    = "ICU_ADM_DTM"
  ),
  match_fun = list(`==`, `<=`, `>=`)
)

sum(!is.na(unique(test$`UR number.y`)))

