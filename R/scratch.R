colnames(screening_log)

colnames(xlsx_data$apd_extract$apd_extract)

apd_extract <- xlsx_data$apd_extract$apd_extract %>%
  select(
    `HRN/NIH`, # DOB, Sex,
    HOSP_ADM_DTM, ICU_ADM_DTM,
    AP2score, AP3score) %>%
  mutate(
    AP2score = as.numeric(AP2score),
    AP3score = as.numeric(AP3score)
  )

nrow(apd_extract)
length(unique(apd_extract$`HRN/NIH`))

screening_log_thin <- screening_log %>%
  select(`UR number`, Admission, Event, DateTime_ICU_admit, starts_with("APACHE")) %>%
  filter(!is.na(DateTime_ICU_admit)) %>%
  mutate(
    DT_start = DateTime_ICU_admit - hours(26),
    DT_end   = DateTime_ICU_admit + hours(26)
  )

nrow(screening_log_thin)
length(intersect(screening_log_thin$`UR number`, apd_extract$`HRN/NIH`))


test <- fuzzy_left_join(
  screening_log_thin, apd_extract,
  by = c(
    "UR number" = "HRN/NIH",
    "DT_start"  = "ICU_ADM_DTM",
    "DT_end"    = "ICU_ADM_DTM"
  ),
  match_fun = list(`==`, `<=`, `>=`)
) %>%
  select(-DT_start, -DT_end)

sum(!is.na(unique(test$`HRN/NIH`)))
nrow(test)

test_err <-  test %>%
  mutate(
    AP_replace = (abs(AP2score - APACHE_II) + abs(AP3score - APACHE_III)) > 100,
    AP_replace = AP_replace | is.na(AP_replace)
  ) %>%
  arrange(-AP_replace) %>%
  filter(AP_replace) %>%
  select(`UR number`:`DateTime_ICU_admit`, AP2score:AP3score)

nrow(test_err)
