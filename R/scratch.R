test <- rbind(creatinine_data,  oliguria_data) %>% 
  fill(Pt_Study_no, .direction = "down") %>% 
  select(all_of(c("Pt_Study_no", "Epis_no", dttm_col$date, dttm_col$time))) %>% 
  pivot_longer(
    all_of(c(dttm_col$date, dttm_col$time)),
    names_to = "DateTimeName",
    values_to = "DateTime"
  ) %>% 
  mutate(
    DateTimeType = if_else(grepl("^time|time$", DateTimeName, ignore.case = TRUE), "Time", ""),
    DateTimeType = if_else(grepl("^date|date$", DateTimeName, ignore.case = TRUE), "Date", DateTimeType),
    DateTimeName = gsub("^time|time$|^date|date$", "DateTime", DateTimeName, ignore.case = TRUE)
  ) %>% 
  pivot_wider(
    names_from = "DateTimeType",
    values_from = "DateTime"
  ) %>% 
  mutate(
    datetime = ifelse(
      (is.na(Date) | is.na(Time)),
      NA,
      paste(format(Date, format = "%Y-%m-%d"), format(Time, format = "%H:%M:%S"))),
    Date = NULL,
    Time = NULL
  ) %>% 
  mutate(datetime = as_datetime(datetime, tz = "Australia/Melbourne"))


test <- data_set %>% 
  select(starts_with("date") | starts_with("time") | ends_with("time")) %>% 
  mutate(
    T0_corresp_DateTime = ceiling_date(DateTime_epis, "hour"),
    T0_corresp_check = format(T0_corresp_time, format = "%H:%M:%S") == 
      format(T0_corresp_DateTime, format = "%H:%M:%S")
  ) %>% 
  filter(!T0_corresp_check)
