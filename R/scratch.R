xlsx_data$creatinine$data_set




setdiff(colnames(creatinine_data), colnames(oliguria_data))

colnames(creatinine_data)

dttm_col = inner_join(
  find_cols("date", "DateTime", colnames(data_set)),
  find_cols("time", "DateTime", colnames(data_set)), 
  by = "match") %>% 
  select(date, time, match)

colnames(data_set)

test <- data_set[, c("Pt_Study_no", "Epis_no", dttm_col$date, dttm_col$time)] %>% 
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
  )  

