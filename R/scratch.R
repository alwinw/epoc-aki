xlsx_data$creatinine$data_set

creatinine_data <- xlsx_data$creatinine$data_set %>% 
  rename(
    Pt_Study_no_crch = Pt_Study_no,
    
  ) %>% 
  mutate(Epis_cr_change = "Y")

oliguria_data <- xlsx_data$oliguria$data_set

setdiff(colnames(creatinine_data), colnames(oliguria_data))

colnames(creatinine_data)

dttm_col = inner_join(
  find_cols("date", "DateTime", colnames(creatinine_data)),
  find_cols("time", "DateTime", colnames(creatinine_data)), 
  by = "match") %>% 
  select(date, time, match)
