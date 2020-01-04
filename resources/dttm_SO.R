library(reprex)

library(tidyverse)
tbl <- read_table2(
  "x   y   dateA       timeA   dateB       timeB   time_obs
   A   1   2020/1/29   3:30PM  2020/10/1   1:20AM  2:30PM
   B   2   2019/12/2   8:10AM  2020/5/15   3:40PM  1:50AM
   C   3   NA          4:20PM  NA          NA      5:20AM"
) %>% 
  mutate_at(vars(matches("date|time")), as.POSIXct)

tidy_tbl <- tbl %>% 
  pivot_longer(
    cols = c("dateA", "timeA", "dateB", "timeB"),
    names_to = "dttm_name",
    values_to = "dttm"
    ) %>% 
  mutate(
    type = if_else(grepl("date", dttm_name, ignore.case = TRUE), "date", "time"),
    dttm_name = gsub("date|time", "DateTime", dttm_name, ignore.case = TRUE)
    ) %>% 
  pivot_wider(
    names_from = "type",
    values_from = "dttm"
    ) %>% 
  mutate(
    datetime = if_else(
      (is.na(date) | is.na(time)),
      NA,
      paste(
        format(date, format = "%d-%m-%Y"),
        format(time, format = "%H:%M:%S")
      ))
  ) %>% 
  select(-date, -time) %>% 
  pivot_wider(
    names_from = "dttm_name",
    values_from = "datetime"
  )
  
# Copy and paste lines of interest
# run 
reprex()
