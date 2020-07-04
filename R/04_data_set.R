# ---- find_cols_function ----
find_cols <- function(text, replace, colnames) {
  cols <- data.frame(
    i = grep(paste0("^", text, "|", text, "$"), colnames, ignore.case = TRUE),
    j = grep(paste0("^", text, "|", text, "$"), colnames, ignore.case = TRUE, value = TRUE),
    stringsAsFactors = FALSE
  ) %>%
    mutate(k = gsub(text, replace, j, ignore.case = TRUE))
  colnames(cols) <- c(paste0(text, "_i"), paste0(text), "match")

  return(cols)
}


# ---- merge_data_sets_outcomes ----
creatinine_data <- xlsx_data$creatinine$data_set %>%
  rename(
    Epis_no = Cr_epis_no,
    Date_epis = Date_Cr_epis,
    Time_epis = Time_Cr_epis,
  ) %>%
  mutate(
    Epis_cr_change = "Y",
    Epis_olig = NA
  ) %>%
  fill(Pt_Study_no, .direction = "down") %>% 
  filter(!(Pt_Study_no %in% xlsx_data$excluded_Pt_Study_no))

oliguria_data <- xlsx_data$oliguria$data_set %>%
  rename(
    Epis_no = Olig_epis_no,
    Date_epis = Date_olig_epis,
    Time_epis = Time_olig_epis,
  ) %>%
  mutate(
    Epis_cr_change = NA,
    Epis_olig = "Y"
  ) %>%
  fill(Pt_Study_no, .direction = "down") %>% 
  filter(!(Pt_Study_no %in% xlsx_data$excluded_Pt_Study_no))


creatinine_outcomes <- xlsx_data$creatinine$outcomes %>%
  mutate(
    Epis_cr_change = "Y",
    Epis_olig = NA
  )
oliguria_outcomes <- xlsx_data$oliguria$outcomes %>%
  rename(AKI_ward_48h = AKI_ward) %>%
  mutate(
    Epis_cr_change = NA,
    Epis_olig = "Y"
  )

creatinine_joint <- full_join(
  creatinine_data, creatinine_outcomes,
  by = c("Pt_Study_no", "Epis_cr_change", "Epis_olig")
)
oliguria_joint <- full_join(
  oliguria_data, oliguria_outcomes,
  by = c("Pt_Study_no", "Epis_cr_change", "Epis_olig")
)

data_set_joint <- rbind(creatinine_joint,  oliguria_joint)

rm(creatinine_data, creatinine_outcomes, creatinine_joint,
   oliguria_data, oliguria_outcomes, oliguria_joint)

# ---- merge_data_sets ----
dttm_col = inner_join(
  find_cols("date", "DateTime", colnames(data_set_joint)),
  find_cols("time", "DateTime", colnames(data_set_joint)),
  by = "match") %>%
  select(date, time, match)

raw_data_set <- data_set_joint %>%
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
    datetime = if_else(
      (is.na(Date) | is.na(Time)),
      NA_character_,
      paste(format(Date, format = "%Y-%m-%d"), format(Time, format = "%H:%M:%S"))),
    Date = NULL,
    Time = NULL
  ) %>%
  mutate(datetime = as_datetime(datetime, tz = "Australia/Melbourne")) %>%
  pivot_wider(
    names_from = "DateTimeName",
    values_from = "datetime"
  ) %>%
  mutate(
    T0_corresp_DateTime = ceiling_date(DateTime_epis, "hour"),
    `T-4_corresp_DateTime` = T0_corresp_DateTime - hours(4),
    T_corresp_check =
      format(T0_corresp_time, format = "%H:%M:%S") == format(T0_corresp_DateTime, format = "%H:%M:%S") &
      format(`T-4_corresp_DateTime`, format = "%H:%M:%S") == format(`T-4_corresp_DateTime`, format = "%H:%M:%S")
  )

# TODO Add time markers

if (any(!raw_data_set$T_corresp_check)) {
  raw_data_set %>%
    select(Pt_Study_no, T_corresp_check, starts_with("date"), starts_with("time"), ends_with("time")) %>%
    filter(!T_corresp_check) %>%
    print(.)
    stop("Found inconsistent times!")
}

raw_data_set_cols <- unique(gsub("^time|time$|^date|date$", "DateTime", colnames(data_set_joint), ignore.case = TRUE))

data_set <- raw_data_set[, raw_data_set_cols] %>%
  filter(!(Pt_Study_no %in% xlsx_data$excluded_Pt_Study_no))

rm(dttm_col, data_set_joint, raw_data_set, raw_data_set_cols, find_cols)

# ---- epis_overview ----
data_set %>%
  group_by(Epis_cr_change, Epis_olig) %>%
  summarise(Episodes = n()) %>%
  pivot_longer(
    starts_with("Epis_"),
    names_to = "Epis",
    values_to = "EpisValue"
  ) %>%
  filter(!is.na(EpisValue)) %>%
  select(Epis, Episodes) %>%
  kable(., caption = "All Episodes", booktabs = TRUE)

data_set %>%
  group_by(Epis_cr_change, Epis_olig, Pt_Study_no) %>%
  top_n(1, Epis_no) %>%
  group_by(Epis_cr_change, Epis_olig, Epis_no) %>%
  summarise(Episodes = n()) %>%
  pivot_longer(
    c(Epis_cr_change, Epis_olig),
    names_to = "Epis",
    values_to = "EpisValue"
  ) %>%
  filter(!is.na(EpisValue)) %>%
  select(Epis, Epis_no, Episodes) %>%
  kable(., caption = "Number of Episodes", booktabs = TRUE)

# Should match the screening log!!
