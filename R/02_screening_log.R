# ----join_demo_screen_log_sheets_function ----
join_demo_screen_log_sheets <- function(demographic, screen_log){
  joint <- full_join(demographic, screen_log, by = "Pt_Study_no")

  if (anyNA(joint$`UR number`)) {
    stop("NA in UR number of merged excel sheets")
  }
  if (nrow(joint) != nrow(screen_log)) {
    stop(paste(
      "Extra rows have been added. Actual:", nrow(joint),
      "Expected: ", nrow(screen_log)))
  }

  joint <- joint %>%
    arrange(Total_admissions, `UR number`, Admission)

  return(joint)
}


# ---- screen_log_all ----
screen_logs <- list()

screen_logs$creatinine <- join_demo_screen_log_sheets(
  xlsx_data$creatinine$demographic, xlsx_data$creatinine$screen_log
)
screen_logs$oliguria <- join_demo_screen_log_sheets(
  xlsx_data$oliguria$demographic, xlsx_data$oliguria$screen_log
)

screen_logs$in_merge_cols = setdiff(
  intersect(colnames(screen_logs$creatinine),
            colnames(screen_logs$oliguria)),
  c("Incl_criteria_ok", "Pt_Study_no", "Comment")
)
screen_logs$screen_in <- full_join(
  screen_logs$creatinine, screen_logs$oliguria,
  by = screen_logs$in_merge_cols, suffix = c("_crch", "_olig"))

# Fill missing NAs due to being screened in for cr and out for olig and vice versa
# Remove issues with destination due to typos and APACHE_II, APACHE_III
screen_logs$screen_in <- screen_logs$screen_in %>%
  group_by(`UR number`, Admission) %>%
  mutate(duplicates = n()) %>%
  arrange(desc(duplicates), `UR number`) %>%
  fill(-`UR number`, -Admission, .direction = "downup") %>%
  mutate(
    Dc_destination = first(Dc_destination),  # Could be bad if first is NA. Change to mutate if duplicates then....
    APACHE_II   = max(APACHE_II , 0, na.rm = TRUE),
    APACHE_III  = max(APACHE_III, 0, na.rm = TRUE),
    APACHE_II   = if_else(APACHE_II  == 0, NA_real_, APACHE_II),
    APACHE_III  = if_else(APACHE_III == 0, NA_real_, APACHE_III),
    Time_ICU_dc = if_else(duplicates > 1, max(Time_ICU_dc), Time_ICU_dc)  # TODO no na.rm on this one
  ) %>%
  distinct() %>%
  mutate(
    duplicates = n()) %>%
  arrange(desc(duplicates), `UR number`) %>%
  ungroup() %>%
  select(-duplicates)

if (anyNA(screen_logs$screen_in$`UR number`)) {
  stop("NA in UR number of merged screening logs sheets")
}
if (nrow(screen_logs$creatinine) != nrow(screen_logs$screen_in)) {
  stop(paste(
    "Extra rows have been added. Actual:", nrow(screen_logs$screen_in),
    "Expected: ", nrow(screen_logs$creatinine)))
}

screen_logs$screen_out <- rbind(
  xlsx_data$screen_out$no_creatinine,
  xlsx_data$screen_out$no_oliguria,
  xlsx_data$screen_out$neither_cr_ol
) %>%
  distinct() %>%
  rename(
    `UR number` = UR,
    Comment_out = Comment
    )%>%
  group_by(`UR number`, `Date first screened`) %>%
  mutate(
    APACHE_II  = max(APACHE_II , 0, na.rm = TRUE),
    APACHE_III = max(APACHE_III, 0, na.rm = TRUE),
    APACHE_II  = if_else(APACHE_II  == 0, NA_real_, APACHE_II),
    APACHE_III = if_else(APACHE_III == 0, NA_real_, APACHE_III),
  ) %>%
  distinct() %>%
  group_by(`UR number`) %>%
  top_n(-1, `Date first screened`)  # Consider not removing, then grouping by neither, arrange by date, then fill up

screen_logs$errors_logi = screen_logs$screen_out$`UR number` %in%
  xlsx_data$excluded_UR_numbers

screen_logs$screen_out[screen_logs$errors_logi, "Excl_criteria_ok"] = "N"
screen_logs$screen_out[screen_logs$errors_logi, "Already_AKI"] = "Y"

# All the URs in screen_out should be in screen_in. Remove any errors
screen_logs$neither_UR <-
  setdiff(unique(screen_logs$screen_out$`UR number`),
          unique(screen_logs$screen_in$`UR number`))
screen_logs$screen_neither <- screen_logs$screen_out %>%
  filter(`UR number` %in% screen_logs$neither_UR)

if (FALSE) {
  kable(
    screen_logs$screen_neither, caption = 'Admissions found in screen out',
    booktabs = TRUE
  )
}

if (nrow(screen_logs$screen_out) != length(unique(screen_logs$screen_out$`UR number`))) {
  stop(paste(
    "Duplicate UR numbers found. Actual:", nrow(screen_logs$screen_out),
    "Expected: ", length(unique(screen_logs$screen_out$`UR number`))))
}

screen_logs$full_merge_cols = intersect(
  colnames(screen_logs$screen_in),
  colnames(screen_logs$screen_out)
)
screen_logs$full <- full_join(
  screen_logs$screen_in, screen_logs$screen_out,
  by = screen_logs$full_merge_cols
)

screen_logs$full <- screen_logs$full %>%
  group_by(`UR number`) %>%
  mutate(
    Total_rows = n(),
    duplicates = Total_admissions != Total_rows,
    Event      = if_else(Epis_olig == "Y",      1, 0, 0),
    Event      = if_else(Epis_cr_change == "Y", 2, 0, 0) + Event,
    Event      = factor(
      Event, levels = c(0, 1, 2, 3),
      labels = c("Neither", "Olig only", "Cr change only", "Both"))
    ) %>%
  group_by(`UR number`, Event) %>%
  arrange(-Total_rows, `UR number`, Event, Admission) %>%
  fill(-`UR number`, -Event, .direction = "updown") %>%
  distinct() %>%
  group_by(`UR number`) %>%
  mutate(
    Total_rows = n(),
    duplicates = Total_admissions != Total_rows
    ) %>%
  ungroup() %>%
  filter(!is.na(Admission)) %>%
  select(
    `UR number`, starts_with("Pt_Study_no"),
    starts_with("Incl_criteria"), starts_with("Epis_"), starts_with("Total_no_"),
    Dates_screened:Child, Age:Dc_destination,
    Admission, Total_admissions, Event, starts_with("Comment")
  ) %>%
  arrange(`UR number`, Admission)

if (nrow(screen_logs$full) != nrow(screen_logs$screen_in)) {
  stop(paste(
    "Duplicate UR numbers found. Actual:", nrow(screen_logs$full),
    "Expected: ", nrow(screen_logs$screen_in)))
}
if (anyNA(screen_logs$full$`UR number`)) {
  stop("Found NAs in merged UR numbers")
}
if (sum(!is.na(screen_logs$full$Pt_Study_no_crch)) !=
    length(unique(filter(xlsx_data$creatinine$screen_log, !is.na(Pt_Study_no))$Pt_Study_no))) {
  stop("Number of LT patients has changed")
}
if (sum(!is.na(screen_logs$full$Pt_Study_no_olig)) !=
    length(unique(filter(xlsx_data$oliguria$screen_log, !is.na(Pt_Study_no))$Pt_Study_no))) {
  stop("Number of L patients has changed")
}

screening_log <- screen_logs$full %>%
  mutate(
    Date_hosp_admit = paste(
      format(Date_hosp_admit, format = "%Y-%m-%d"),
      format(Time_hosp_admit, format = "%H:%M:%S")),
    Date_ICU_admit = paste(
      format(Date_ICU_admit, format = "%Y-%m-%d"),
      format(Time_ICU_admit, format = "%H:%M:%S"))
    ) %>%
  select(-starts_with("Time_")) %>%
  rename(
    DateTime_hosp_admit = Date_hosp_admit,
    DateTime_ICU_admit  = Date_ICU_admit
    ) %>%
  mutate_at(
    vars(DateTime_hosp_admit, DateTime_ICU_admit),
    function(dt) if_else(dt == "NA NA", NA_character_, dt)
  ) %>%
  mutate_at(
    vars(DateTime_hosp_admit, DateTime_ICU_admit),
    as_datetime,
    tz = "Australia/Melbourne"
  )

if (FALSE) {
  kable(
    as.data.frame(t(head(screening_log))),
    caption = "Screening Log",
    booktabs = TRUE)
}

rm(join_demo_screen_log_sheets, screen_logs)


# ---- screen_log_apache ----
screening_log_thin <- screening_log %>%
  select(`UR number`, Admission, Event, DateTime_ICU_admit, starts_with("APACHE")) %>%
  filter(!is.na(DateTime_ICU_admit)) %>%
  mutate(
    DT_start = DateTime_ICU_admit - hours(26),
    DT_end   = DateTime_ICU_admit + hours(26)
  )

apd_extract <- xlsx_data$apd_extract$apd_extract %>%
  select(
    `HRN/NIH`, # DOB, Sex,
    HOSP_ADM_DTM, ICU_ADM_DTM,
    AP2score, AP3score) %>%
  mutate(
    AP2score = as.numeric(AP2score),
    AP3score = as.numeric(AP3score)
  )

apache_replace <- fuzzy_left_join(
  screening_log_thin, apd_extract,
  by = c(
    "UR number" = "HRN/NIH",
    "DT_start"  = "ICU_ADM_DTM",
    "DT_end"    = "ICU_ADM_DTM"
  ),
  match_fun = list(`==`, `<=`, `>=`)
) %>%
  mutate(
    AP_error   = abs(AP2score - APACHE_II) + abs(AP3score - APACHE_III),
    AP_replace = AP_error > 100 | !is.na(AP2score) & is.na(APACHE_II)  # Values arbitrarily chosen
  ) %>%
  arrange(desc(AP_replace), desc(AP_error)) %>%
  filter(AP_replace) %>%  # Only keep rows of interest
  select(`UR number`:`DateTime_ICU_admit`, AP2score:AP3score, AP_replace)

screening_log <- left_join(
  screening_log, apache_replace,
  by = c("UR number", "DateTime_ICU_admit", "Admission", "Event")
) %>%
  arrange(-AP_replace) %>%
  mutate(
    APACHE_II  = if_else(AP_replace, AP2score, APACHE_II , missing = APACHE_II),
    APACHE_III = if_else(AP_replace, AP3score, APACHE_III, missing = APACHE_III)
  ) %>%
  select(-AP2score:-AP_replace)

# TODO Add some checks here

rm(screening_log_thin, apd_extract, apache_replace)


# ---- screen_log_overview ----
screening_log %>%
  summarise(
    Admissions = n(),
    `Unique Patients` = n_distinct(`UR number`, na.rm = TRUE)
  ) %>%
  kable(., caption = "Total Admissions", booktabs = TRUE)

screening_log %>%
  group_by(Excl_criteria_ok) %>%
  # Due to multiple admissions, 1 UR could have ok on one admission and not on another
  summarise(Admissions = n()) %>%
  arrange(desc(Excl_criteria_ok)) %>%
  adorn_percentages("all") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") %>%
  kable(., caption = "Patients included and excluded", booktabs = TRUE)

screening_log %>%
  filter(Excl_criteria_ok == "Y") %>%
  select(`UR number`, starts_with("Epis")) %>%
  replace_na(list(
    Epis_cr_change = "N",
    Epis_olig = "N")
  ) %>%
  group_by(Epis_cr_change, Epis_olig) %>%
  summarise(Admissions = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = Epis_olig, values_from = Admissions) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("all") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") %>%
  adorn_title("top", row_name = "Epis_Cr", col_name = "Epis_Olig") %>%
  kable(., caption = "Creatinine change and Oliguria Epis Total Admissions")

screening_log %>%
  filter(Excl_criteria_ok == "Y") %>%  # Fine with and without
  select(`UR number`, starts_with("Total_no_")) %>%
  mutate(
    Total_no_cr_epis = if_else(
      is.na(Total_no_cr_epis), " 0 cr epis", sprintf("%2d cr epis", Total_no_cr_epis)),
    Total_no_olig_epis = if_else(
      is.na(Total_no_olig_epis), " 0 olig epis", sprintf("%2d olig epis", Total_no_olig_epis)),
  ) %>%
  group_by(Total_no_cr_epis, Total_no_olig_epis) %>%
  summarise(
    Admissions = n(),
  ) %>%
  ungroup() %>%
  pivot_wider(names_from = Total_no_olig_epis, values_from = Admissions) %>%
  adorn_totals(c("row", "col")) %>%
  rename(Epis = Total_no_cr_epis) %>%
  kable(., caption = "Creatinine change and Oliguria Episodes per Admission (Incl. criteria ok only)", booktabs = TRUE)

screening_log %>%
  filter(Excl_criteria_ok == "N") %>%
  select(`UR number`, Already_AKI:Child) %>%
  pivot_longer(-`UR number`, names_to = "Excl_reason", values_to = "Excluded") %>%
  # Single UR can have multiple exclusion reasons
  group_by(Excl_reason) %>%
  summarise(
    Admissions = sum(Excluded == "Y", na.rm = TRUE),
    `Unique Patients` = n_distinct((`UR number`[Excluded == "Y"]), na.rm = TRUE)) %>%
  arrange(-Admissions) %>%
  adorn_totals("row") %>%
  kable(., caption = "Excluded Admissions", booktabs = TRUE)

# Consider saving these and referring to them later
unique_comorbidities = unique(gsub(",", "", unlist(strsplit(paste0(screening_log$Comorbidities, collapse = ", "), ", "))))
temp <- grep("T2DM|T1DM|IDDM|insulin", unique_comorbidities, value = TRUE)
temp <- grep("AF|pAF", unique_comorbidities, value = TRUE)
temp <- grep("IHD|CABG|CAD|CAGS|NSTEMI", unique_comorbidities, value = TRUE)
temp <- grep("\\bHF\\b|hypertrophy|CCF|cardiomyopathy|heart failure|LVH", unique_comorbidities, value = TRUE)
temp <- grep("^(?=.*\\bHT\\b)(?!.*portal)(?!.*pulm)", unique_comorbidities, value = TRUE, perl = TRUE)
temp <- grep("PVD|arteritis|pop bypass|id steno|stents", unique_comorbidities, value = TRUE)
temp <- grep(paste0(
  "chronic liver disease|portal HT|varice|ETOH|",
  "HCC|NASH|CLD|ESLD|awaiting OLTx|SMV|ascites|SBP|HCC|cirrho|",
  "Hepatosplenomegaly|Cirrho|hepatic encephalopathy"), unique_comorbidities, value = TRUE)
