# ---- check-merge-data-fun ----
check_merge_data <- function(vector1, vector2, error_msg) {
  if (!setequal(vector1, vector2) | (length(vector1) != length(vector2))) {
    stop(paste("Error", error_msg, 
               "Actual:", length(vector1), "Expected:", length(vector2),
               "Difference:", paste(setdiff(vector1, vector2), collapse = ", ")
    ))
  }
  return(invisible(NULL))
}


# ---- merge-obs-data ----
screening_data <- screening_log %>% 
  pivot_longer(
    starts_with("Pt_Study_no"), 
    names_to = "Pt_Study_no_type",
    values_to = "Pt_Study_no",
  ) %>% 
  select(-Pt_Study_no_type) %>% 
  distinct() %>% 
  mutate(
    duplicate = is.na(Pt_Study_no) & (Event == 1 | Event == 2)
  ) %>% 
  filter(!duplicate) %>% 
  mutate(
    Epis_cr_change = ifelse(grepl("LT[0-9]", Pt_Study_no), "Y", NA),
    Epis_olig      = ifelse(grepl("L[0-9]",  Pt_Study_no), "Y", NA)
  ) %>% 
  select(`UR number`:Pt_Study_no, Dates_screened, Event, starts_with("Epis_"))

# Check screening_data (pivot longered) with screening_log (original)
check_merge_data(
  grep("L[0-9]", screening_data$Pt_Study_no, value = TRUE), 
  as.vector(na.omit(screening_log$Pt_Study_no_olig)),
  "Number of olig events different!"
)
check_merge_data(
  grep("LT[0-9]", screening_data$Pt_Study_no, value = TRUE), 
  as.vector(na.omit(screening_log$Pt_Study_no_crch)),
  "Number of cr change events different!"
)
check_merge_data(
  filter(screening_data, is.na(Pt_Study_no))$`UR number`, 
  filter(screening_log, Event == 0)$`UR number`,
  "Number of neither olig or cr change events different!"
)
check_merge_data(
  screening_data$`UR number`,
  c(filter(screening_log, Event != 3)$`UR number`, rep(filter(screening_log, Event == 3)$`UR number`, 2)),
  "Total number of events has changed!"
)

# Join in the data_set
obs_data <- full_join(
  screening_data, 
  data_set,
  by = c("Pt_Study_no", "Epis_cr_change", "Epis_olig")
)


check_merge_data(
  grep("L[0-9]", obs_data$Pt_Study_no, value = TRUE), 
  grep("L[0-9]", data_set$Pt_Study_no, value = TRUE),
  "Number of olig events different!"
)
check_merge_data(
  grep("LT[0-9]", obs_data$Pt_Study_no, value = TRUE), 
  grep("LT[0-9]", data_set$Pt_Study_no, value = TRUE),
  "Number of cr change events different!"
)
check_merge_data(
  filter(obs_data, Event == 0)$`UR number`,
  filter(screening_data, Event == 0)$`UR number`,
  "Number of neither olig or cr change events different!"
)

if (nrow(obs_data) != nrow(data_set) + nrow(filter(screening_log, Event == 0))){
  stop("Number of total events has changed!")
}

# ---- add-time-series ----

# ---- add-outcomes ----

# Various outcomes here
# Check individual columns next...
# Sort columns by UR, PTSn, Admission, Event Type, etc

colnames(obs_data)

epoc_aki <- obs_data %>% 
  mutate(AdmissionID = paste(`UR number`, Admission, sep = ".")) %>% 
  group_by(`UR number`) %>% 
  group_by(Pt_Study_no) %>% 
  mutate(
    Epis_cr_change_no = cumsum(Epis_cr_change == "Y"),
    Epis_olig_no      = cumsum(Epis_olig      == "Y")
  ) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(Comment = paste(unique(na.omit(c(Comment_crch, Comment_olig, Comment_out, Comment))), collapse = ", ")) %>% 
  ungroup() %>% 
  select(
    # PT INFO
    `UR number`, 
    Admission, Total_admissions, AdmissionID,
    DateTime_hosp_admit:Dc_destination,
    # EPIS
    Pt_Study_no, Event,
    Incl_criteria_ok_crch, Incl_criteria_ok_olig, Excl_criteria_ok,
    Epis_cr_change, Epis_olig, 
    Epis_cr_change_no, Epis_olig_no, Epis_no,
    Total_no_cr_epis, Total_no_olig_epis,
    # SCREENING LOG
    APACHE_II:APACHE_III,
    Already_AKI:Mecvenadm,
    # DATA SET
    DateTime_epis:Cause_death,
    # COMMENTS
    Dates_screened,
    Comment
  ) %>% 
  arrange(AdmissionID)

glimpse(epoc_aki)
View(epoc_aki)

setdiff(colnames(obs_data), colnames(epoc_aki))
