Ls  = as.vector(na.omit(screening_log$Pt_Study_no_olig))
LTs = as.vector(na.omit(screening_log$Pt_Study_no_crch))

# --- Merge Observation Data ----
screening_data <- screening_log %>% 
  pivot_longer(
    starts_with("Pt_Study_no"), 
    names_to = "Pt_Study_no_type",
    values_to = "Pt_Study_no",
  ) %>% 
  select(`UR number`, Pt_Study_no, Incl_criteria_ok_crch:Comment_out) %>% 
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

nrow(filter(screening_data, grepl("L[0-9]",  Pt_Study_no)))
nrow(filter(screening_data, grepl("LT[0-9]", Pt_Study_no)))
nrow(filter(screening_data, is.na(Pt_Study_no)))


nrow(filter(screening_log, Event != 3)) +
nrow(filter(screening_log, Event == 3)) * 2

setdiff(data_set$Pt_Study_no, c(Ls, LTs))

obs_data <- full_join(
  screening_data, 
  data_set %>% select(Pt_Study_no, Epis_no, starts_with("Epis_")),
  by = c("Pt_Study_no", "Epis_cr_change", "Epis_olig")
)

nrow(obs_data)
nrow(filter(obs_data, Event == 0))
nrow(filter(screening_data, Event == 0))
nrow(data_set)

nrow(filter(obs_data, grepl("L[0-9]",  Pt_Study_no)))
nrow(filter(data_set, grepl("L[0-9]",  Pt_Study_no)))

nrow(filter(obs_data, grepl("LT[0-9]", Pt_Study_no)))

# Issue with excluded UR numbers...
