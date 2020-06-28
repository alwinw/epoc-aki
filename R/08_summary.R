
epoc_aki_admissions <- epoc_aki %>% 
  group_by(AdmissionID) %>% 
  filter(row_number()==n()) %>%  # Consider a better method
  ungroup()



epoc_aki %>% 
  group_by(Total_admissions) %>% 
  summarise(
    Events = n(),
    `Admissions` = n_distinct(AdmissionID),
    `Unique Patients` = n_distinct(`UR number`)
  ) %>% 
  adorn_totals("row")

# Included vs excluded


# Admissions vs Cr
# Admissions vs olig

# Should have just used the screening log here
# Cr vs olig
epoc_aki_admissions %>% 
  filter(Excl_criteria_ok == 1) %>%  # TODO If no filter, there is an 'extra' one
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
  rename(Epis = Total_no_cr_epis)
# Turn this into a heat map with marginal distribution plot on the side


# Pyramid plot for male/female
