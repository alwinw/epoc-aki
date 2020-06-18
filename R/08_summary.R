
epoc_aki %>% 
  group_by(Total_admissions) %>% 
  summarise(
    Events = n(),
    `Admissions` = n_distinct(AdmissionID),
    `Unique Patients` = n_distinct(`UR number`)
  ) %>% 
  adorn_totals("row")




# Pyramid plot for male/female
