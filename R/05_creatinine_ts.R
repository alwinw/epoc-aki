# ---- combine_blood_gas_bio_chem ----
UR_number_list <- unique(filter(admission_data, Event != "Neither")$`UR number`)
blood_gas_adjust = 2  # FIXME Estimated!! Would need something that matches the mean AND the variance

blood_gas_ts <- xlsx_data$creat_furo$blood_gas %>%
  select(`UR number`, TC_ICU_ADMISSION_DTTM, TC_ICU_DISCHARGE_DTTM, Pathology_Result_DTTM,
         `Blood Gas Creatinine`) %>%
  mutate(Pathology = "Blood Gas", `Bio Chem Creatinine` = NA)
bio_chem_ts <- xlsx_data$creat_furo$bio_chem %>%
  select(`UR number`, TC_ICU_ADMISSION_DTTM, TC_ICU_DISCHARGE_DTTM, Pathology_Result_DTTM,
         `Bio Chem Creatinine` = `Creatinine Level`) %>%
  mutate(Pathology = "Bio Chem", `Blood Gas Creatinine` = NA)

creatinine_ts <- rbind(blood_gas_ts, bio_chem_ts) %>%
  arrange(`UR number`, TC_ICU_ADMISSION_DTTM, Pathology_Result_DTTM) %>%
  filter(
    `UR number` %in% UR_number_list,
    Pathology_Result_DTTM > as.Date('2018-01-01')
  ) %>%
  mutate(
    Creatinine_level = if_else(Pathology == "Bio Chem", `Bio Chem Creatinine`, `Blood Gas Creatinine` + blood_gas_adjust)
  ) %>%
  filter(!is.na(Creatinine_level)) %>%
  mutate_at(
    vars(ends_with("DTTM")),
    force_tz,
    tzone = "Australia/Melbourne"
  ) %>%
  group_by(`UR number`) %>%
  mutate(ICU_Admission = cumsum(TC_ICU_ADMISSION_DTTM != lag(TC_ICU_ADMISSION_DTTM, default = as.POSIXct("1990-01-01")))) %>%  # Arbitrarily chosen
  arrange(`UR number`, Pathology_Result_DTTM)

rm(blood_gas_ts, bio_chem_ts, UR_number_list)

# ---- example_creatinine_plot ----
UR_number_list = creatinine_ts %>% arrange(-ICU_Admission) %>% select(`UR number`) %>% unique(.)
UR_number = UR_number_list[3,]

ggplot(
  filter(creatinine_ts, `UR number` == UR_number) %>% mutate(ICU_Admission = paste0("ICU Admission ", ICU_Admission)),
  aes(x = Pathology_Result_DTTM,
      y = Creatinine_level,
      group = ICU_Admission,
  )
) +
  geom_line(linetype = "dashed", colour = "grey") +
  geom_point(aes(colour = Pathology)) +
  facet_wrap(vars(ICU_Admission), nrow = 1, scales = "free_x")

rm(UR_number, UR_number_list)

# ---- plot_blood_gas_vs_bio_chem ----
bio_chem_blood_gas <- creatinine_ts %>%
  select(-TC_ICU_ADMISSION_DTTM, -TC_ICU_DISCHARGE_DTTM, -`Blood Gas Creatinine`, -`Bio Chem Creatinine`) %>%
  group_by(`UR number`, ICU_Admission) %>%
  mutate(
    Delta_t = as.double(Pathology_Result_DTTM - lag(Pathology_Result_DTTM, default = as_datetime(0)), units = "mins"),
    Delta_in = Delta_t < 45
  ) %>%
  filter(Delta_in | lead(Delta_in) & !is.na(Delta_in)) %>%
  mutate(
    Delta_cr = if_else(Delta_in, Creatinine_level - lag(Creatinine_level), 0),
    Pathology_first = if_else(Delta_in, lag(Pathology), ""),
    Pathology_last = if_else(Delta_in, Pathology, ""),
    Creatinine_first = if_else(Delta_in, lag(Creatinine_level), 0),
    Creatinine_last = if_else(Delta_in, Creatinine_level, 0),
    Pathology_first_DTTM = lag(Pathology_Result_DTTM),
    Pathology_type = paste0("First Cr reading: ", Pathology_first, ", Second Cr reading: ", Pathology_last)
  ) %>%
  filter(Delta_in) %>%
  select(
    `UR number`, ICU_Admission, Pathology_first_DTTM, Delta_t, Pathology_type,
    Pathology_first, Creatinine_first,
    Pathology_last, Creatinine_last,
    Delta_cr) %>%
  filter(abs(Delta_cr) < 30)

ggplot(bio_chem_blood_gas, aes(x = Delta_t, y = Delta_cr, colour = Pathology_type)) +
  geom_hline(yintercept = c(-10, 10), linetype = "dashed", colour = "grey") +
  geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  facet_wrap(vars(Pathology_type), ncol = 1) +
  theme(legend.position="bottom") +
  guides(colour = guide_legend(ncol = 1))

rm(blood_gas_adjust)

# ---- aki_cr_ch_fun ----
aki_cr_ch <- function(
  UR_number, DateTime_ICU_admit, DateTime_ICU_dc,
  AKI_ICU, DateTime_AKI_Dx)
  {
  cr_ts = creatinine_ts %>%
    ungroup() %>%
    filter(
      `UR number` == UR_number,
      Pathology_Result_DTTM > DateTime_ICU_admit,
      Pathology_Result_DTTM < DateTime_ICU_dc
    ) %>%
    select(Pathology_Result_DTTM, Creatinine_level)

  if (nrow(cr_ts) < 2) {
    return(data.frame(
      del_t_ch  = as.duration(NA_real_),
      del_t_aki = as.duration(NA_real_),
      del_cr    = NA_real_,
      cr_i      = NA_real_
    ))
  }
  # Consider filtering out ones post AKI here?

  combns <- combn(nrow(cr_ts), 2)
  Ti_1 = cr_ts[combns[1,],]
  Ti   = cr_ts[combns[2,],]

  if(AKI_ICU == 1) {
    del_t_aki = as.duration(DateTime_AKI_Dx - Ti$Pathology_Result_DTTM)
  } else {
    del_t_aki = rep(as.duration(NA_real_), nrow(Ti))
  }

  return(data.frame(
    del_t_ch  = as.duration(Ti$Pathology_Result_DTTM - Ti_1$Pathology_Result_DTTM),
    del_t_aki = del_t_aki,
    del_cr    = Ti$Creatinine_level - Ti_1$Creatinine_level,
    cr_i      = Ti$Creatinine_level
  ))
}

# ---- admission_ts ----

admission_ts <- admission_data %>%
  filter(
    Excl_criteria_ok == 1,
    !is.na(AKI_ICU)  # TODO Remove this restriction later as we can calculate AKI from the Cr data!!!
  ) %>%
  mutate(
    DateTime_ICU_dc = Date_ICU_dc + hours(23) + minutes(59) + seconds(59)
  ) %>%
  select(
    `UR number`:Admission, Pt_Study_nos, Event, Excl_criteria_ok,
    Age, APACHE_II, APACHE_III, Baseline_Cr, PCs_cardio, Vasopressor:Chronic_liver_disease,
    AKI_ICU, AKI_stage,
    DateTime_ICU_admit, DateTime_ICU_dc,
    Baseline_Cr:Cr_defined_AKI_stage
  ) %>%
  rowwise() %>%
  do(data.frame(., aki_cr_ch(
    .$`UR number`, .$DateTime_ICU_admit, .$DateTime_ICU_dc, .$AKI_ICU, .$DateTime_AKI_Dx))
  ) %>%
  ungroup() %>%
  rename(
    `UR number` = UR.number,
    `Highest Cr UEC` = Highest.Cr.UEC,
    `AKI Dx Cr 26.5` = AKI.Dx.Cr.26.5,
    `AKI Dx Cr 1.5 times` = AKI.Dx.Cr.1.5.times,
    `AKI Dx oliguria` = AKI.Dx.oliguria,
    `Criteria for stage of AKI` = Criteria.for.stage.of.AKI
  )

rm(bio_chem_blood_gas, creatinine_ts)

# ---- summary_plots ----
ggplot(admission_ts, aes(x = del_t_ch/3600)) +
  geom_histogram(bins = 100, fill = "cyan", colour = "blue") +
  xlim(0, 48)
# Add another plot based on admissions?

ggplot(admission_ts, aes(x = del_cr)) +
  geom_histogram(bins = 50, fill = "cyan", colour = "blue") +
  xlim(0, 100)

# ggplot(admission_ts, aes(x = log(del_t_ch/3600), y = del_cr)) +
#   geom_hex()

ggplot(admission_ts, aes(x = del_t_ch/3600, y = del_cr)) +
  geom_hex(bins = 100) +
  xlim(0, 48) + ylim(-100, 100) +
  coord_cartesian(expand = FALSE) +
  scale_fill_viridis_c()

# ---- heatmap_plot ----
heatmap_all <- admission_ts %>%
  filter(is.na(del_t_aki) | del_t_aki > 0) %>%
  mutate(
    heatmap = case_when(
      is.na(del_t_aki)    ~ " No AKI",
      del_t_aki/3600 <  4 ~ "t_AKI in  0-4hrs",
      del_t_aki/3600 <  8 ~ "t_AKI in  4-8hrs",
      del_t_aki/3600 < 12 ~ "t_AKI in  8-12hrs",
      del_t_aki/3600 < 16 ~ "t_AKI in 12-16hrs",
      TRUE                ~ "T_AKI in 16+hrs"
    ),
  )


heatmap_count <- heatmap_all %>%
  group_by(heatmap) %>%
  summarise(n_cr = n(), n_admission = n_distinct(AdmissionID), .groups = "keep")

heatmap_ts <- heatmap_all %>%
  filter(del_t_ch/3600 < 13, abs(del_cr) < 50)

heatmap_plot <- ggplot(heatmap_ts, aes(x = del_t_ch/3600, y = del_cr)) +
  geom_density_2d_filled(contour_var = "density") + #, breaks = c(0, round(1.4^seq(1:11),0)/50, 1)) + #= c(seq(0, 10), 20, 30, 40, 50, 60)) + #, bins = 20) +
  scale_x_continuous(breaks = seq(0, 12, by = 2)) +
  # geom_point(alpha = 0.1, shape = 21, fill = NA, colour = "white", size = 0.9) +
  # ylim(-30, 30) +
  coord_cartesian(xlim = c(0, 12), ylim = c(-25, 30), expand = FALSE) +
  facet_wrap(~heatmap) +
  scale_fill_viridis_d("Density") +
  geom_hline(yintercept = 0, colour = "white", linetype = "solid") +
  geom_vline(xintercept = seq(0, 16, by = 4), colour = "white", linetype = "dotted") +
  # theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE, panel.grid.minor = element_line(colour = NA)) +
  geom_text(
    data = heatmap_count,
    aes(x = 0.2, y = -23, label = paste0("n(Admissions): ", n_admission, "\nn(cr_ch events): ", n_cr)),
    colour = "white", hjust = 0, vjust = 0
  ) +
  ggtitle("Creatinine Changes") +
  xlab(expression("Duration of small change in Cr epis: "*Delta*"t"["cr_ch"]*" (hours)")) +
  ylab(expression("Change in Cr during epis: "*Delta*"cr"*" ("*mu*"mol/L)")) +
  theme(panel.spacing = unit(0.8, "lines"))

print(heatmap_plot)

ggsave("cr_ch_heatmap.png", heatmap_plot, path = paste0(rel_path, "/doc/images/"),
       width = 12, height = 8, scale = 0.8)
