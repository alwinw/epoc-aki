# ---- generate_cr_ch_function ----
generate_cr_ch <- function(
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
    select(Pathology_Result_DTTM, Creatinine_level) %>%
    unique(.)  # To remove any duplicate DTTM entries

  if (nrow(cr_ts) < 2) {
    return(data.frame(
      DateTime_Pathology_Result = as_datetime(NA_real_),
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

  if(AKI_ICU == 0 | is.na(AKI_ICU)) {
    del_t_aki = rep(as.duration(NA_real_), nrow(Ti))
  } else {
    del_t_aki = as.duration(DateTime_AKI_Dx - Ti$Pathology_Result_DTTM)
  }

  return(data.frame(
    DateTime_Pathology_Result = Ti$Pathology_Result_DTTM,
    del_t_ch  = as.duration(Ti$Pathology_Result_DTTM - Ti_1$Pathology_Result_DTTM),
    del_t_aki = del_t_aki,
    del_cr    = Ti$Creatinine_level - Ti_1$Creatinine_level,
    cr_i      = Ti$Creatinine_level
  ))
}


# ---- generate_cr_changes ----
cr_ch_ts_all <- admission_data %>%
  filter(
    Excl_criteria_ok == 1,
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
  do(data.frame(., generate_cr_ch(
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

neither_ts <- cr_ch_ts_all %>%
  filter(is.na(AKI_ICU)) %>%
  filter(!is.na(cr_i)) %>%  # Only one measurement in ICU
  group_by(AdmissionID) %>%
  mutate(
    # No need to check for olig definition of AKI, else would have had "oliguria episode"
    Baseline_Cr = min(cr_i),
    AKI_ICU = if_else(cr_i > Baseline_Cr*1.5, 1, 0),  # FIXME NEED TO PROGRAM IN THE RISE CASE TOO
    AKI_stage = case_when(                            # TODO Add other variables too!
      cr_i > Baseline_Cr*3   ~ 3,
      cr_i > Baseline_Cr*2   ~ 2,
      cr_i > Baseline_Cr*1.5 ~ 1,
      TRUE ~ 0),
    Max_Cr_ICU = max(cr_i),
  ) %>%
  arrange(AdmissionID, desc(cr_i)) %>%
  mutate(
    Max_Cr_DateTime = first(DateTime_Pathology_Result)
  ) %>%
  arrange(AdmissionID, desc(AKI_ICU), DateTime_Pathology_Result) %>%
  mutate(
    DateTime_AKI_Dx = if_else(first(AKI_ICU) == 1, first(DateTime_Pathology_Result), as_datetime(NA_real_))
  ) %>%
  arrange(AdmissionID, DateTime_Pathology_Result) %>%
  mutate(
    # Must be done AFTER arrange() and mutate()
    AKI_ICU   = max(AKI_ICU, na.rm = TRUE),
    AKI_stage = max(AKI_stage, na.rm = TRUE),
    AKI_stage = if_else(AKI_stage == 0, NA_real_, AKI_stage),
    del_t_aki = if_else(AKI_ICU == 1, as.duration(DateTime_AKI_Dx - DateTime_Pathology_Result), as.duration(NA_real_))
  ) %>%
  ungroup()
# length(unique(neither_ts$AdmissionID))

cr_ch_ts <- rbind(
  cr_ch_ts_all %>% filter(!is.na(AKI_ICU)),
  neither_ts
)

# TODO Add checks on number of rows, etc

rm(cr_ch_ts_all, neither_ts, generate_cr_ch)


# ---- cr_changes_overview ----
cr_ch_ts %>%
  filter(abs(del_cr) < 50) %>%
  mutate(
    t_AKI = if_else(is.na(del_t_aki) | del_t_aki > 0, "Before", "After")
  ) %>%
  select(AdmissionID, AKI_ICU, DateTime_Pathology_Result, t_AKI) %>%
  # unique(.) %>%
  group_by(AKI_ICU, t_AKI) %>%
  summarise(
    cr_measurements = n_distinct(DateTime_Pathology_Result, na.rm = TRUE),
    cr_ch_epis = n(),
    .groups = "drop"
  ) %>%
  arrange(AKI_ICU, desc(t_AKI)) %>%
  adorn_totals() %>%
  kable(., caption = "All cr measurements (measurement errors included)")


# ---- summary_plots ----
# ggplot(cr_ch_ts, aes(x = del_t_ch/3600)) +
#   geom_histogram(bins = 100, fill = "cyan", colour = "blue") +
#   xlim(0, 48)
# # Add another plot based on admissions?
#
# ggplot(cr_ch_ts, aes(x = del_cr)) +
#   geom_histogram(bins = 50, fill = "cyan", colour = "blue") +
#   xlim(0, 100)
#
# ggplot(cr_ch_ts, aes(x = del_t_ch/3600, y = del_cr)) +
#   geom_hex(bins = 100) +
#   xlim(0, 48) + ylim(-100, 100) +
#   coord_cartesian(expand = FALSE) +
#   scale_fill_viridis_c()

# ---- heatmap_plot ----
heatmap_all <- cr_ch_ts %>%
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
  geom_density_2d_filled(
    aes(fill = after_stat(level_mid)),
    contour_var = "density"
  ) +
  scale_x_continuous(breaks = seq(0, 12, by = 2)) +
  coord_cartesian(xlim = c(0, 12), ylim = c(-25, 30), expand = FALSE) +
  facet_wrap(~heatmap) +
  scale_fill_viridis_c("Density", option = "D") +
  geom_hline(yintercept = 0, colour = "white", linetype = "solid") +
  geom_vline(xintercept = seq(0, 16, by = 4), colour = "white", linetype = "dotted") +
  geom_text(
    data = heatmap_count,
    aes(x = 0.2, y = -23, label = paste0("n(Admissions): ", n_admission, "\nn(Cr_ch epis): ", n_cr)),
    colour = "white", hjust = 0, vjust = 0
  ) +
  ggtitle("Distribution of Short-term Creatinine Change Episodes (Cr_ch)") +
  xlab(expression("Duration of short-term Cr change epis: "*Delta*"t"["cr_ch"]*" (hours)")) +
  ylab(expression("Change in Cr during epis: "*Delta*"cr"*" ("*mu*"mol/L)")) +
  theme(panel.spacing = unit(0.8, "lines"))
print(heatmap_plot)

ggsave("cr_ch_heatmap.png", heatmap_plot, path = paste0(rel_path, "/doc/images/"),
       width = 12, height = 8, scale = 0.8)
