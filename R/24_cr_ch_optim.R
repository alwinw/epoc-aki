# ---- next ----

cr_ch_grid = expand.grid(seq(2.15, 12, by = 0.16), seq(0.5, 4, by = 0.075)) %>%
  rename(centre = Var1, width = Var2) %>%
  mutate(
    lower = centre - width/2,
    upper = centre + width/2
  )

cr_ch_list <- split(cr_ch_grid, rownames(cr_ch_grid))
cl <- makeCluster(detectCores() - 1)
invisible(clusterEvalQ(cl, library("dplyr")))
invisible(clusterEvalQ(cl, library("cutpointr")))
clusterExport(cl, c("analysis_df", "analysis_wrapper"))
cr_ch_dump <- pblapply(
  cr_ch_list, function(df) {
    data.frame(df, analysis_wrapper(
      outcome_var = "AKI_ICU",
      baseline_predictors = c(
       "Age + APACHE_II + APACHE_III + Baseline_Cr",
       "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"
      ),
      cr_predictors = "cr",
      del_t_ch_hr_range = c(df$lower, df$upper),
      del_t_aki_hr_range = c(8, 16),
      add_gradient_predictor = 1,
      heuristic_only = TRUE,
      analysis_data = analysis_df
      )
    )
  },
  cl = cl
)
stopCluster(cl)

# AUC: (0.6,0.1), (0.7, 0.5), (0.8, 0.9)
# per: (0.3, 0.1), (0.5, 0.5), (0.7, 0.9)

cr_ch_plot <- bind_rows(cr_ch_dump) %>%
  select(centre, width, AUC, n_admissions, per_admin_in) %>%
  mutate(AUC = if_else(AUC == 0, NA_real_, AUC)) %>%
  mutate(
    heuristic = (
      1/2 + 1/2*tanh(11*(AUC - 0.85 + 0.1)) + 
      1/2 + 1/2*tanh(11*(per_admin_in - 0.55 + 0.1))
    )/2
  ) %>%
  arrange(centre)
  # arrange(desc(heuristic))

cr_ch_plot %>% 
  arrange(desc(heuristic)) %>% 
  filter(centre > 4) %>% 
  head(., 50)

ggplot(cr_ch_plot, aes(centre, width)) +
  geom_contour_fill(aes(z = AUC), bins = 24, alpha = 1, na.fill = TRUE) +
  geom_contour(aes(z = n_admissions), colour = "white", binwidth = 20) +
  geom_text_contour(aes(z = n_admissions), colour = "white") +
  geom_point(shape = 1, fill = NA, colour = "white", alpha = 0.1) +
  scale_x_continuous(
    breaks = seq(0, 20, by = 1),
    limits = c(min(cr_ch_plot$centre), max(cr_ch_plot$centre)),
    expand = c(0, 0)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(name = "AUC")


ggplot(cr_ch_plot, aes(centre, width)) +
  geom_contour_fill(aes(z = heuristic), bins = 24, alpha = 1, na.fill = TRUE) +
  geom_contour(aes(z = n_admissions), colour = "white", binwidth = 20) +
  geom_text_contour(aes(z = n_admissions), colour = "white") +
  geom_point(shape = 1, fill = NA, colour = "white", alpha = 0.1) +
  scale_x_continuous(
    breaks = seq(0, 20, by = 1),
    limits = c(min(cr_ch_plot$centre), max(cr_ch_plot$centre)),
    expand = c(0, 0)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(name = "Heuristic", option = "C")





# ---- optimisation ----
optim(
  c(10, 11),
  function(x) -cr_ch_model(x[1], x[2], 12)$heuristic,
  lower = c(0, 0),
  upper = c(200, 200),
  method = "L-BFGS-B"
)
