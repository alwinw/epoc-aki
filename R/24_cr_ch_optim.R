# ---- cr_ch_multi_plot ----

density = 4
centre_spacing = 0.15/sqrt(density)

cr_ch_grid = expand.grid(
    seq(2 + centre_spacing, 12, by = centre_spacing), 
    seq(0.5, 4, by = centre_spacing/2)
  ) %>%
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

heuristic_calc <- function(AUC, per_admin_in){
  return((1/2 + 1/2*tanh(11*(AUC - 0.875 + 0.1)) + 
        1/2 + 1/2*tanh(11*(per_admin_in - 0.525 + 0.1)))/2)
}

cr_ch_plot <- bind_rows(cr_ch_dump) %>%
  select(centre, width, AUC, n_admissions, per_admin_in) %>%
  mutate(AUC = if_else(AUC == 0, NA_real_, AUC)) %>%
  mutate(heuristic = heuristic_calc(AUC, per_admin_in)) %>%
  arrange(centre)
  # arrange(desc(heuristic))

cr_ch_plot %>%
  arrange(desc(heuristic)) %>%
  filter(centre > 4) %>%
  head(., 5)

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
  scale_fill_viridis_c(name = "AUC") +
  ggtitle("AUC (Colour) and Number of Included Admissions (Contour Lines)")


ggplot(cr_ch_plot, aes(centre, width)) +
  geom_contour_fill(aes(z = heuristic), bins = 24, alpha = 1, na.fill = TRUE) +
  geom_point(shape = 1, fill = NA, colour = "white", alpha = 0.1) +
  scale_x_continuous(
    breaks = seq(0, 20, by = 1),
    limits = c(min(cr_ch_plot$centre), max(cr_ch_plot$centre)),
    expand = c(0, 0)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(name = "Heuristic", option = "C") +
  ggtitle("Heuristic Plot")





# ---- optimisation ----
optim(
  c(6, 1.5, 8, 10),
  function(x) {
    output = analysis_wrapper(
      outcome_var = "AKI_ICU",
      baseline_predictors = c(
        "Age + APACHE_II + APACHE_III + Baseline_Cr",
        "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"
      ),
      cr_predictors = "cr",
      del_t_ch_hr_range = c(x[1] - x[2]/2, x[1] + x[2]),
      del_t_aki_hr_range = c(x[3], x[3] + x[4]),
      add_gradient_predictor = 1,
      heuristic_only = TRUE,
      analysis_data = analysis_df
    )
    return(-heuristic_calc(output$AUC, output$per_admin_in))
  },
  lower = c(3, 0.1, 8, 4),
  upper = c(10, 3, 12, 24),
  method = "L-BFGS-B"
)

set.seed(8)
optim_in = rbind(
  c(6, 1.5, 8, 10), 
  c(5, 1.5, 8, 10),
  c(6.5, 2.5, 8, 8),
  cbind(
    runif(10, 5, 8),
    runif(10, 0.1, 3),
    runif(10, 8, 10),
    runif(10, 8, 12)
  ),
  cbind(
    runif(50, 3, 10),
    runif(50, 0.1, 3),
    runif(50, 8, 12),
    runif(50, 3, 48)
  )
)
optim_out <- multistart(
  optim_in,
  function(x) {
    ch_min = x[1] - x[2]/2
    ch_max = x[1] + x[2]/2
    aki_min = x[3]
    aki_max = x[3] + x[4]
    output = analysis_wrapper(
      outcome_var = "AKI_ICU",
      baseline_predictors = c(
        "Age + APACHE_II + APACHE_III + Baseline_Cr",
        "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"
      ),
      cr_predictors = "cr",
      del_t_ch_hr_range = c(ch_min, ch_max),
      del_t_aki_hr_range = c(aki_min, aki_max),
      add_gradient_predictor = 1,
      heuristic_only = TRUE,
      analysis_data = analysis_df
    )
    return(-heuristic_calc(output$AUC, output$per_admin_in))
  },
  lower = c(3, 0.1, 8, 4),
  upper = c(10, 3, 12, 48),
  method = "L-BFGS-B"
)
optim_summary <- optim_out %>% 
  arrange(value) %>% 
  rowwise() %>%
  do(data.frame(
    .,
    analysis_wrapper(
      outcome_var = "AKI_ICU",
      baseline_predictors = c("Age + APACHE_II + APACHE_III + Baseline_Cr",
                              "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"),
      cr_predictors = "cr",
      del_t_ch_hr_range = c(.$p1, .$p2),
      del_t_aki_hr_range = c(.$p3, .$p4),
      add_gradient_predictor = 1,
      heuristic_only = TRUE,
      analysis_data = analysis_df
    )
  )) %>% 
  mutate(value = -value) %>% 
  ungroup()
