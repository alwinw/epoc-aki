# ---- cr_ch_only_plot_df ----
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
      baseline_predictors = "",
      cr_predictors = "",
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
rm(density, centre_spacing, cr_ch_grid, cr_ch_list, cl)

heuristic_calc <- function(AUC, per_admin_in){
  return(AUC)
}

cr_ch_plot <- bind_rows(cr_ch_dump) %>%
  select(centre, width, AUC, n_admissions, per_admin_in) %>%
  mutate(AUC = if_else(AUC == 0, NA_real_, AUC)) %>%
  mutate(heuristic = heuristic_calc(AUC, per_admin_in)) %>%
  arrange(centre)

# cr_ch_plot %>%
#   arrange(desc(heuristic)) %>%
#   filter(centre > 4) %>%
#   head(., 5)


# ---- cr_ch_only_AUC_n_plot ----
ggplot(cr_ch_plot, aes(centre, width)) +
  geom_contour_fill(aes(z = AUC), bins = 32, alpha = 1, na.fill = TRUE) +
  geom_contour(aes(z = n_admissions), colour = "white", binwidth = 20) +
  geom_text_contour(aes(z = n_admissions), colour = "white") +
  scale_x_continuous(
    breaks = seq(0, 20, by = 1),
    limits = c(min(cr_ch_plot$centre), max(cr_ch_plot$centre)),
    expand = c(0, 0)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(name = "AUC") +
  ggtitle("AUC (Colour) and Number of Included Admissions (Contour Lines)")


# ---- cr_ch_only_optimisation ----
optim_one <- optim(
  c(6, 2, 8, 10),
  function(x) {
    output = analysis_wrapper(
      outcome_var = "AKI_ICU",
      baseline_predictors = "",
      cr_predictors = "",
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
  c(6.0, 1.0, 8.0, 10 ), # Baseline
  c(6.0, 2.1, 8.1, 4.3), # AUC 0.6451
  c(6.6, 2.5, 8.0, 4.0), # AUC 0.6637
  c(6.7, 1.2, 8.1, 9.6), # AUC 0.6396
  cbind(
    runif(20, 6, 7),
    runif(20, 1.5, 2.5),
    runif(20, 7.5, 8.5),
    runif(20, 4, 5)
  ),
  cbind(
    runif(50, 3, 10),
    runif(50, 1, 3),
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
      baseline_predictors = "",
      cr_predictors = "",
      del_t_ch_hr_range = c(ch_min, ch_max),
      del_t_aki_hr_range = c(aki_min, aki_max),
      add_gradient_predictor = 1,
      heuristic_only = TRUE,
      analysis_data = analysis_df
    )
    return(-heuristic_calc(output$AUC, output$per_admin_in))
  },
  lower = c(3, 0.1, 8, 3),
  upper = c(10, 3, 12, 48),
  method = "L-BFGS-B"
)
optim_cr_ch_only <- optim_out %>%
  arrange(value) %>%
  mutate(
    ch_hr_lower = p1 - p2/2,
    ch_hr_upper = p1 + p2/2,
    aki_hr_lower = p3,
    aki_hr_upper = p3 + p4
  ) %>%
  rowwise() %>%
  do(data.frame(
    .,
    analysis_wrapper(
      outcome_var = "AKI_ICU",
      baseline_predictors = "",
      cr_predictors = "",
      del_t_ch_hr_range = c(.$ch_hr_lower, .$ch_hr_upper),
      del_t_aki_hr_range = c(.$aki_hr_lower, .$aki_hr_upper),
      add_gradient_predictor = 1,
      heuristic_only = TRUE,
      analysis_data = analysis_df
    )
  )) %>%
  ungroup() %>%
  mutate(value = -value) %>%
  mutate(
    heuristic = heuristic_calc(AUC, per_admin_in)
  ) %>%
  select(-p1:-p4, -fevals:-convergence, -value)

rm(cr_ch_dump, optim_one, optim_in, optim_out)
rm(heuristic_calc)

# ---- cr_ch_only_optim_summary ----
kable(head(optim_cr_ch_only, 30))
