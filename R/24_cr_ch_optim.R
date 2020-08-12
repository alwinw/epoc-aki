# ---- next ----

cr_ch_grid = expand.grid(seq(1.75, 12, by = 0.1), seq(0.2, 3, by = 0.05)) %>%
  rename(centre = Var1, width = Var2) %>%
  mutate(
    lower = centre - width/2,
    upper = centre + width/2
  )

cr_ch_list <- split(cr_ch_grid, rownames(cr_ch_grid))
cl <- makeCluster(detectCores() - 1)
invisible(clusterEvalQ(cl, library("dplyr")))
invisible(clusterEvalQ(cl, library("cutpointr")))
clusterExport(cl, c("logit_df", "cr_ch_binary_model"))
cr_ch_dump <- pblapply(
  cr_ch_list, function(df) {
    data.frame(df, cr_ch_binary_model(c(df$lower, df$upper), c(8, 16), 1))
  },
  cl = cl
)
stopCluster(cl)

AUCwt = 1

cr_ch_plot <- bind_rows(cr_ch_dump) %>%
  select(centre, width, AUC, n_admissions, per_admin_in) %>%
  mutate(AUC = if_else(AUC == 0, NA_real_, AUC)) %>%
  mutate(heuristic = (AUC*AUCwt + per_admin_in^0.5)/(AUCwt+1)) %>%
  arrange(centre)
  # arrange(desc(heuristic))

head(cr_ch_plot, 50)

ggplot(cr_ch_plot, aes(centre, width)) +
  # geom_raster(aes(fill = AUC)) +
  geom_contour_fill(aes(z = AUC), bins = 24, alpha = 1, na.fill = TRUE) +
  geom_contour(aes(z = n_admissions), colour = "white", binwidth = 20) +
  geom_text_contour(aes(z = n_admissions), colour = "white") +
  scale_x_continuous(
    breaks = seq(0, 20, by = 1),
    limits = c(min(cr_ch_plot$centre), max(cr_ch_plot$centre)),
    expand = c(0, 0)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(name = "AUC")

ggplot(cr_ch_plot, aes(centre, width)) +
  geom_contour_fill(aes(z = AUC), bins = 24, alpha = 1, na.fill = TRUE) +
  geom_contour(aes(z = n_admissions), colour = "white", binwidth = 20) +
  geom_text_contour(aes(z = n_admissions), colour = "white") +
  scale_x_continuous(
    breaks = seq(0, 20, by = 1),
    limits = c(5, 7),
    expand = c(0, 0)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(name = "AUC")

ggplot(cr_ch_plot, aes(centre, width)) +
  geom_contour_fill(aes(z = heuristic), alpha = 0.8, na.fill = TRUE) +
  # geom_contour(aes(z = n_admissions), colour = "white", binwidth = 20) +
  # geom_text_contour(aes(z = n_admissions), colour = "white") +
  geom_contour(aes(z = AUC, colour = after_stat(level)), bins = 16) +
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
