# ---- generator_function ----
gen_cr_ch_model <- function(lower, upper, step, min_hr_until_aki, max_hr_until_aki) {
  cr_ch_steps = seq(lower, upper, by = step)
  cr_ch_steps_df = data.frame(
    lower_hr_del_t_ch = head(cr_ch_steps, -1),
    upper_hr_del_t_ch = tail(cr_ch_steps, -1),
    min_hr_until_aki  = min_hr_until_aki,
    max_hr_until_aki  = max_hr_until_aki
  ) %>%
    mutate(del_t_ch_range = paste0("[", lower_hr_del_t_ch, ", ", upper_hr_del_t_ch, "]")) %>%
    rowwise() %>%
    do(data.frame(., cr_ch_model(
      c(.$lower_hr_del_t_ch, .$upper_hr_del_t_ch), c(.$min_hr_until_aki, .$max_hr_until_aki)))
    ) %>%
    ungroup()
  cr_ch_steps_df$del_t_ch_range <- factor(
    cr_ch_steps_df$del_t_ch_range, levels = cr_ch_steps_df$del_t_ch_range)

  return(cr_ch_steps_df)
}

# ---- Example_1hr_incr_spread ----
n_admin_total = round(length(unique(logit_df$AdmissionID))/100, 0)*100

cr_ch_steps = gen_cr_ch_model(
  lower = 0,
  upper = 20,
  step = 1,
  min_hr_until_aki = 8,
  max_hr_until_aki = 16
) %>%
  select(-sensitivity:-optimal_cutpoint) %>%
  mutate(heuristic = (AUC*1.1 + per_admin_in)/2.1) %>%
  pivot_longer(cols = c(AUC, per_admin_in), names_to = "names", values_to = "values") %>%
  mutate(
    labels = if_else(names == "AUC", round(values, 2), round(values*n_admin_total, 0)),
    names  = if_else(names == "AUC", "AUC", "Admissions"),
    names  = factor(names, levels = c("AUC", "Admissions"))
  )

ggplot(cr_ch_steps, aes(x = del_t_ch_range, y = values, fill = names, colour = names)) +
  geom_col(position = "dodge", alpha = 0.5, colour = NA) +
  geom_label(
    aes(label = labels),
    position = position_dodge(0.9), vjust = -0.2, fill = "white",
    show.legend = FALSE
  ) +
  geom_point(position = position_dodge(0.9)) +
  geom_point(
    aes(x = del_t_ch_range, y = heuristic, fill = "Heuristic", colour = "Heuristic"),
    data = cr_ch_steps %>% filter(names == "AUC"),
  ) +
  scale_y_continuous(
    limits = c(0, 0.95),
    breaks = seq(0, 0.9, by = 0.1),
    sec.axis = sec_axis(
      trans = ~.*n_admin_total,
      name = "Number of Included Admissions",
      breaks = seq(0, 0.9, by = 0.2)*n_admin_total)
  ) +
  ggtitle("AUC and Number of Admissions for Various \u0394t Increments") +
  xlab(expression("Duration of small change in Cr epis: "*Delta*"t"["cr_ch"]*" (hours)")) +
  ylab("AUC") +
  scale_fill_manual(name = "Legend", values=c("orange","blue", "black")) +
  scale_colour_manual(name = "Legend", values=c("orange","blue", "black")) +
  theme(legend.position="bottom")

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
clusterExport(cl, c("logit_df", "cr_ch_model"))
cr_ch_dump <- pblapply(
  cr_ch_list, function(df) {
    data.frame(df, cr_ch_model(c(df$lower, df$upper), c(8, 16)))
  },
  cl = cl
)
stopCluster(cl)

cr_ch_plot <- bind_rows(cr_ch_dump) %>%
  select(centre, width, AUC, n_admissions, per_admin_in) %>% 
  mutate(AUC = if_else(AUC == 0, NA_real_, AUC)) %>% 
  mutate(heuristic = (AUC*1.1 + per_admin_in)/2.1)


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

head(cr_ch_plot %>% arrange(desc(heuristic)), 20)



# ---- optimisation ----
optim(
  c(10, 11),
  function(x) -cr_ch_model(x[1], x[2], 12)$heuristic,
  lower = c(0, 0),
  upper = c(200, 200),
  method = "L-BFGS-B"
)
