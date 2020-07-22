

# ---- generator_function ----
gen_cr_ch_model <- function(lower, upper, step, hr_before_aki) {
  cr_ch_steps = seq(lower, upper, by = step)
  cr_ch_steps_df = data.frame(
    lower_hr_del_t_ch = head(cr_ch_steps, -1),
    upper_hr_del_t_ch = tail(cr_ch_steps, -1),
    hr_before_aki     = hr_before_aki
  ) %>%
    mutate(del_t_ch_range = paste0("[", lower_hr_del_t_ch, ", ", upper_hr_del_t_ch, "]")) %>%
    rowwise() %>%
    do(data.frame(., cr_ch_model(.$lower_hr_del_t_ch, .$upper_hr_del_t_ch, .$hr_before_aki))) %>%
    ungroup() %>%
    arrange(desc(heuristic), desc(AUC))

  return(cr_ch_steps_df)
}

# ---- example_models ----
cr_ch_steps = gen_cr_ch_model(0, 20, 1, 12)

ggplot(cr_ch_steps, aes())




plot_cr_ch = expand.grid(seq(0, 30, by = 0.1), seq(0, 3, by = 0.1)) %>%
  rename(centre = Var1, tol = Var2) %>%
  mutate(
    lower = centre - tol,
    upper = centre + tol) %>%
  filter(upper > lower, lower > 0) %>%
  rowwise() %>%
  do(data.frame(., cr_ch_model(.$lower, .$upper, 12))) %>%
  ungroup()

# Consider parallising above^
# cluster <- new_cluster(4)
# cluster_assign(cluster, logit_df = logit_df, cr_ch_model = cr_ch_model)
# partition(cluster)

head(plot_cr_ch %>% arrange(desc(heuristic), desc(AUC)))
head(plot_cr_ch %>% mutate(heuristic = (AUC*3 + (n_admissions/313))) %>% arrange(desc(heuristic), desc(AUC)), 20)

ggplot(plot_cr_ch %>% filter(AUC > 0), aes(centre, tol, fill = AUC)) +
  geom_tile() +
  geom_contour(aes(z = AUC, colour = after_stat(level)),
               binwidth = 0.01) + #, colour = after_stat(level))) +
  coord_fixed() +
  scale_fill_viridis_c() +
  scale_colour_viridis_c()

ggplot(plot_cr_ch %>% filter(AUC > 0), aes(centre, tol, fill = heuristic)) +
  geom_tile() +
  geom_contour(aes(z = heuristic), colour = "white", binwidth = 0.025) + #, colour = after_stat(level))) +
  coord_fixed() +
  scale_fill_viridis_c() +
  scale_colour_viridis_c()

ggplot(plot_cr_ch %>% filter(AUC > 0), aes(centre, tol, fill = n_admissions)) +
  geom_tile() +
  geom_contour(aes(z = n_admissions), colour = "white", binwidth = 20) + #, colour = after_stat(level))) +
  geom_text_contour(aes(z = n_admissions), colour = "white") +
  coord_fixed() +
  scale_fill_viridis_c() +
  scale_colour_viridis_c()

ggplot(plot_cr_ch %>% filter(AUC > 0), aes(centre, tol)) +
  geom_tile(aes(fill = AUC)) +
  geom_contour(aes(z = n_admissions), colour = "white", binwidth = 20) + #, colour = after_stat(level))) +
  geom_text_contour(aes(z = n_admissions), colour = "white") +
  # geom_text_contour(aes(z = AUC), colour = "white") +
  coord_fixed() +
  scale_fill_viridis_c() +
  scale_colour_viridis_c()

# ---- optimisation ----
optim(
  c(10, 11),
  function(x) -cr_ch_model(x[1], x[2], 12)$heuristic,
  lower = c(0, 0),
  upper = c(200, 200),
  method = "L-BFGS-B"
)
