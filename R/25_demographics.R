# ---- demographics_df ----
stopifnot(nrow(baseline_df) <= nrow(admission_data))
demographics_df <- left_join(
  baseline_df,
  admission_data
) %>%
  # TEMP ONLY
  mutate(Epis_cr = if_else(Event == "Both" | Event == "Cr change only", 1, 0))
stopifnot(nrow(demographics_df) == nrow(baseline_df))
# TODO check why there are NAs for Wt
# TODO calculate LOS in the admission_data


demographics_df %>%
  group_by(Epis_cr) %>%
  summarise(
    n = n(),
    median = median(Age),
    q1 = quantile(Age, c(0.25)),
    q3 = quantile(Age, c(0.75))
  )
wilcox.test(Age ~ Epis_cr, demographics_df)

demographics_df %>%
  group_by(Epis_cr) %>%
  summarise(
    n = n(),
    median = median(Wt, na.rm = TRUE),
    q1 = quantile(Wt, c(0.25), na.rm = TRUE),
    q3 = quantile(Wt, c(0.75), na.rm = TRUE)
  )
wilcox.test(Wt ~ Epis_cr, demographics_df)


demographics_df %>%
  group_by(Epis_cr) %>%
  summarise(
    n = n(),
    median = median(APACHE_II, na.rm = TRUE),
    q1 = quantile(APACHE_II, c(0.25), na.rm = TRUE),
    q3 = quantile(APACHE_II, c(0.75), na.rm = TRUE)
  )
wilcox.test(APACHE_II ~ Epis_cr, demographics_df)


cont_iqr <- demographics_df %>%
  select(
    AKI_2or3,
    Age, Wt, APACHE_II, APACHE_III
  ) %>% 
  pivot_longer(-AKI_2or3, names_to = "variable") %>% 
  group_by(AKI_2or3, variable) %>% 
  summarise(
    median = median(value, na.rm = TRUE),
    q1 = quantile(value, 0.25, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(text = sprintf("%.1f (%.1f - %.1f)", median, q1, q3)) %>% 
  select(AKI_2or3, variable, text) %>% 
  pivot_wider(names_from = AKI_2or3, values_from = text)

cont_pval <- demographics_df %>%
  select(
    AKI_2or3,
    Age, Wt, APACHE_II, APACHE_III
  ) %>% 
  pivot_longer(-AKI_2or3, names_to = "variable") %>% 
  group_by(variable) %>% 
  summarise(pval = wilcox.test(value ~ AKI_2or3)$p.value, .groups = "drop") %>%
  select(variable, pval)

bin_n <- demographics_df %>% 
  select(
    AKI_2or3,
    Male, Mecvenadm
  ) %>% 
  pivot_longer(-AKI_2or3, names_to = "variable") %>% 
  group_by(AKI_2or3, variable, value) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  group_by(AKI_2or3, variable) %>% 
  mutate(text = sprintf("%d (%.1f%%)", n, n/sum(n)*100)) %>% 
  filter(value == 1) %>%  # might have to check for NAs?
  select(-value, -n) %>% 
  pivot_wider(names_from = AKI_2or3, values_from = text)

bin_pval <- demographics_df %>% 
  select(
    AKI_2or3,
    Male, Mecvenadm
  ) %>% 
  pivot_longer(-AKI_2or3, names_to = "variable") %>% 
  group_by(variable) %>% 
  summarise(pval = chisq.test(AKI_2or3, value)$p.value, .groups = "drop")


