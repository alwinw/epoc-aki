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

outcome_var <- "AKI_2or3"

number <- demographics_df %>%
  select({{ outcome_var }}) %>%
  group_by(.data[[outcome_var]]) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = {{ outcome_var }}, values_from = n) %>%
  mutate(variable = "number", all = nrow(demographics_df), pval = NA)

cont_vars <- c(
  "Age", "Wt", "APACHE_II", "APACHE_II"
)

cont_all <- demographics_df %>%
  select(all_of(cont_vars)) %>%
  pivot_longer(everything(), names_to = "variable") %>%
  group_by(variable) %>%
  summarise(
    all = sprintf(
      "%.1f (%.1f - %.1f)",
      median(value, na.rm = TRUE),
      quantile(value, 0.25, na.rm = TRUE),
      quantile(value, 0.75, na.rm = TRUE)
    ),
    .groups = "drop"
  )

cont_iqr <- demographics_df %>%
  select({{ outcome_var }}, all_of(cont_vars)) %>%
  pivot_longer(-{{ outcome_var }}, names_to = "variable") %>%
  group_by(.data[[outcome_var]], variable) %>%
  summarise(
    text = sprintf(
      "%.1f (%.1f - %.1f)",
      median(value, na.rm = TRUE),
      quantile(value, 0.25, na.rm = TRUE),
      quantile(value, 0.75, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = AKI_2or3, values_from = text)

cont_pval <- demographics_df %>%
  select({{ outcome_var }}, all_of(cont_vars)) %>%
  pivot_longer(-{{ outcome_var }}, names_to = "variable") %>%
  group_by(variable) %>%
  summarise(pval = wilcox.test(value ~ AKI_2or3)$p.value, .groups = "drop") %>% # FIXME hard coding
  select(variable, pval)

bin_vars <- c(
  "AKI_2or3",
  "Male", "Mecvenadm",
  "Surgadmission"
)

bin_n <- demographics_df %>%
  select(all_of(bin_vars)) %>%
  pivot_longer(-{{ outcome_var }}, names_to = "variable") %>%
  group_by(.data[[outcome_var]], variable, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(.data[[outcome_var]], variable) %>%
  mutate(text = sprintf("%d (%.1f%%)", n, n / sum(n) * 100)) %>%
  filter(value == 1) %>% # might have to check for NAs?
  group_by(variable) %>%
  mutate(all = sum(n)) %>% # FIXME needs percentage as well
  select(-value, -n) %>%
  pivot_wider(names_from = {{ outcome_var }}, values_from = text)

bin_pval <- demographics_df %>%
  select(
    AKI_2or3,
    Male, Mecvenadm,
    Surgadmission
  ) %>%
  pivot_longer(-AKI_2or3, names_to = "variable") %>%
  group_by(variable) %>%
  summarise(pval = chisq.test(AKI_2or3, value)$p.value, .groups = "drop")

demographics_table <- rbind(
  number,
  left_join(left_join(cont_iqr, cont_all), cont_pval),
  left_join(bin_n, bin_pval)
) %>%
  select(variable, all, {{ outcome_var }} := `1`, "No_{{outcome_var}}" := `0`, pval)

kable(demographics_table)
