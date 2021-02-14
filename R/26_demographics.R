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
  summarise(text = sprintf("%d (%.1f%%)", n(), n() / nrow(.) * 100), .groups = "drop") %>%
  pivot_wider(names_from = {{ outcome_var }}, values_from = text) %>%
  mutate(
    variable = "Admissions",
    all = sprintf("%d (%.1f%%)", nrow(demographics_df), 100),
    pval = NA
  )

cont_df <- demographics_df %>%
  select(
    {{ outcome_var }},
    Age, Wt, APACHE_II, APACHE_III, Baseline_Cr,
    LOS_ICU_hr, LOS_Hosp_hr
  ) %>%
  pivot_longer(-{{ outcome_var }}, names_to = "variable")
cont_all <- cont_df %>%
  select(-{{ outcome_var }}) %>%
  group_by(variable) %>%
  summarise(
    all = sprintf(
      "%.1f (%.1f - %.1f)", median(value, na.rm = TRUE),
      quantile(value, 0.25, na.rm = TRUE), quantile(value, 0.75, na.rm = TRUE)
    ), .groups = "drop"
  )
cont_iqr <- cont_df %>%
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
  pivot_wider(names_from = {{ outcome_var }}, values_from = text)
cont_pval <- cont_df %>%
  group_by(variable) %>%
  summarise(pval = wilcox.test(formula(paste0("value ~ ", outcome_var)))$p.value, .groups = "drop") %>%
  select(variable, pval)

bin_df <- demographics_df %>%
  select(
    {{ outcome_var }},
    Male, Mecvenadm, PCs_cardio, Surgadmission:PCs_metabolic,
    Vasopressor:Chronic_liver_disease, RRT
  ) %>%
  pivot_longer(-{{ outcome_var }}, names_to = "variable")
bin_n <- bin_df %>%
  group_by(.data[[outcome_var]], variable, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(.data[[outcome_var]], variable) %>%
  mutate(text = sprintf("%d (%.1f%%)", n, n / sum(n) * 100)) %>%
  filter(value == 1) %>% # might have to check for NAs?
  group_by(variable) %>%
  mutate(all = sprintf("%d (%.1f%%)", sum(n), sum(n) / nrow(demographics_df) * 100)) %>%
  select(-value, -n) %>%
  pivot_wider(names_from = {{ outcome_var }}, values_from = text)
bin_pval <- bin_df %>%
  group_by(variable) %>%
  summarise(
    pval = tryCatch(chisq.test(.data[[outcome_var]], value)$p.value, error = function(e) NA),
    .groups = "drop"
  )

demographics_table <- rbind(
  number,
  full_join(left_join(cont_iqr, cont_all), cont_pval),
  full_join(bin_n, bin_pval) %>% arrange(variable)
) %>%
  mutate(
    all = if_else(is.na(all), "0 (0.0%)", all),
    `0` = if_else(is.na(`0`), "0 (0.0%)", `0`),
    `1` = if_else(is.na(`1`), "0 (0.0%)", `1`)
  ) %>%
  select(variable, all, {{ outcome_var }} := `1`, "No_{{outcome_var}}" := `0`, pval) %>%
  mutate(pval = case_when(
    pval < 0.001 ~ "<0.001",
    pval < 0.01 ~ "<0.01 ",
    abs(pval - 0.05) < 0.01 ~ sprintf("%.3f", pval),
    TRUE ~ sprintf("%.2f ", pval)
  ))

kable(demographics_table)
