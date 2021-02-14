# ---- demographics_dfs ----
stopifnot(nrow(baseline_df) <= nrow(admission_data))
demographics_df <- left_join(
  baseline_df,
  admission_data
) %>%
  # TEMP ONLY
  mutate(Epis_cr = if_else(Event == "Both" | Event == "Cr change only", 1, 0)) %>%
  mutate(
    ICU_time2AKI = as.numeric(as.duration(DateTime_AKI_Dx - DateTime_ICU_admit), "hours")
  )

stopifnot(nrow(demographics_df) == nrow(baseline_df))
# TODO check why there are NAs for Wt

meas_df <- measurements_df %>%
  filter(cr_before_aki == 1) ## ONLY consider post AKI measurements

ts_df <- analysis_df %>%
  filter(cr_before_aki == 1) %>%
  mutate(
    cr_gradient = if_else(del_cr >= 1 * del_t_ch_hr, 1, 0), # HARD CODED 1umol/L/h
    per_cr_change = del_cr / cr * 100,
    per_cr_change_time_weighed = del_cr / cr / del_t_ch_hr * 100
  )

# ---- tabulate_functions ----
tabulate_numbers <- function(outcome_var, df, label) {
  df %>%
    select({{ outcome_var }}) %>%
    group_by(.data[[outcome_var]]) %>%
    summarise(text = sprintf("%d (%.1f%%)", n(), n() / nrow(.) * 100), .groups = "drop") %>%
    pivot_wider(names_from = {{ outcome_var }}, values_from = text) %>%
    mutate(
      variable = label,
      all = sprintf("%d (%.1f%%)", nrow(df), 100),
      pval = NA
    )
}

tabulate_cont_vars <- function(outcome_var, df, ...) {
  cont_df <- df %>%
    select({{ outcome_var }}, ...) %>%
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
  full_join(full_join(cont_iqr, cont_all, by = "variable"), cont_pval, by = "variable")
}

tabulate_bin_vars <- function(outcome_var, df, ...) {
  bin_df <- df %>%
    select({{ outcome_var }}, ...) %>%
    pivot_longer(-{{ outcome_var }}, names_to = "variable")
  bin_n <- bin_df %>%
    group_by(.data[[outcome_var]], variable, value) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(.data[[outcome_var]], variable) %>%
    mutate(text = sprintf("%d (%.1f%%)", n, n / sum(n) * 100)) %>%
    filter(value == 1) %>% # might have to check for NAs?
    group_by(variable) %>%
    mutate(all = sprintf("%d (%.1f%%)", sum(n), sum(n) / nrow(df) * 100)) %>%
    select(-value, -n) %>%
    pivot_wider(names_from = {{ outcome_var }}, values_from = text)
  bin_pval <- bin_df %>%
    group_by(variable) %>%
    summarise(
      pval = tryCatch(chisq.test(.data[[outcome_var]], value)$p.value, error = function(e) NA),
      .groups = "drop"
    )
  full_join(bin_n, bin_pval, by = "variable") %>%
    arrange(variable) %>%
    mutate(
      all = if_else(is.na(all), "0 (0.0%)", all),
      `0` = if_else(is.na(`0`), "0 (0.0%)", `0`),
      `1` = if_else(is.na(`1`), "0 (0.0%)", `1`)
    )
}

# ---- demographics_tables ----

generate_demographics_table <- function(outcome_var) {
  tables <- list(
    tabulate_numbers(outcome_var, demographics_df, "Admissions"),
    tabulate_cont_vars(
      outcome_var, demographics_df,
      Age, Wt, APACHE_II, APACHE_III,
      Baseline_Cr, LOS_ICU_hr, LOS_Hosp_hr, ICU_time2AKI
    ),
    tabulate_bin_vars(
      outcome_var, demographics_df,
      Male, Mecvenadm, PCs_cardio, Surgadmission:PCs_metabolic,
      Vasopressor:Chronic_liver_disease, RRT, AKI_ICU:Olig_defined_AKI_2or3
    ),
    tabulate_numbers(outcome_var, meas_df, "Measurements"),
    tabulate_numbers(outcome_var, ts_df, "Creatinine_changes"),
    tabulate_cont_vars(outcome_var, meas_df, cr),
    tabulate_cont_vars(outcome_var, baseline_df, n_measurements),
    tabulate_cont_vars(
      outcome_var,
      ts_df, del_cr, del_t_ch_hr, del_t_aki_hr,
      per_cr_change, per_cr_change_time_weighed
    ),
    tabulate_bin_vars(outcome_var, ts_df, cr_gradient)
  )

  bind_rows(tables) %>%
    select(variable, all, {{ outcome_var }} := `1`, "No_{{outcome_var}}" := `0`, pval) %>%
    mutate(pval = case_when(
      pval < 0.001 ~ "<0.001",
      pval < 0.01 ~ "<0.01 ",
      abs(pval - 0.05) < 0.01 ~ sprintf("%.3f", pval),
      TRUE ~ sprintf("%.2f ", pval)
    ))
}

demographics_table <- generate_demographics_table("AKI_2or3")

kable(demographics_table)
write.csv(demographics_table, "demographics_table.csv")
