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


demographics_df %>%
  select(
    AKI_2or3,
    Age, Wt, APACHE_II, APACHE_III
  )
