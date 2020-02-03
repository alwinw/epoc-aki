# Merge Excel apache scores
# Alwin Wang 2020

# Merge in Apache Data ----
merge_xlsx_apache <- function(screening_data, xlsx_data) {
  apd_extract <-
    xlsx_data$apd_extract$apd_extract[c("HRN/NIH", "ICU_ADM_DTM",
                                        "AP2score", "AP3score")]
  apd_extract <- apd_extract %>%
    mutate(AP2score = as.numeric(AP2score),
           AP3score = as.numeric(AP3score)) %>% 
    rename(`UR number` = `HRN/NIH`)
  
  # Note this contains some duplicate UR numbers for readmission (e.g. Mon, Sat)
  # After merging, we need to remove combinations of (Mon-Sat, Sat-Mon) so that
  # we are only left with (Mon-Mon) and (Sat-Sat) combinations
  
  screening_apd <-left_join(screening_data, apd_extract, by = "UR number")
  screening_apd <- screening_apd %>% 
    mutate(ICU_admit_diff =
             as.duration(
               DateTime_ICU_admit %--% force_tz(ICU_ADM_DTM, "Australia/Melbourne")
             ) / dhours(1)) %>%
    mutate(ICU_admit_diff = ifelse(is.na(ICU_admit_diff), 0, ICU_admit_diff))
  
  
  
  nrow(screening_data) == nrow(screening_apd)
  
}