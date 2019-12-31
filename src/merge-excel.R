# Merge Excel Files
# Alwin Wang 2019

suppressMessages(
oliguria_xlsx_data <- list(
  demographic = read_excel(oliguria_xlsx_path, "Patient Demographics"),
  data_set    = read_excel(oliguria_xlsx_path, "Data set"            ),
  outcomes    = read_excel(oliguria_xlsx_path, "AKI & outcomes"      ),
  screen_log  = read_excel(oliguria_xlsx_path, "Screening log"       )
))
suppressMessages(
creatinine_xlsx_data <- list(
  demographic = read_excel(creatinine_xlsx_path, "Patient Demographics"),
  data_set    = read_excel(creatinine_xlsx_path, "Data set"            ),
  outcomes    = read_excel(creatinine_xlsx_path, "AKI & outcomes"      ),
  screen_log  = read_excel(creatinine_xlsx_path, "Screening log"       )
))
suppressMessages(
demographics_xlsx_data <- list(
  num_creatinine = read_excel(demographics_xlsx_path, "no cr change"       ),
  num_oliguria   = read_excel(demographics_xlsx_path, "no oliguria"        ),
  neither_cr_ol  = read_excel(demographics_xlsx_path, "neither cr nor olig")
))

#

excel_date_to_character <- function(vector) {
  suppressWarnings(
  ifelse(
    grepl("/", vector), 
    vector, 
    as.character(as.Date(as.numeric(vector), origin = "1899-12-30"), format = "%d/%m/%y"))
  )
}

oliguria_xlsx_data$screen_log   %<>% arrange(`UR number`, Dates_screened) %>% 
  mutate(Dates_screened = excel_date_to_character(Dates_screened))

creatinine_xlsx_data$screen_log %<>% arrange(`UR number`, Dates_screened) %>% 
  mutate(Dates_screened = excel_date_to_character(Dates_screened))

#

data_collection_errors = 
  (creatinine_xlsx_data$screen_log$`UR number`      != oliguria_xlsx_data$screen_log$`UR number`     ) |
  (creatinine_xlsx_data$screen_log$Dates_screened   != oliguria_xlsx_data$screen_log$Dates_screened  ) |
  (creatinine_xlsx_data$screen_log$Excl_criteria_ok != oliguria_xlsx_data$screen_log$Excl_criteria_ok)

creatinine_collection_errors = creatinine_xlsx_data$screen_log[data_collection_errors,]
oliguria_collection_errors   = oliguria_xlsx_data  $screen_log[data_collection_errors,]

excluded_UR_numbers  =   creatinine_collection_errors$`UR number`
excluded_Pt_Study_no = discard(c(oliguria_collection_errors$Pt_Study_no, creatinine_collection_errors$Pt_Study_no), is.na)
cat(paste(
  "\n",
  "Discarded UR Numbers:       ", paste(excluded_UR_numbers , collapse = ", "), "\n",
  "Discarded Pt Study Numbers: ", paste(excluded_Pt_Study_no, collapse = ", "), "\n"
))

#

merge_columns = setdiff(
  intersect(colnames(creatinine_xlsx_data$screen_log), colnames(oliguria_xlsx_data$screen_log)),
  c("Incl_criteria_ok", "Pt_Study_no", tail(colnames(creatinine_xlsx_data$screen_log),1))
)
merged_xlsx_data = list(screen_log = 
    full_join(
      filter(creatinine_xlsx_data$screen_log, !(`UR number` %in% excluded_UR_numbers)),
      filter(oliguria_xlsx_data  $screen_log, !(`UR number` %in% excluded_UR_numbers)),
      by     = merge_columns,
      suffix = c("_cre", "_oli")
))
if (anyNA(merged_xlsx_data$screen_log$`UR number`)) {
  stop("NA in UR number of merged Excel sheets")
}
creatinine_xlsx_n_obs = nrow(filter(creatinine_xlsx_data$screen_log, !(`UR number` %in% excluded_UR_numbers)))
oliguria_xlsx_n_obs   = nrow(filter(oliguria_xlsx_data$screen_log  , !(`UR number` %in% excluded_UR_numbers)))
merged_xlsx_n_obs     = nrow(merged_xlsx_data$screen_log)
if (length(unique(c(creatinine_xlsx_n_obs, oliguria_xlsx_n_obs, merged_xlsx_n_obs))) != 1) {
  stop(paste0(
    "Inconsistent number of n_obs after UR numbers discarded. ",
    "Creatinine: ", creatinine_xlsx_n_obs, ", ",
    "Oliguria: "  , oliguria_xlsx_n_obs  , ", ",
    "Merged: "    , merged_xlsx_n_obs    , ", "))
}

#

# merged_xlsx_data$screen_log %<>% 
#   mutate(Excl_criteria_ok = factor(Excl_criteria_ok, levels = c("N", "Y"))) %>% 
#   group_by(Excl_criteria_ok)

admissions_flow_chart <- merged_xlsx_data$screen_log %>% 
  summarise(
    `Total Unique UR Numbers`     = length(unique(`UR number`)),
    `Total Admissions`            = length(`UR number`),
    `> Total Excluded Admissions` = length(`UR number`[Excl_criteria_ok == "N"]),
    `> Total Eligible Admissions` = length(`UR number`[Excl_criteria_ok == "Y"])
  )

kable(t(admissions_flow_chart))

cat(paste("\n", 
          "Total Admissions:         ", nrow(mer$scre), "\n",
          "Total Unique UR Number:   ", length(unique(mer$scre$`UR number`)), "\n",
          "Total Excluded Admissions:", nrow(filter(mer$scre, Excl_criteria_ok == "N")), "\n",
          "Total Eligible Admissions:", nrow(filter(mer$scre, Excl_criteria_ok == "Y")), "\n",
          "Excluded Branch\n",
          "  AKI:                    ", nrow(filter(mer$scre, Excl_criteria_ok =="N", Already_AKI == "Y")), "\n",
          "  Weekend:                ", nrow(filter(mer$scre, Excl_criteria_ok =="N", Admit_weekend == "Y")), "\n",
          "  No IDC:                 ", nrow(filter(mer$scre, Excl_criteria_ok =="N", No_IDC == "Y")), "\n",
          "  ESKD:                   ", nrow(filter(mer$scre, Excl_criteria_ok =="N", ESKD == "Y")), "\n",
          "  EOLC:                   ", nrow(filter(mer$scre, Excl_criteria_ok =="N", EOLC == "Y")), "\n",
          "  Kidney transplant:      ", nrow(filter(mer$scre, Excl_criteria_ok =="N", Kidney_transplant == "Y")), "\n",
          "  Child:                  ", nrow(filter(mer$scre, Excl_criteria_ok =="N", Child == "Y")), "\n",
          "Included Cr Branch\n",
          "  Cr change episode:      ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Epis_cr_change == "Y")), "\n",
          "     1 Cr episode:        ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_cr_epis ==  1)), "\n",
          "     2 Cr episode:        ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_cr_epis ==  2)), "\n",
          "     3 Cr episode:        ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_cr_epis ==  3)), "\n",
          "     4 Cr episode:        ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_cr_epis ==  4)), "\n",
          "     5 Cr episode:        ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_cr_epis ==  5)), "\n",
          "     6 Cr episode:        ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_cr_epis ==  6)), "\n",
          "     7 Cr episode:        ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_cr_epis ==  7)), "\n",
          "     8 Cr episode:        ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_cr_epis ==  8)), "\n",
          "     9 Cr episode:        ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_cr_epis ==  9)), "\n",
          "    10 Cr episode:        ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_cr_epis == 10)), "\n",
          "    Total Cr episodes:    ", sum(mer$scre$Total_no_cr_epis, na.rm = TRUE), "\n",
          "  No Cr change episode:   ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", is.na(Epis_cr_change))), "\n",
          "Included Olig Branch\n",
          "  Olig episode:           ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Epis_olig == "Y")), "\n",
          "     1 Olig episode:      ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_olig_epis ==  1)), "\n",
          "     2 Olig episode:      ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_olig_epis ==  2)), "\n",
          "     3 Olig episode:      ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_olig_epis ==  3)), "\n",
          "     4 Olig episode:      ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Total_no_olig_epis ==  4)), "\n",
          "     Total Olig episodes: ", sum(mer$scre$Total_no_olig_epis, na.rm = TRUE), "\n",
          "  No Olig episode:        ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", is.na(Epis_olig))), "\n",
          "Cr change and Olig\n",
          "  Both Cr and Olig:       ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Epis_cr_change == "Y", Epis_olig == "Y")), "\n",
          "  Neither Cr nor Olig:    ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", is.na(Epis_cr_change), is.na(Epis_olig))), "\n",
          "  Cr change only:         ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", Epis_cr_change == "Y", is.na(Epis_olig))), "\n",
          "  Olig only:              ", nrow(filter(mer$scre, Excl_criteria_ok =="Y", is.na(Epis_cr_change), Epis_olig == "Y")), "\n"
))

