# EPOC-AKI Study
# Copyright (C) 2018-2021  Alwin Wang, Lisa Toh

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# ---- Load Functions ----
file_sources <- list.files(path = "R/", pattern = "^[0-9][0-9].*.R$", full.names = TRUE)

max_num <- 12
excl_num <- c(12, 14)
file_nums <- as.numeric(gsub(".*([0-9]{2})_[A-Za-z].*", "\\1", file_sources))
file_sources <- file_sources[file_nums <= max_num & !(file_nums %in% excl_num)]
rm(max_num, excl_num, file_nums)

sapply(file_sources, function(file) {
  cat(paste0("\n", file, "\n"))
  source(file, .GlobalEnv)
})
rm(file_sources)

# ---- Run Analysis ----
# Load Data
xlsx_data <- load_excel_data(rel_path = rel_path)
excl_pts <- find_data_collection_errors(
  cr_data = xlsx_data$creatinine,
  olig_data = xlsx_data$oliguria
)
xlsx_data <- fix_data_collection_errors(
  xlsx_data = xlsx_data,
  excl_Pt_Study_no = excl_pts$Pt_Study_no
)

# Screening Log
screen_log <- create_screening_log(
  cr_data = xlsx_data$creatinine,
  olig_data = xlsx_data$oliguria,
  out_data = xlsx_data$screen_out,
  excl_UR_numbers = excl_pts$UR_numbers
)
screen_log <- verify_apache(
  screen_log = screen_log,
  apd_extract = xlsx_data$apd_extract$apd_extract
)
overview_screening_log(screen_log)

# Data Set
data_set <- create_data_set(
  cr_data = xlsx_data$creatinine,
  olig_data = xlsx_data$oliguria,
  excl_Pt_Study_no = excl_pts$Pt_Study_no
)
overview_data_set(data_set)

# Obs Data
obs_data <- create_obs_data(
  screen_log = screen_log,
  data_set = data_set
)
obs_data <- tidy_obs_data(
  obs_data = obs_data
)

# Admission Data
admission_data <- create_admin_data(
  obs_data = obs_data
)

# Creatinine Data
cr_data <- create_cr_data(
  creat_furo_data = xlsx_data$creat_furo,
  UR_numbers = unique(admission_data$UR_number),
  blood_gas_adjust = 0
)

cr_ch_ts <- generate_cr_changes(
  admission_data = admission_data,
  cr_data = cr_data
)

# Analysis Data
epoc_aki <- create_analysis_data(cr_ch_ts)

summarise_analysis(
  analysis_df = epoc_aki$analysis,
  measurements_df = epoc_aki$measurements
)

if (.Platform$OS.type == "windows") {
  plot_cr_ch_heatmap(
    analysis_df = epoc_aki$analysis,
    outcome_var = "AKI_ICU",
    save_plots = TRUE
  )

  plot_cr_ch_heatmap(
    analysis_df = epoc_aki$analysis,
    outcome_var = "AKI_2or3",
    save_plots = TRUE
  )
}

# Cr gradient only model
grad_only_model <- deoptim_search(
  analysis_data = epoc_aki$analysis,
  outcome_var = "AKI_2or3",
  baseline_predictors = NULL,
  cr_predictors = NULL,
  add_gradient_predictor = 1,
  stepwise = FALSE,
  penalty_fn = heuristic_penalty,
  itermax = 200,
  NP = 320,
  parallel = TRUE,
  secondary_outcomes = c(
    "AKI_ICU",
    "Cr_defined_AKI_2or3", "Cr_defined_AKI",
    "Olig_defined_AKI_2or3", "Olig_defined_AKI"
  ),
  override = c(5.7, 3.2, 3.0, 34.7),
  print = FALSE
)

# Cr change model
change_only_model <- deoptim_search(
  analysis_data = epoc_aki$analysis,
  outcome_var = "AKI_2or3",
  baseline_predictors = NULL,
  cr_predictors = "del_cr",
  add_gradient_predictor = NULL,
  stepwise = FALSE,
  k = "mBIC",
  penalty_fn = heuristic_penalty,
  itermax = 200,
  NP = 320,
  parallel = TRUE,
  secondary_outcomes = c(
    "AKI_ICU",
    "Cr_defined_AKI_2or3", "Cr_defined_AKI",
    "Olig_defined_AKI_2or3", "Olig_defined_AKI"
  ),
  override = c(5.6, 3.1, 3.0, 34.7),
  print = FALSE
)

# Cr percentage change model
per_only_model <- deoptim_search(
  analysis_data = epoc_aki$analysis,
  outcome_var = "AKI_2or3",
  baseline_predictors = NULL,
  cr_predictors = "per_cr_change",
  add_gradient_predictor = NULL,
  stepwise = FALSE,
  k = "mBIC",
  penalty_fn = heuristic_penalty,
  itermax = 200,
  NP = 320,
  parallel = TRUE,
  secondary_outcomes = c(
    "AKI_ICU",
    "Cr_defined_AKI_2or3", "Cr_defined_AKI",
    "Olig_defined_AKI_2or3", "Olig_defined_AKI"
  ),
  override = c(5.6, 3.1, 3.0, 34.7),
  print = FALSE
)

# Multivariate model
multi_model <- deoptim_search(
  analysis_data = epoc_aki$analysis,
  outcome_var = "AKI_2or3",
  baseline_predictors = c(
    "Age + Male + APACHE_II + APACHE_III + Baseline_Cr",
    "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + PVD + Chronic_liver_disease" # HT excluded
  ),
  cr_predictors = "cr", # c("cr", "per_cr_change"), # Put this into an "alternate" model
  add_gradient_predictor = 1,
  first_cr_only = FALSE,
  stepwise = TRUE,
  k = "mBIC",
  penalty_fn = heuristic_penalty,
  itermax = 200,
  NP = 320,
  parallel = TRUE,
  secondary_outcomes = c(
    "AKI_ICU",
    "Cr_defined_AKI_2or3", "Cr_defined_AKI",
    "Olig_defined_AKI_2or3", "Olig_defined_AKI"
  ),
  override = c(4.9, 1.8, 8.7, 16.9),
  print = FALSE
)
