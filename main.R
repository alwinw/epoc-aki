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

max_num <- 7
excl_num <- c()
file_nums <- as.numeric(gsub(".*R/(.+[0-9])_[A-Za-z].*", "\\1", file_sources))
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
