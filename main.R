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

max_num <- 15
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
  baseline_data = epoc_aki$baseline,
  override = c(4.9, 1.8, 8.7, 16.9),
  print = FALSE
)


# Table 2
model_ssAOCI_summary(list(change_only_model, per_only_model, grad_only_model)) %>%
  kable(.)
# TODO: Fix up `cr_gradient + cr_gradient` Predictor
# TODO: Uniform cr change ep duration

nribin(
  event = multi_model$optim_model$data$AKI_2or3,
  z.std = as.matrix(select(
    multi_model$optim_model$data,
    APACHE_II, PCs_cardio, Vasopressor
  )),
  z.new = as.matrix(select(
    multi_model$optim_model$data,
    PCs_cardio, Vasopressor, Chronic_liver_disease, cr_gradient
  )),
  cut = 0.1, # multi_model$baseline_models$baseline_sig$cutpoint$youden,
  msg = TRUE,
  updown = "diff"
)

BrierScore(multi_model$baseline_models$baseline_all$model)
BrierScore(multi_model$baseline_models$baseline_sig$model)
BrierScore(multi_model$optim_model$model)

opt_cut_baseline_sig <- cutpointr(
  multi_model$baseline_models$baseline_sig$data,
  predict, AKI_2or3,
  use_midpoints = TRUE, direction = ">=", pos_class = 1, neg_class = 0,
  method = maximize_metric, metric = youden,
  boot_runs = 1000
)
boot_ci(opt_cut_baseline_sig, AUC, in_bag = TRUE, alpha = 0.05)

opt_cut_optim_model <- cutpointr(
  multi_model$optim_model$data,
  predict, AKI_2or3,
  use_midpoints = TRUE, direction = ">=", pos_class = 1, neg_class = 0,
  method = maximize_metric, metric = youden,
  boot_runs = 1000
)
boot_ci(opt_cut_optim_model, AUC, in_bag = TRUE, alpha = 0.05)


# Score
manual_predictor <- function(PCs_cardio, Vasopressor, Chronic_liver_disease, cr_gradient) {
  y <- coef(multi_model$optim_model$model)["(Intercept)"] +
    coef(multi_model$optim_model$model)["PCs_cardio"] * PCs_cardio +
    coef(multi_model$optim_model$model)["Vasopressor"] * Vasopressor +
    coef(multi_model$optim_model$model)["Chronic_liver_disease"] * Chronic_liver_disease +
    coef(multi_model$optim_model$model)["cr_gradient"] * cr_gradient
  as.numeric(1 / (1 + exp(-y)))
}

stopifnot(all.equal(
  manual_predictor(
    multi_model$optim_model$data$PCs_cardio,
    multi_model$optim_model$data$Vasopressor,
    multi_model$optim_model$data$Chronic_liver_disease,
    multi_model$optim_model$data$cr_gradient
  ),
  as.numeric(multi_model$optim_model$data$predict)
))

lm_score <- lm(
  predict ~ PCs_cardio + Vasopressor + Chronic_liver_disease + cr_gradient,
  multi_model$optim_model$data
)

manual_predictor(
  PCs_cardio = 0,
  Vasopressor = 1,
  Chronic_liver_disease = 0,
  cr_gradient = 0
)

BrierScore(multi_model$optim_model$data$AKI_2or3, multi_model$optim_model$data$predict)

brier_wrapper <- function(b, data) {
  y <- 0 + b[1] +
    b[2] * data$PCs_cardio +
    b[3] * data$Vasopressor +
    b[4] * data$Chronic_liver_disease +
    b[5] * data$cr_gradient
  p <- as.numeric(1 / (1 + exp(-y)))
  BrierScore(data$AKI_2or3, p)
}

# brier_wrapper(c(-4.9068747, 1.7988061, 1.1770319, 3.6879612, 0.9126502), multi_model$optim_model$data)

score_coef <- DEoptim(
  brier_wrapper,
  rep(-10, 5),
  rep(10, 5),
  DEoptim.control(NP = 320, itermax = 500, trace = 100),
  fnMap = function(x) c(x[1], round(x[2:5], 0)),
  data = multi_model$optim_model$data
)
score_coef$optim$bestval
score_coef$optim$bestmem
# Summary: Vasopressor is 3x more important than others

temp <- multi_model$optim_model$data %>%
  select(AKI_2or3, predict, PCs_cardio, Vasopressor, Chronic_liver_disease, cr_gradient) %>%
  mutate(score = PCs_cardio + Vasopressor + 3 * Chronic_liver_disease + cr_gradient) %>%
  group_by(score)

temp %>% summarise(
  # across(everything(), list(mean = mean, median = median))
  mean = mean(predict) * 100,
  median = median(predict) * 100,
  .groups = "drop"
)

ggplot(temp) +
  geom_point(aes(score, predict))

score_predictor <- function(PCs_cardio, Vasopressor, Chronic_liver_disease, cr_gradient) {
  score <- PCs_cardio + Vasopressor + 3 * Chronic_liver_disease + cr_gradient
  case_when(
    score == 0 ~ 0.00734,
    score == 1 ~ 0.0234,
    score == 2 ~ 0.127,
    score == 3 ~ 0.265,
    score == 4 ~ 0.424,
    score == 5 ~ 0.853
  )
  # ^ Consider running a DEOptim to optimise these instead of just mean/median
}

score_est <- score_predictor(
  multi_model$optim_model$data$PCs_cardio,
  multi_model$optim_model$data$Vasopressor,
  multi_model$optim_model$data$Chronic_liver_disease,
  multi_model$optim_model$data$cr_gradient
)

temp <- temp %>%
  mutate(score_est = score_predictor(PCs_cardio, Vasopressor, Chronic_liver_disease, cr_gradient))

BrierScore(temp$AKI_2or3, temp$score_est)

score_cp <- cutpointr(
  temp,
  score_est, AKI_2or3,
  use_midpoints = TRUE, direction = ">=", pos_class = 1, neg_class = 0,
  method = maximize_metric, metric = youden,
  boot_runs = 1000
)
