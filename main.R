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
  override = c(5.7, 3.2, 3.0, 34.7),
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
  override = c(5.7, 3.2, 3.0, 34.7),
  print = FALSE
)

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


# ---- Summarise Models ----
# Predictive value of cr ch change
table_cr_ch <- model_ssAOCI_summary(list(change_only_model, per_only_model, grad_only_model)) %>%
  as_tibble(.) %>%
  mutate(
    Predictor = case_when(
      Predictor == "del_cr" ~ "Cr change",
      Predictor == "per_cr_change" ~ "% Cr change",
      Predictor == "cr_gradient" ~ "Cr change >=1µmol/L/h"
    )
  )
kable(table_cr_ch, caption = "Predictive value parameters for creatinine change as an independent predictor of AKI")
write.csv(table_cr_ch, file = "table2.csv")

# Multivariable models with patient characteristics and creatinine change for the prediction of stages 2 and 3 AKI
table_multi <- model_ssACIBnri_summary(multi_model, multi_model$baseline_models$baseline_sig)
kable(table_multi, caption = "Multivariable models with patient characteristics and creatinine change for the prediction of stages 2 and 3 AKI")
write.csv(table_multi, file = "table3a.csv")

# Score
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

score_model_data <- multi_model$optim_model$data %>%
  select(AKI_2or3, predict, PCs_cardio, Vasopressor, Chronic_liver_disease, cr_gradient) %>%
  mutate(score = PCs_cardio + Vasopressor + 3 * Chronic_liver_disease + cr_gradient) %>%
  group_by(score) %>%
  mutate(score_est = score_predictor(PCs_cardio, Vasopressor, Chronic_liver_disease, cr_gradient))

BrierScore(score_model_data$AKI_2or3, score_model_data$score_est)

score_cp <- cutpointr(
  score_model_data,
  score_est, AKI_2or3,
  use_midpoints = TRUE, direction = ">=", pos_class = 1, neg_class = 0,
  method = maximize_metric, metric = youden,
  boot_runs = 1000
)
AUC_ci <- boot_ci(score_cp, AUC, in_bag = TRUE, alpha = 0.05)

nribin(
  event = score_model_data$AKI_2or3,
  p.std = multi_model$optim_model$data$predict, # TODO: NEEDS TO BE CHANGED TO BASELINE PREDICTION
  p.new = score_model_data$score_est,
  cut = 0.1, # multi_model$baseline_models$baseline_sig$cutpoint$youden,
  msg = FALSE,
  updown = "diff"
)$nri %>%
  rownames_to_column("Var") %>%
  slice_head(n = 3) %>%
  mutate(CI = sprintf("%.2f [%.2f-%.2f]", Estimate, Lower, Upper)) %>%
  select(Var, CI) %>%
  pivot_wider(names_from = Var, values_from = CI)

# ---- AUC Graph ----
# plot_roc(multi_model$optim_model$cutpoint)
cutpoints <- list(
  baseline_sig = multi_model$baseline_models$baseline_sig$cutpoint,
  optim_model = multi_model$optim_model$cutpoint,
  risk_score = score_cp
)

plot_data <- lapply(names(cutpoints), function(name) {
  cp <- cutpoints[[name]]
  data.frame(
    model = name,
    sensitivity = cp$sensitivity,
    specificity = cp$specificity,
    AUC = cp$AUC,
    tnr = cp$roc_curve[[1]]$tnr,
    tpr = cp$roc_curve[[1]]$tpr,
    row.names = NULL
  )
}) %>%
  bind_rows() %>%
  mutate(
    label = sprintf("AUC: %.2f \nSens: %.0f%%\nSpec: %.0f%%", AUC, sensitivity * 100, specificity * 100),
    hjust = case_when(
      model == "optim_model" ~ 1.1,
      model == "risk_score" ~ -0.1,
      model == "baseline_sig" ~ -0.1,
    ),
    vjust = case_when(
      model == "optim_model" ~ -0.1,
      model == "risk_score" ~ 1.1,
      model == "baseline_sig" ~ 1.1,
    ),
    model = factor(
      model,
      levels = c("optim_model", "risk_score", "baseline_sig"),
      labels = c(
        "Significant variables with\nCr change model",
        "ARC risk score",
        "Significant baseline\ncharacteristics model"
      ),
      ordered = TRUE
    ),
  )

auc_plot <- ggplot(plot_data, aes(colour = model)) +
  geom_line(aes(x = 1 - tnr, y = tpr, linetype = model), size = 0.5) +
  geom_point(aes(x = 1 - specificity, y = sensitivity), size = 3) +
  annotate("segment",
    x = 0, xend = 1, y = 0, yend = 1,
    colour = "darkgrey", linetype = "dashed"
  ) +
  geom_label(
    aes(x = 1 - specificity, y = sensitivity, label = label, hjust = hjust, vjust = vjust),
    show.legend = FALSE
  ) +
  xlab("1 - Specificity") +
  ylab("Sensitivity") +
  theme(aspect.ratio = 1, legend.position = c(0.88, 0.08)) +
  scale_colour_manual(name = "Legend", values = c("#289d87", "#404a88", "#440154")) +
  scale_linetype_manual(values = c("dashed", "solid", "dashed"), guide = "none") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

if (.Platform$OS.type == "windows") {
  ggsave("AUC_plot.png", auc_plot,
    path = paste0(rel_path, "/doc/images/"),
    width = 11.5, height = 11, scale = 0.7
  )
}
