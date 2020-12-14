# ---- model_summary ----

library(kableExtra)

optim_model_summary <- list(
  c("prev study", cr_ch_prev_study_bin),
  c("cr ch only", only_model_1),
  c("baseline all", base_all_model),
  c("abstract", example_bin_2),
  c("multi 1", multi_model_1b),
  c("multi 2", multi_model_2b),
  c("multi 3", multi_model_3),
  c("multi 4", multi_model_4),
  c("multi 5", multi_model_5),
  c("multi 6", multi_model_6),
  c("multi 7", multi_model_7),
  c("multi 8", multi_model_8),
  c("multi 9", multi_model_9),
  c("multi 10", multi_model_10),
  c("multi 11", multi_model_11)
)

optim_model_list <- lapply(
  optim_model_summary, function(model) {
    return(data.frame(
      Description = model[[1]],
      model$params,
      model$summary[c(1:3, 6, 5, 9:10)]
    ))
  }
)

bind_rows(optim_model_list) %>%
  kable(.) %>%
  kable_styling() %>%
  scroll_box(width = "100%", box_css = "border: 0px;")

write.csv(bind_rows(optim_model_list), file = "model_summary.csv", row.names = FALSE)

rm(cr_ch_prev_study_bin, only_model_1, base_all_model, example_bin_2,
   multi_model_1b, multi_model_2b, multi_model_3, multi_model_4, 
   multi_model_5, multi_model_6, multi_model_7, multi_model_8, multi_model_9, 
   multi_model_10, multi_model_11, optim_model_list)
