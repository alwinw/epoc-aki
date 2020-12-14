# ---- model_summary ----

library(kableExtra)

model_summary <- list(
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

model_list <- lapply(
  model_summary, function(model) {
    return(data.frame(
      Description = model[[1]],
      model$params,
      model$summary[c(1:3, 6, 5, 9:10)]
    ))
  }
)

bind_rows(model_list) %>%
  kable(.) %>%
  kable_styling() %>%
  scroll_box(width = "100%", box_css = "border: 0px;")

write.csv(bind_rows(model_list), file = "model_summary.csv", row.names = FALSE)
