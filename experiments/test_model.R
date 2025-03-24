devtools::load_all()


test_model <- function(model_path, test_path, output_dir) {

  results <- list()
  results$params <- list()

  # Load model
  model <- readRDS(model_path)

  # Load test
  test <- readRDS(test_path)

  # Set name of outfile
  results$params$output_dir <- output_dir
  results$params$filename <- paste0(
    stringr::str_replace(model$params$filename, ".rds", ""),
    "_and_",
    test$params$filename
  )

  results$slrs <- handwriterRF::compare_writer_profiles(
    writer_profiles = test$long,
    writer_profiles2 = test$short,
    score_only = FALSE,
    rforest = model$random_forest,
    reference_scores = model$reference_scores
  )
  results$slrs <- add_total_graphs_to_results(test, results$slrs)

  results$accuracy <- handwriterRF::get_rates_of_misleading_slrs(results$slrs)

  # Add params ----

  results$model_params <- model$params
  results$test_params <- test$params
  results$test_params$total_graphs_long_docs <- test$params$long$total_graphs
  results$test_params$total_graphs_short_docs <- test$params$short$total_graphs

  # Save
  saveRDS(results, file.path(results$params$output_dir, results$params$filename))

  return(results)
}

models <- list.files("experiments/models/long_v_lines_noGerman", full.names = TRUE)
test_sets <- list.files("experiments/test_sets/long_v_lines_noGerman", full.names = TRUE)
output_dir <- "experiments/results/long_v_lines_noGerman"

for (m in models) {
  for (t in test_sets) {
    results <- test_model(m, t, output_dir)
    message(paste("Saved results:", m, "and", t))
  }
}

