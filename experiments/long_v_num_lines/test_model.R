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

test_baseline_model <- function(test_path, output_dir) {

  get_baseline_model_params <- function() {
    model_params <- list()
    model_params$train_writers <- unique(handwriterRF::train$writer)
    model_params$validation_writers <- unique(handwriterRF::validation$writer)
    model_params$csafe_long_prompts <- c("pLND", "pWOZ")
    model_params$csafe_num_long <- 2
    model_params$csafe_num_short <- 0
    model_params$cvl_num_long <- 4
    model_params$cvl_num_short <- 0
    model_params$cvl_drop_German_prompt_train <- TRUE
    model_params$cvl_drop_German_prompt_validation <- TRUE
    model_params$distance_measures <- c("abs", "euc")
    model_params$downsample_diff_ref_pairs <- TRUE
    model_params$seed <- "Unknown"
    model_params$cvl_num_lines <- 0
    model_params$output_dir <- NA
    model_params$filename <- NA
    return(model_params)
  }

  results <- list()
  results$params <- list()

  # Load test
  test <- readRDS(test_path)

  # Set name of outfile
  results$params$output_dir <- output_dir
  results$params$filename <- paste0(
    "basline_model_and_",
    test$params$filename
  )

  results$slrs <- handwriterRF::compare_writer_profiles(
    writer_profiles = test$long,
    writer_profiles2 = test$short,
    score_only = FALSE,
    rforest = handwriterRF::random_forest,
    reference_scores = handwriterRF::ref_scores
  )
  results$slrs <- add_total_graphs_to_results(test, results$slrs)

  results$accuracy <- handwriterRF::get_rates_of_misleading_slrs(results$slrs)

  # Add params ----

  results$model_params <- get_baseline_model_params()
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

# for (m in models) {
#   for (t in test_sets) {
#     results <- test_model(m, t, output_dir)
#     message(paste("Saved results:", m, "and", t))
#   }
# }

# Results on test sets with baseline model trained on full doc vs. full doc
# comparisons
output_dir <- "experiments/results/baseline_noGerman"
for (t in test_sets) {
  results <- test_baseline_model(t, output_dir)
  message(paste("Saved results:", t))
}
