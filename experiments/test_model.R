devtools::load_all()

test_model <- function(params, model = NULL, test = NULL) {
  results <- list()

  # Load model
  if (is.null(model)) {
    model <- readRDS(file.path(params$model_dir, params$model_name))
  }

  # Load test
  if (is.null(test)) {
    test <- readRDS(file.path(params$test_dir, params$test_name))
  }

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

  results$results_params <- params

  if (is.null(model)) {
    results$model_params <- model$params
  }

  if (is.null(test)) {
    results$test_params <- test$params
  }

  # Save
  saveRDS(results, file.path(params$results_dir, params$results_name))

  return(results)
}

params <- list()
params$model_name <- "model1.rds"
params$model_dir <- "experiments/models/long_v_lines/1line"
params$test_name <- "long_v_1line_noGerman.rds"
params$test_dir <- "experiments/test_sets"
params$results_name <- "results1.rds"
params$results_dir <- "experiments/results"

results <- test_model(params)
