library(handwriterRF)
devtools::load_all()


train_model <- function(params) {

  set.seed(params$seed)

  model <- list()

  # Make Train and Validation Sets ----

  model$train <- make_csafe_cvl_set(
    all_writers = params$train_writers,
    csafe_long_prompts = params$csafe_long_prompts,
    csafe_num_long = params$csafe_num_long,
    csafe_num_short = params$csafe_num_short,
    cvl_num_long = params$cvl_num_long,
    cvl_num_short = params$cvl_num_short,
    cvl_num_lines = params$cvl_num_lines,
    cvl_drop_German_prompt = params$cvl_drop_German_prompt_train
  )

  model$validation <- make_csafe_cvl_set(
    all_writers = params$validation_writers,
    csafe_long_prompts = params$csafe_long_prompts,
    csafe_num_long = params$csafe_num_long,
    csafe_num_short = params$csafe_num_short,
    cvl_num_long = params$cvl_num_long,
    cvl_num_short = params$cvl_num_short,
    cvl_num_lines = params$cvl_num_lines,
    cvl_drop_German_prompt = params$cvl_drop_German_prompt_validation
  )


  # Train SLR Model ----

  model$random_forest <- handwriterRF::train_rf(
    df = model$train$long,
    distance_measures = params$distance_measures,
    df2 = model$train$short,
  )

  model$reference_scores <- handwriterRF::get_ref_scores(
    rforest = model$random_forest,
    df = model$validation$long,
    df2 = model$validation$short,
    seed = params$seed,
    downsample_diff_pairs = params$downsample_diff_ref_pairs
  )

  # Add params ----

  model$params <- params

  # Save ----

  saveRDS(model, file.path(params$output_dir, params$model_name))

  return(model)
}


params <- list()
params$train_writers <- unique(handwriterRF::train$writer)
params$validation_writers <- unique(handwriterRF::validation$writer)
params$csafe_long_prompts <- c("pLND", "pWOZ")
params$csafe_num_long <- 1
params$csafe_num_short <- 2
params$cvl_num_long <- 2
params$cvl_num_short <- 2
params$cvl_drop_German_prompt_train <- TRUE
params$cvl_drop_German_prompt_validation <- TRUE
params$distance_measures <- c("abs", "euc")
params$downsample_diff_ref_pairs <- TRUE

seed <- 0

# Make models with 1, 2, and 3 line pseudo-docs. Train 5 models for each
# length of pseudo-docs.
for (j in 1:3) { # number of lines
  # set number of lines
  params$cvl_num_lines <- j

  # set output directory
  if (j == 1) {
    params$output_dir <- "experiments/models/long_v_lines/1line"
  } else {
    params$output_dir <- paste0("experiments/models/long_v_lines/", j, "lines")
  }

  for (k in 1:5) {  # repetitions

    params$model_name <- paste0("model", k, ".rds")
    params$seed <- seed

    temp_model <- train_model(params)
    message(paste("Trained model: rep", k, "for", j, "line(s)"))

    seed <- seed + 100
  }
}

