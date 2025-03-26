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

  saveRDS(model, file.path(params$output_dir, params$filename))

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
params$seed <- 100

# Make models with 1, 2, 3, 4, and 5 line pseudo-docs. Train 5 models for each
# length of pseudo-docs.
for (j in 1:5) { # number of lines
  # set number of lines
  params$cvl_num_lines <- j

  # set output directory
  params$output_dir <- paste0("experiments/models/long_v_lines_noGerman")

  for (k in 1:5) {  # repetitions

    if (j == 1) {
      params$filename <- paste0(j, "line_model", k, ".rds")
    } else {
      params$filename <- paste0(j, "lines_model", k, ".rds")
    }

    temp_model <- train_model(params)
    message(paste("Trained model: rep", k, "for", j, "line(s)"))

    params$seed <- params$seed + 100
  }
}

