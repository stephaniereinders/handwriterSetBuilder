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
params$cvl_num_lines <- 1
params$cvl_drop_German_prompt_train <- TRUE
params$cvl_drop_German_prompt_validation <- TRUE
params$distance_measures <- c("abs", "euc")
params$downsample_diff_ref_pairs <- TRUE
params$seed <- 100

model <- train_model(params)
saveRDS(model, "experiments/new_models/model_1line_v_long.rds")
