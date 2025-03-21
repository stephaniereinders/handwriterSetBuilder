devtools::load_all()

make_test_set <- function(params) {
  set.seed(params$seed)
  test <- make_csafe_cvl_set(
    all_writers = params$test_writers,
    csafe_long_prompts = params$csafe_long_prompts,
    csafe_num_long = params$csafe_num_long,
    csafe_num_short = params$csafe_num_short,
    cvl_num_long = params$cvl_num_long,
    cvl_num_short = params$cvl_num_short,
    cvl_num_lines = params$cvl_num_lines,
    cvl_drop_German_prompt = params$cvl_drop_German_prompt_test
  )

  # Add params ----

  test$params <- params

  # Save

  saveRDS(test, file.path(params$output_dir, params$test_name))

  return(test)
}


params <- list()
params$test_name <- "long_v_1line_noGerman.rds"
params$output_dir <- "experiments/test_sets"
params$test_writers <- unique(handwriterRF::test$writer)
params$csafe_long_prompts <- c("pLND", "pWOZ")
params$csafe_num_long <- 1
params$csafe_num_short <- 2
params$cvl_num_long <- 2
params$cvl_num_short <- 2
params$cvl_num_lines <- 1
params$cvl_drop_German_prompt_test <- TRUE
params$seed <- 100

test <- make_test_set(params)
