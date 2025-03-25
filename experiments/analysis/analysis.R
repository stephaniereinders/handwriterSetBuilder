df <- readRDS("experiments/analysis/results_noGerman.rds")

# Look at 1 model per each number of lines in short training prompts
df <- df %>% dplyr::filter(model_rep == 1)
