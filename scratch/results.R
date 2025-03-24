dfs <- lapply(list.files("~/Documents/version_control/handwriterSetBuilder/experiments/results/long_v_lines_noGerman", full.names = TRUE), readRDS)

model_dir <- sapply(dfs, function(i) i$results_params$model_dir)
model <- sapply(dfs, function(i) i$results_params$model_name)
test_dir <- sapply(dfs, function(i) i$results_params$test_dir)
test_set <- sapply(dfs, function(i) i$results_params$test_name)
fpr <- sapply(dfs, function(i) i$accuracy$incorrectly_favors_prosecution)
fnr <- sapply(dfs, function(i) i$accuracy$incorrectly_favors_defense)

df <- data.frame(model_dir, model, test_dir, test_set, fpr, fnr)
