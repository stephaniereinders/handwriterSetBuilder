library(magrittr)

dfs <- lapply(list.files("~/Documents/version_control/handwriterSetBuilder/experiments/results/long_v_lines_noGerman", full.names = TRUE), readRDS)

model <- sapply(dfs, function(i) i$model_params$filename)
test_set <- sapply(dfs, function(i) i$test_params$filename)
fpr <- sapply(dfs, function(i) i$accuracy$incorrectly_favors_prosecution)
fnr <- sapply(dfs, function(i) i$accuracy$incorrectly_favors_defense)

test_prompt_ymin <- sapply(dfs, function(i) min(i$slrs$total_graphs1))
test_prompt_y25 <- sapply(dfs, function(i) quantile(i$slrs$total_graphs1, probs = 0.25))
test_prompt_y50 <- sapply(dfs, function(i) median(i$slrs$total_graphs1))
test_prompt_y75 <- sapply(dfs, function(i) quantile(i$slrs$total_graphs1, probs = 0.75))
test_prompt_ymax <- sapply(dfs, function(i) max(i$slrs$total_graphs1))

test_line_ymin <- sapply(dfs, function(i) min(i$slrs$total_graphs2))
test_line_y25 <- sapply(dfs, function(i) quantile(i$slrs$total_graphs2, probs = 0.25))
test_line_y50 <- sapply(dfs, function(i) median(i$slrs$total_graphs2))
test_line_y75 <- sapply(dfs, function(i) quantile(i$slrs$total_graphs2, probs = 0.75))
test_line_ymax <- sapply(dfs, function(i) max(i$slrs$total_graphs2))

df <- data.frame(
  model, test_set,
  test_prompt_ymin, test_prompt_y25, test_prompt_y50, test_prompt_y75, test_prompt_ymax,
  test_line_ymin, test_line_y25, test_line_y50, test_line_y75, test_line_ymax,
  fpr, fnr
)

df <- df %>% tidyr::separate_wider_delim(
  model,
  delim = "_",
  names = c("model_num_lines", "model_rep"),
  cols_remove = FALSE)
df$model_num_lines <- as.numeric(stringr::str_extract(df$model_num_lines, "\\d+"))
df$model_rep <- as.numeric(stringr::str_extract(df$model_rep, "\\d+"))

df <- df %>% tidyr::separate_wider_delim(
  test_set,
  delim = "_",
  names = c("test_num_lines", NA, "test_rep"),
  cols_remove = FALSE)
df$test_num_lines <- as.numeric(stringr::str_extract(df$test_num_lines, "\\d+"))
df$test_rep <- as.numeric(stringr::str_extract(df$test_rep, "\\d+"))

saveRDS(df, "experiments/analysis/long_v_lines_noGerman.rds")
