library(magrittr)

expand_filenames <- function(exps, baseline = FALSE) {
  filenames <- sapply(exps, function(ex) ex$params$filename)
  df <- data.frame(filename = filenames)
  df <- df %>% tidyr::separate_wider_delim(
    filename,
    delim = "_",
    names = c("model_lines", "model_num", NA, "test_lines", NA, "test_num"),
    cols_remove = FALSE)

  # Drop file extension
  df$test_num <- stringr::str_replace(df$test_num, ".rds", "")

  # Add experiment name
  df$experiment <- paste("full", "v", df$model_lines, df$model_num, "full", "v", df$test_lines, df$test_num, sep = "_")

  # Add model and model_rep
  if (baseline){
    df$model <- "full_v_full"
    df$model_rep <- 1
  } else {
    df$model <- paste("full", "v", df$model_lines, sep = "_")
    df$model_rep <- as.numeric(stringr::str_replace(df$model_num, "model", ""))
  }

  # Add test_set and test_set_rep
  df$test_set <- paste("full", "v", df$test_lines, sep = "_")
  df$test_set_rep <- as.numeric(stringr::str_replace(df$test_num, "set", ""))

  # Drop unused columns
  df <- df %>%
    dplyr::select(tidyselect::any_of(c("experiment", "model", "model_rep", "test_set", "test_set_rep")))

  return(df)
}

get_accuracy <- function(exps) {
  fpr <- sapply(exps, function(ex) ex$accuracy$incorrectly_favors_prosecution)
  fnr <- sapply(exps, function(ex) ex$accuracy$incorrectly_favors_defense)

  return(data.frame(fpr, fnr))
}

get_total_graphs <- function(exps) {
  get_quantiles <- function(slr) {
    # Get unique rows of docname / total graphs pairs for short documents
    tg_df <-
      data.frame(docname = slr$docname2, total_graphs = slr$total_graphs2) %>%
      dplyr::distinct()

    # Get quantiles of total graphs
    quantiles <- data.frame(short_tg_min = min(tg_df$total_graphs),
                            short_tg_25 = quantile(tg_df$total_graphs, 0.25),
                            short_tg_median = median(tg_df$total_graphs),
                            short_tg_75 = quantile(tg_df$total_graphs, 0.75),
                            short_tg_max = max(tg_df$total_graphs))
    rownames(quantiles) <- NULL

    return(quantiles)
  }

  slrs <- lapply(exps, function(ex) ex$slrs)

  tg <- lapply(slrs, function(slr) get_quantiles(slr))
  tg <- do.call(rbind, tg)

  return(tg)
}

load_and_format_results <- function(exps, baseline = FALSE) {
  filenames <- expand_filenames(exps, baseline = baseline)
  tg <- get_total_graphs(exps)
  accuracy <- get_accuracy(exps)
  df <- cbind(filenames, tg, accuracy)
  return(df)
}


files <- list.files("experiments/results/long_v_lines_noGerman", full.names = TRUE)
exps <- lapply(files, readRDS)

baseline_files <- list.files("experiments/results/baseline_noGerman", full.names = TRUE)
baseline_exps <- lapply(baseline_files, readRDS)

df <- load_and_format_results(exps, baseline = FALSE)
baseline_df <- load_and_format_results(baseline_exps, baseline = TRUE)

results <- rbind(df, baseline_df)
saveRDS(results, "experiments/analysis/results_noGerman.rds")
