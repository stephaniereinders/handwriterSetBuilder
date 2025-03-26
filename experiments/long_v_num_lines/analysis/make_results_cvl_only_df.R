library(magrittr)

make_results_row <- function(ex, baseline) {
  slr <- ex$slrs

  cvl_short <- slr %>% dplyr::filter(!startsWith(writer2, "w"))

  accuracy <- handwriterRF::get_rates_of_misleading_slrs(cvl_short)

  total_graphs <- cvl_short %>%
    dplyr::select(tidyselect::all_of(c("docname2", "total_graphs2"))) %>%
    dplyr::distinct() %>%
    dplyr::pull(total_graphs2)
  total_graphs_min <- min(total_graphs)
  total_graphs_25 <- quantile(total_graphs, 0.25)
  total_graphs_median <- median(total_graphs)
  total_graphs_75 <- quantile(total_graphs, 0.75)
  total_graphs_max <- max(total_graphs)

  if (baseline) {
    filename <- ex$params$filename
    numbers <- regmatches(filename, gregexpr("\\d+", filename))[[1]]
    model <- "full_v_full_rep1"
    model_lines <- "full_v_full"
    model_rep <- "rep1"
    test_set <- paste0("full_v_", numbers[1], "lines_rep_", numbers[2])
    test_lines <- paste0("full_v_", numbers[1], "lines")
    test_rep <- paste0("rep", numbers[2])
  } else {
    filename <- ex$params$filename
    numbers <- regmatches(filename, gregexpr("\\d+", filename))[[1]]
    model <- paste0("full_v_", numbers[1], "lines_rep", numbers[2])
    model_lines <- paste0("full_v_", numbers[1], "lines")
    model_rep <- paste0("rep", numbers[2])
    test_set <- paste0("full_v_", numbers[3], "lines_rep_", numbers[4])
    test_lines <- paste0("full_v_", numbers[3], "lines")
    test_rep <- paste0("rep", numbers[4])
  }

  df <- data.frame(
    model,
    model_lines,
    model_rep,
    test_set,
    test_lines,
    test_rep,
    total_graphs_min,
    total_graphs_25,
    total_graphs_median,
    total_graphs_75,
    total_graphs_max,
    fpr = accuracy$incorrectly_favors_prosecution,
    fnr = accuracy$incorrectly_favors_defense
  )
  row.names(df) <- NULL

  return(df)
}

files <- list.files("experiments/results/long_v_lines_noGerman", full.names = TRUE)
exps <- lapply(files, readRDS)
results <- lapply(exps, function(ex) make_results_row(ex, baseline = FALSE))
results <- do.call(rbind, results)
saveRDS(results, "experiments/analysis/short_is_cvl_only_noGerman.rds")

baseline_files <- list.files("experiments/results/baseline_noGerman", full.names = TRUE)
baseline_exps <- lapply(baseline_files, readRDS)
baseline_results <- lapply(baseline_exps, function(ex) make_results_row(ex, baseline = TRUE))
baseline_results <- do.call(rbind, baseline_results)
saveRDS(baseline_results, "experiments/analysis/baseline_short_is_cvl_only_noGerman.rds")

results <- readRDS("experiments/analysis/short_is_cvl_only_noGerman.rds")
baseline <- readRDS("experiments/analysis/baseline_short_is_cvl_only_noGerman.rds")
master <- rbind(results, baseline)
saveRDS(master, "experiments/analysis/results_and_baseline.rds")
