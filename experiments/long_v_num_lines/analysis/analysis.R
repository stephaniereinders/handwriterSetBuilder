library(ggplot2)


df <- readRDS("experiments/analysis/results_and_baseline.rds")


# FPR and FNR -------------------------------------------------------------

# Average FPR grouping by number of lines in model and number of lines in test
# set
averages <- df %>%
  dplyr::group_by(model_lines, test_lines) %>%
  dplyr::summarize(avg_fpr = mean(fpr),
                   fpr_sd = sd(fpr),
                   avg_fnr = mean(fnr),
                   fnr_sd = sd(fnr))

fpr <- averages %>% ggplot(aes(x=model_lines, y=test_lines)) +
  geom_tile(aes(fill = avg_fpr)) +
  geom_text(aes(label = scales::percent(round(avg_fpr, 4))), color = "black") +
  scale_fill_gradient(
    limits = c(0,1),
    breaks = c(0, 0.25, 0.50, 0.75, 1.00),
    labels = scales::percent(c(0, 0.25, 0.50, 0.75, 1.00)),
    low = "white",
    high = "coral",
    na.value = NA) +
  labs(title = "False Positive Rates",
       fill = "Mean FPR",
       x = "Comparisons in Model Training Set",
       y = "Comparisons in Test Set"
  ) +
  theme_bw() +
  theme(legend.position="bottom")
ggsave(filename = "experiments/plots/false_positive_rates.png")

fnr <- averages %>% ggplot(aes(x=model_lines, y=test_lines)) +
  geom_tile(aes(fill = avg_fnr)) +
  geom_text(aes(label = scales::percent(round(avg_fnr, 4))), color = "black") +
  scale_fill_gradient(
    limits = c(0,1),
    breaks = c(0, 0.25, 0.50, 0.75, 1.00),
    labels = scales::percent(c(0, 0.25, 0.50, 0.75, 1.00)),
    low = "white",
    high = "coral",
    na.value = NA) +
  labs(title = "False Negative Rates",
       fill = "Mean FNR",
       x = "Comparisons in Model Training Set",
       y = "Comparisons in Test Set"
  ) +
  theme_bw() +
  theme(legend.position="bottom")
ggsave(filename = "experiments/plots/false_negative_rates.png")


# Graphs Per Document -----------------------------------------------------

lines <- df %>%
  dplyr::select(
    tidyselect::all_of(
      c("test_set", "test_lines", "test_rep", "total_graphs_min", "total_graphs_25",
        "total_graphs_median", "total_graphs_75", "total_graphs_max"))) %>%
  dplyr::distinct() %>%
  dplyr::mutate(test_set = as.factor(test_set),
                test_lines = as.factor(test_lines),
                test_rep = as.factor(test_rep))

# Clean up test set values for better plot labels
lines$test_set <- stringr::str_replace(lines$test_set, "full_v_", "")
lines$test_set <- stringr::str_replace(lines$test_set, "1lines_rep_", "1 line rep ")
lines$test_set <- stringr::str_replace(lines$test_set, "2lines_rep_", "2 lines rep ")
lines$test_set <- stringr::str_replace(lines$test_set, "3lines_rep_", "3 lines rep ")
lines$test_set <- stringr::str_replace(lines$test_set, "4lines_rep_", "4 lines rep ")
lines$test_set <- stringr::str_replace(lines$test_set, "5lines_rep_", "5 lines rep ")

lines %>%
  ggplot(aes(x=test_set)) +
  geom_boxplot(
    aes(ymin = total_graphs_min, lower = total_graphs_25, middle = total_graphs_median,
        upper = total_graphs_75, ymax = total_graphs_max),
    stat = "identity") +
  labs(x = "Test Set", y = "# of Graphs per Document") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave(filename = "experiments/plots/graphs_per_doc.png")
