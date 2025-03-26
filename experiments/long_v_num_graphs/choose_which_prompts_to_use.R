# This script performs exploratory analysis to decide (1) which prompts to use
# for which set - train, valdiation, and test - and (2) which prompts to use to
# create pseudo documents from individual lines

library(ggplot2)
devtools::load_all()

train_writers <- unique(handwriterRF::train$writer)
validation_writers <- unique(handwriterRF::validation$writer)
test_writers <- unique(handwriterRF::test$writer)

counts <- cvl_prompt_cfr %>%
  dplyr::filter(prompt != 6) %>%
  dplyr::group_by(prompt) %>%
  dplyr::summarize(
    tgmin = min(total_graphs),
    tg25 = quantile(total_graphs, 0.25),
    tgmedian = median(total_graphs),
    tg75 = quantile(total_graphs, 0.75),
    tgmax = max(total_graphs)
  ) %>%
  dplyr::mutate(prompt = as.factor(prompt))

counts %>%
  ggplot(aes(x=prompt)) +
  geom_boxplot(aes(ymin = tgmin, lower = tg25, middle = tgmedian, upper = tg75, ymax = tgmax),
               stat = "identity") +
  labs(x = "CVL Prompt", y = "# Graphs per Document") +
  theme_bw()
ggsave(filename = "experiments/long_v_num_graphs/plots/cvl_num_graphs_per_doc.png")

csafe_counts <- csafe_prompt_cfr %>%
  dplyr::group_by(prompt) %>%
  dplyr::summarize(
    tgmin = min(total_graphs),
    tg25 = quantile(total_graphs, 0.25),
    tgmedian = median(total_graphs),
    tg75 = quantile(total_graphs, 0.75),
    tgmax = max(total_graphs)
  ) %>%
  dplyr::mutate(prompt = as.factor(prompt))



csafe_counts %>%
  ggplot(aes(x=prompt)) +
  geom_boxplot(aes(ymin = tgmin, lower = tg25, middle = tgmedian, upper = tg75, ymax = tgmax),
               stat = "identity") +
  labs(x = "CSAFE Prompt", y = "# Graphs per Document") +
  theme_bw()
ggsave(filename = "experiments/long_v_num_graphs/plots/csafe_num_graphs_per_doc.png")

