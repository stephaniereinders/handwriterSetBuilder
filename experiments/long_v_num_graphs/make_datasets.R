# This script groups pseudo-docs by number of graphs

library(ggplot2)


df <- readRDS("experiments/long_v_num_graphs/data/pseudo-docs/all_pseudo_docs.rds")

# Chunk total graphs into groups
df$group <- cut(df$total_graphs, breaks = seq(0, 500, 50))
df$lower <- as.numeric(sub("\\((.+),.*", "\\1", df$group))
df$upper <- as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", df$group))

# Select 2 prompt 2's and 2 prompt 4's from each writer for each group
sample <- df %>%
  dplyr::group_by(group, writer, prompt) %>%
  dplyr::slice_sample(n = 2)

# Drop cases where the writer does not have exactly 4 prompts for a group
sample <- sample %>%
  dplyr::group_by(group, writer) %>%
  dplyr::filter(dplyr::n() == 4)

# Get writers ----

# Drop writers that don't have 4 prompts exactly in each of the first 5 groups

groups <- unique(sample$group)

writers1 <- sample %>%
  dplyr::filter(group == groups[1]) %>%
  dplyr::ungroup() %>%
  dplyr::select(tidyselect::all_of("writer")) %>%
  dplyr::distinct() %>%
  dplyr::pull()
writers2 <- sample %>%
  dplyr::filter(group == groups[2]) %>%
  dplyr::ungroup() %>%
  dplyr::select(tidyselect::all_of("writer")) %>%
  dplyr::distinct() %>%
  dplyr::pull()
writers3 <- sample %>%
  dplyr::filter(group == groups[3]) %>%
  dplyr::ungroup() %>%
  dplyr::select(tidyselect::all_of("writer")) %>%
  dplyr::distinct() %>%
  dplyr::pull()
writers4 <- sample %>%
  dplyr::filter(group == groups[4]) %>%
  dplyr::ungroup() %>%
  dplyr::select(tidyselect::all_of("writer")) %>%
  dplyr::distinct() %>%
  dplyr::pull()
writers5 <- sample %>%
  dplyr::filter(group == groups[5]) %>%
  dplyr::ungroup() %>%
  dplyr::select(tidyselect::all_of("writer")) %>%
  dplyr::distinct() %>%
  dplyr::pull()

# Find writers in all five groups
writers <- intersect(writers1, writers2)
writers <- intersect(writers, writers3)
writers <- intersect(writers, writers4)
writers <- intersect(writers, writers5)

# Check number of writers in train, validation, and test
train_writers <- writers[writers %in% handwriterRF::train$writer]
validation_writers <- writers[writers %in% handwriterRF::validation$writer]
test_writers <- writers[writers %in% handwriterRF::test$writer]

# Make sets ----

# Add writer lists
datasets <- list()
datasets$train_writers <- train_writers
datasets$validation_writers <- validation_writers
datasets$test_writers <- test_writers

# For each group - (0, 50], (50, 100], (100, 150], (150, 200], and (200, 250] -
# make train, validation, and test sets of pseudo-docs

datasets$pseudo_docs <- lapply(c("(0,50]", "(50,100]", "(100,150]", "(150,200]", "(200,250]"), function(gr) {
  new_group <- list()
  new_group$train <- sample %>%
    dplyr::filter(group == gr, writer %in% train_writers)
  new_group$validation <- sample %>%
    dplyr::filter(group == gr, writer %in% validation_writers)
  new_group$test <- sample %>%
    dplyr::filter(group == gr, writer %in% test_writers)
  new_group$length <- paste("Each document has", gr, "total graphs")
  return(new_group)
})
names(datasets$pseudo_docs) <- paste("length", c("(0,50]", "(50,100]", "(100,150]", "(150,200]", "(200,250]"))

# Make sets of "long" CVL prompts using prompts 1 and 3

datasets$long <- list()
datasets$long$train <- cvl_prompt_cfr %>%
  dplyr::filter(prompt %in% c(1, 3), writer %in% train_writers)
datasets$long$validation <- cvl_prompt_cfr %>%
  dplyr::filter(prompt %in% c(1, 3), writer %in% validation_writers)
datasets$long$test <- cvl_prompt_cfr %>%
  dplyr::filter(prompt %in% c(1, 3, 7, 8), writer %in% test_writers)
# Add range of total graphs for all long documents
min <- cvl_prompt_cfr %>%
  dplyr::filter(prompt %in% c(1, 3), writer %in% writers) %>%
  dplyr::select(tidyselect::all_of(c("total_graphs"))) %>%
  min()
max <- cvl_prompt_cfr %>%
  dplyr::filter(prompt %in% c(1, 3), writer %in% writers) %>%
  dplyr::select(tidyselect::all_of(c("total_graphs"))) %>%
  max()
datasets$long$length <- paste0("Each document has [", min, ",", max, "] total graphs")

saveRDS(datasets, "experiments/long_v_num_graphs/data/datasets.rds")
