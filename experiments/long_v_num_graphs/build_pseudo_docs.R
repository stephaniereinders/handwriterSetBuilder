# This script makes pseudo-docs by summing cluster fill counts from lines from
# CVL prompts 2 and 4

# The long_v_num_graphs experiment:
#   1. train: long = CVL prompts 1 and 3, short = CVL prompts 2 and 4
#   2. validation: long = CVL prompts 1 and 3, short = CVL prompts 2 and 4
#   3. test: long = CVL prompts 1, 3, 7, and 8, short = CVL prompts 2 and 4


devtools::load_all()


# Prompt 2 Pseudo-docs ----
prompt2 <- cvl_prompt_cfr %>%
  dplyr::filter(prompt == 2)

for (j in 1:8) {
  # Sample lines 50 times
  lines <- lapply(1:50, function(i)
    make_pseudo_docs(
      docnames = prompt2$docname,
      num_lines = j,
      drop_writers = TRUE,
      output = "rates",
      output_line_nums = TRUE
    )
  )
  lines <- do.call(rbind, lines)

  # Remove cases where the same line was used more than once for a writer
  lines <- lines %>% dplyr::distinct()

  saveRDS(lines, paste0("experiments/long_v_num_graphs/data/pseudo-docs/prompt2_pseudo_docs_", j, "lines.rds"))
}

# Prompt 4 Pseudo-docs ----
prompt4 <- cvl_prompt_cfr %>%
  dplyr::filter(prompt == 4)

for (j in 1:8) {
  # Sample lines 50 times
  lines <- lapply(1:50, function(i)
    make_pseudo_docs(
      docnames = prompt4$docname,
      num_lines = j,
      drop_writers = TRUE,
      output = "rates",
      output_line_nums = TRUE
    )
  )
  lines <- do.call(rbind, lines)

  # Remove cases where the same line was used more than once for a writer
  lines <- lines %>% dplyr::distinct()

  saveRDS(lines, paste0("experiments/long_v_num_graphs/data/pseudo-docs/prompt4_pseudo_docs_", j, "lines.rds"))
}

# Make master data frame of all pseudo-docs ----

# Load files into one data frame
files <- list.files("experiments/long_v_num_graphs/data/pseudo-docs", full.names = TRUE)
dfs <- lapply(files, readRDS)
df <- do.call(rbind, dfs)

# Add column for number of lines
df$num_lines <- sapply(df$line, length)

saveRDS(df, file = "experiments/long_v_num_graphs/data/pseudo-docs/all_pseudo_docs.rds")
