#' Select CSAFE Documents
#'
#' Randomly select CSAFE handwriting documents from the csafe_prompt_cfr data
#' frame. `select_csafe_docs` filters the data frame for the specified writer
#' IDs and prompts. Then, the function randomly selects the specified number of
#' documents per writer and prompt.
#'
#' @param writers A vector of CSAFE writer IDs
#' @param prompts A vector of CSAFE prompt codes c("pLND", "pPHR", "pWOZ")
#' @param num Number of documents to sample per writer and prompt code
#' @param drop_extra_columns TRUE or FALSE. If TRUE, will keep the docname,
#'   writer and cluster columns and drop all other columns. If FALSE, will keep
#'   all columns.
#'
#' @returns A data frame
#' @export
#'
#' @examples
#' csafe <- select_csafe_docs(
#'   writers = unique(handwriterRF::train$writer),
#'   prompts = "pPHR",
#'   num = 2,
#'   drop_extra_columns = FALSE
#' )
#'
#' @md
select_csafe_docs <- function(writers, prompts, num, drop_extra_columns = TRUE) {
  # Prevent note "no visible binding for global variable"
  writer <- prompt <- NULL

  # FUTURE: could allow users to input a different CSAFE cfr data frame
  df = csafe_prompt_cfr

  csafe <- df %>%
    dplyr::filter(writer %in% writers, prompt %in% prompts) %>%
    dplyr::group_by(writer, prompt) %>%
    dplyr::slice_sample(n = num) %>%
    dplyr::ungroup()

  if (drop_extra_columns) {
    csafe <- .drop_extra_labels(csafe)
  }

  return(csafe)
}


#' Select Short and Long CSAFE Documents
#'
#' Randomly select short and long CSAFE documents from the csafe_prompt_cfr data
#' frame. The common phrase prompts are the only option for short documents.
#' Choose from London Letter and Wizard of Oz prompts for long documents.
#'
#' @param writers A vector of CSAFE writer IDs
#' @param long_prompts A vector of CSAFE prompt codes c("pLND", "pWOZ")
#' @param num_short Number of common phrase "pPHR" prompts to sample per writer
#' @param num_long Number of prompts to sample per writer and prompt
#' @param drop_extra_columns TRUE or FALSE. If TRUE, will keep the docname,
#'   writer and cluster columns and drop all other columns. If FALSE, will keep
#'   all columns.
#'
#' @returns A list of two data frames
#' @export
#'
#' @examples
#' csafe_train <- select_long_short_csafe_docs(
#'   writers = unique(handwriterRF::train$writer),
#'   long_prompts = c("pLND", "pWOZ"),
#'   num_short = 2,
#'   num_long = 1
#' )
#'
#' @md
select_long_short_csafe_docs <- function(
    writers,
    long_prompts,
    num_short,
    num_long,
    drop_extra_columns = TRUE) {

  # FUTURE: could allow users to input a different CSAFE cfr data frame
  df = csafe_prompt_cfr

  csafe <- list()
  csafe$short <- select_csafe_docs(
    writers = writers,
    prompts = "pPHR",
    num = num_short,
    drop_extra_columns = drop_extra_columns
  )

  csafe$long <- select_csafe_docs(
    writers = writers,
    prompts = c("pLND", "pWOZ"),
    num = num_long,
    drop_extra_columns = drop_extra_columns
  )

  return(csafe)
}

#' Select CVL Documents
#'
#' Randomly select CVL handwriting documents from the cvl_prompt_cfr data frame.
#' `select_cvl_docs` filters the data frame for the specified writer IDs. Then,
#' the function randomly selects the specified number of documents per writer
#' and prompt.
#'
#' @param writers A vector of CSAFE writer IDs
#' @param num Number of documents to sample per writer
#' @param drop_German_prompt TRUE or FALSE. If TRUE, the prompt written in
#'   German will be dropped from the data frame before randomly selecting
#'   prompts. If FALSE, the German prompt could be randomly selected.
#' @param drop_writers_missing_docs TRUE or FALSE. Some writers might not have
#'   `num` prompts to select. If TRUE, these writers will be dropped from the
#'   data frame. If FALSE, these writers will be included in the data frame.
#' @param drop_extra_columns TRUE or FALSE. If TRUE, will keep the docname,
#'   writer and cluster columns and drop all other columns. If FALSE, will keep
#'   all columns.
#'
#' @returns A data frame
#' @export
#'
#' @examples
#' cvl <- select_cvl_docs(
#'   writers = unique(handwriterRF::train$writer),
#'   num = 2
#' )
#'
#' @md
select_cvl_docs <- function(
    writers,
    num,
    drop_German_prompt = TRUE,
    drop_writers_missing_docs = TRUE,
    drop_extra_columns = TRUE) {

  # Prevent note "no visible binding for global variable"
  prompt <- writer <- n <- NULL

  # FUTURE: could allow users to input a different CVL cfr data frame
  df = cvl_prompt_cfr

  if (drop_German_prompt) {
    df <- df %>%
      dplyr::filter(prompt != 6)
  }

  # Randomly select num prompts from each writer
  df <- df %>%
    dplyr::filter(writer %in% writers) %>%
    dplyr::group_by(writer) %>%
    dplyr::slice_sample(n = num) %>%
    dplyr::ungroup()

  if (drop_writers_missing_docs) {
    # Find writers who do not have num docs
    writers_missing_docs <- df %>%
      dplyr::group_by(writer) %>%
      dplyr::summarize(n = dplyr::n()) %>%
      dplyr::filter(n != num) %>%
      dplyr::pull(writer)
    if (length(writers_missing_docs) > 0) {
      df <- df %>%
        dplyr::filter(!(writer %in% writers_missing_docs))
      message("One or more writer dropped from data frame because they are missing documents")
    }
  }

  if (drop_extra_columns) {
    df <- .drop_extra_labels(df)
  }

  return(df)
}

#' Select Short and Long CVL Documents
#'
#' Randomly select CVL documents to construct two data frames - short and long -
#' from the cvl_prompt_cfr and cvl_line_cfc data frames:
#' 1. Randomly select num_short + num_long documents from each writer
#' 2. Assign the first num_short docs selected for each writer to the short data
#' frame and assign the remaining documents to the long data frame
#' 3. For each document in the short data frame, randomly select num_lines from the document
#' and build a pseudo document with [`make_pseudo_docs()`]
#'
#' @param writers A vector of CVL writer IDs
#' @param num_short Number of short documents to construct from lines for each
#'   writer
#' @param num_long Number of long documents to sample per writer
#' @param num_lines Number of lines to use to construct each short document
#' @param drop_German_prompt TRUE or FALSE. If TRUE, the prompt written in
#'   German will be dropped from the data frame before randomly selecting
#'   prompts. If FALSE, the German prompt could be randomly selected.
#' @param drop_extra_columns TRUE or FALSE. If TRUE, will keep the docname,
#'   writer and cluster columns and drop all other columns. If FALSE, will keep
#'   all columns.
#'
#' @returns A list of two data frames
#' @export
#'
#' @examples
#' cvl_train <- select_long_short_cvl_docs(
#'   writers = unique(handwriterRF::train$writer),
#'   num_short = 2,
#'   num_long = 2,
#'   num_lines = 1,
#'   drop_German_prompt = TRUE
#' )
#'
#' @md
select_long_short_cvl_docs <- function(
    writers,
    num_short,
    num_long,
    num_lines,
    drop_German_prompt = TRUE,
    drop_extra_columns = TRUE) {

  # Prevent note "no visible binding for global variable"
  prompt_docname <- group <- NULL

  # FUTURE: select_cvl_docs uses cvl_prompt_cfr, but this could be changed
  prompt_df <- select_cvl_docs(
    writers = writers,
    num = (num_short + num_long),
    drop_German_prompt = drop_German_prompt,
    drop_writers_missing_docs = TRUE
  )

  # WARNING: drop_writers_missing_docs must be TRUE in `select_cvl_docs()`
  # otherwise some writers in the data frame could have more documents than
  # others which would cause the group assignments to be wrong
  prompt_df$group <- rep(c(rep("short", num_short), rep("long", num_long)), length.out = nrow(prompt_df))

  cvl <- list()
  cvl$long <- prompt_df %>%
    dplyr::filter(group == "long")

  cvl$short <- prompt_df %>%
    dplyr::filter(group == "short")
  # FUTURE: select_cvl_lines uses cvl_line_cfc, but this could be changed
  cvl$short <- select_cvl_lines(docnames = unique(cvl$short$docname), num_lines = num_lines)
  cvl$short <- make_pseudo_docs(cvl$short, output = "rates")

  if (drop_extra_columns) {
    cvl$long <- .drop_extra_labels(cvl$long)
    cvl$short <- .drop_extra_labels(cvl$short)
  }

  return(cvl)
}

#' Select Lines from CVL Documents
#'
#' Randomly select lines from CVL handwriting documents in cvl_line_cfc.
#' `select_cvl_lines` filters the data frame for the input document names. Then,
#' the function randomly selects the specified number of lines per document.
#'
#' @param docnames A vector of CVL document names
#' @param num_lines Number of lines to sample per document
#'
#' @returns A data frame
#' @export
#'
#' @examples
#' cvl <- select_cvl_lines(
#'   docnames = c("0001-1", "0003-2", "0005-3"),
#'   num_lines = 2
#' )
#'
#' @md
select_cvl_lines <- function(docnames, num_lines) {
  # Prevent note "no visible binding for global variable"
  prompt_docname <- group <- NULL

  # FUTURE: could allow users to input a different CVL lines cfc data frame
  lines_df = cvl_line_cfc

  # Add prompt docname to lines_df data frame
  lines_df$prompt_docname <- paste(lines_df$writer, lines_df$prompt, sep = "-")
  lines_df <- lines_df %>% dplyr::select(tidyselect::any_of(c("docname", "prompt_docname", "writer", "prompt", "line")), dplyr::everything())

  lines_df <- lines_df %>% dplyr::filter(prompt_docname %in% docnames)

  lines_df <- lines_df %>%
    dplyr::group_by(prompt_docname) %>%
    dplyr::slice_sample(n = num_lines)

  return(lines_df)
}

#' Make Pseudo Documents from Lines from CVL Documents
#'
#' Make pseudo documents from a data frame of writer profiles for lines from CVL
#' documents. The writer profiles are summed for each writer to make a pseudo
#' document.
#'
#' @param lines_df A data frame of cluster fill counts for lines from CVL
#'   documents
#' @param output Either "counts" to return cluster fill counts or "rates" to
#'   retun cluster fill rates
#' @returns A data frame of cluster fill counts or cluster fill rates
#' @export
#'
#' @examples
#' cvl <- select_cvl_lines(
#'   docnames = c("0001-1", "0003-2", "0005-3"),
#'   num_lines = 2
#' )
#' counts <- make_pseudo_docs(cvl)
#' rates <- make_pseudo_docs(cvl, output = "rates")
#'
#' @md
make_pseudo_docs <- function(lines_df, output = "counts") {
  clusters <- .get_cluster_cols(lines_df)

  pseudo_docs <- lines_df %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(c("prompt_docname", "writer", "prompt"))) %>%
    dplyr::summarize(
      dplyr::across(
        tidyselect::all_of(clusters),
        function(x) sum(x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
  colnames(pseudo_docs)[colnames(pseudo_docs) == "prompt_docname"] <- "docname"

  # Add "pseudo" to the end of the docnames so they won't have the same names as
  # the corresponding prompts
  pseudo_docs$docname <- paste0(pseudo_docs$docname, "-pseudo")

  if (output == "rates") {
    labels <- pseudo_docs[.get_label_cols(pseudo_docs)]
    clusters <- pseudo_docs[.get_cluster_cols(pseudo_docs)]
    total_graphs <- .get_total_graphs(pseudo_docs)
    rates <- clusters / total_graphs
    colnames(rates) <- paste0("cluster", colnames(rates))
    pseudo_docs <- cbind(labels, total_graphs, rates)
  }

  return(pseudo_docs)
}

#' Make CSAFE and CVL Documents Set
#'
#' Randomly select short and long CSAFE and CVL documents to construct two data
#' frames.
#'
#' @param writers A vector of CSAFE and CVL writer IDs to select from the input
#'   data frames
#' @param csafe_long_prompts A vector of CSAFE prompt codes c("pLND", "pWOZ")
#' @param csafe_num_short Number of common phrase "pPHR" prompts to sample per
#'   writer
#' @param csafe_num_long Number of prompts to sample per writer and prompt
#' @param cvl_num_short Number of short documents to construct from lines for each
#'   writer
#' @param cvl_num_long Number of long documents to sample per writer
#' @param cvl_num_lines Number of lines to use to construct each short document
#' @param drop_German_prompt TRUE or FALSE. If TRUE, the prompt written in
#'   German will be dropped from the data frame before randomly selecting
#'   prompts. If FALSE, the German prompt could be randomly selected.
#'
#' @returns A list of two data frames
#' @export
#'
#' @examples
#' make_csafe_cvl_set(
#'   all_writers = c("w0001", "w0002", "0023", "0024"),
#'   csafe_long_prompts = c("pLND", "pWOZ"),
#'   csafe_num_long = 1,
#'   csafe_num_short = 2,
#'   cvl_num_long = 2,
#'   cvl_num_short = 2,
#'   cvl_num_lines = 1,
#'   cvl_drop_German_prompt = TRUE
#' )
#'
#' @md
make_csafe_cvl_set <- function(
    all_writers,
    csafe_long_prompts = c("pLND", "pWOZ"),
    csafe_num_long = 1,
    csafe_num_short = 2,
    cvl_num_long = 2,
    cvl_num_short = 2,
    cvl_num_lines = 1,
    cvl_drop_German_prompt = TRUE) {

  csafe <- select_long_short_csafe_docs(
    writers = all_writers,
    long_prompts = csafe_long_prompts,
    num_long = csafe_num_long,
    num_short = csafe_num_short,
    drop_extra_columns = TRUE
  )

  cvl <- select_long_short_cvl_docs(
    writers = all_writers,
    num_long = cvl_num_long,
    num_short = cvl_num_short,
    num_lines = cvl_num_lines,
    drop_extra_columns = TRUE,
    drop_German_prompt = cvl_drop_German_prompt
  )

  long <- rbind(csafe$long, cvl$long)
  short <- rbind(csafe$short, cvl$short)

  return(list(long = long, short = short))
}

#' Add Total Graphs Columns to Results Data Frame
#'
#' The results data frame created by [`handwriterRF::compare_writer_profiles()`]
#' does not include total graphs columns. `add_total_graphs_to_results()` adds
#' total_graphs1 and total_graphs2 columns to results to show the number of
#' graphs in the documents in docname1 and docname2, respectively.
#'
#' @param test_set The test set used to get the results data frame. The test set
#'   must be created with [`make_csafe_cvl_set()`].
#' @param results A results data frame created with
#'   [`handwriterRF::compare_writer_profiles()`] and test_set.
#'
#' @returns A data frame
#' @export
#'
#' @examples
#' \dontrun{
#' test <- make_csafe_cvl_set(all_writers = unique(handwriterRF::test$writer))
#' results <- handwriterRF::compare_writer_profiles(test$long, test$short, score_only = FALSE)
#' add_total_graphs_to_results(test, results)
#' }
#'
#' @md
add_total_graphs_to_results <- function(test_set, results) {
  # Make lookup table with columns docnames and total_graphs1
  docnames <- c(test_set$long$docname, test_set$short$docname)
  total_graphs <- c(test_set$long$total_graphs, test_set$short$total_graphs)
  total_graphs_lookup <- data.frame(docnames = docnames, total_graphs = total_graphs)

  # Add total_graphs1 column to results by looking up docname1 in lookup table
  results <- results %>% dplyr::left_join(total_graphs_lookup, by = dplyr::join_by("docname1" == "docnames"))
  # Change column to total_graphs1
  colnames(results)[colnames(results) == "total_graphs"] <- "total_graphs1"

  # Add total_graphs2 column to results by looking up docname2 in lookup table
  results <- results %>% dplyr::left_join(total_graphs_lookup, by = dplyr::join_by("docname2" == "docnames"))
  # Change column to total_graphs2
  colnames(results)[colnames(results) == "total_graphs"] <- "total_graphs2"

  return(results)
}
