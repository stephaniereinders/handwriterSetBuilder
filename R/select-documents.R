#' Select CSAFE Documents
#'
#' Randomly select CSAFE handwriting documents from a data frame.
#' `select_csafe_docs` filters the data frame for the specified writer IDs and
#' prompts. Then, the function randomly selects the specified number of
#' documents per writer and prompt.
#'
#' @param df A data frame of CSAFE handwriting documents. The data frame must
#'   contain writer and prompt columns
#' @param writers A vector of CSAFE writer IDs
#' @param prompts A vector of CSAFE prompt codes c("pLND", "pPHR", "pWOZ")
#' @param num Number of documents to sample per writer and prompt code
#'
#' @returns A data frame
#' @export
#'
#' @examples
#' csafe <- select_csafe_docs(
#'   df = csafe_prompt_cfr,
#'   writers = unique(handwriterRF::train$writer),
#'   prompts = "pPHR",
#'   num = 2
#' )
#'
#' @md
select_csafe_docs <- function(df = csafe_prompt_cfr, writers, prompts, num) {
  # Prevent note "no visible binding for global variable"
  writer <- prompt <- NULL

  csafe <- df %>%
    dplyr::filter(writer %in% writers, prompt %in% prompts) %>%
    dplyr::group_by(writer, prompt) %>%
    dplyr::slice_sample(n = num) %>%
    dplyr::ungroup()

  return(csafe)
}


#' Select Short and Long CSAFE Documents
#'
#' Randomly select short and long CSAFE documents to construct two data frames.
#' The common phrase prompts are the only option for short documents. Choose
#' from London Letter and Wizard of Oz prompts for long documents.
#'
#' @param df A data frame of CSAFE handwriting documents. The data frame must
#'   contain writer and prompt columns
#' @param writers A vector of CSAFE writer IDs
#' @param long_prompts A vector of CSAFE prompt codes c("pLND", "pWOZ")
#' @param num_short Number of common phrase "pPHR" prompts to sample per writer
#' @param num_long Number of prompts to sample per writer and prompt
#'
#' @returns A list of two data frames
#' @export
#'
#' @examples
#' csafe_train <- select_short_long_csafe_docs(
#'   df = csafe_prompt_cfr,
#'   writers = unique(handwriterRF::train$writer),
#'   long_prompts = c("pLND", "pWOZ"),
#'   num_short = 2,
#'   num_long = 1
#' )
#'
#' @md
select_short_long_csafe_docs <- function(df = csafe_prompt_cfr, writers, long_prompts, num_short, num_long) {
  csafe <- list()
  csafe$short <- select_csafe_docs(df = df, writers = writers, prompts = "pPHR", num = num_short)
  csafe$long <- select_csafe_docs(df = df, writers = writers, prompts = c("pLND", "pWOZ"), num = num_long)
  return(csafe)
}

#' Select CVL Documents
#'
#' Randomly select CVL handwriting documents from a data frame.
#' `select_cvl_docs` filters the data frame for the specified writer IDs. Then,
#' the function randomly selects the specified number of documents per writer
#' and prompt.
#'
#' @param df A data frame of CSAFE handwriting documents. The data frame must
#'   contain writer and prompt columns
#' @param writers A vector of CSAFE writer IDs
#' @param num Number of documents to sample per writer
#' @param drop_German_prompt TRUE or FALSE. If TRUE, the prompt written in
#'   German will be dropped from the data frame before randomly selecting
#'   prompts. If FALSE, the German prompt could be randomly selected.
#' @param drop_writers_missing_docs TRUE or FALSE. Some writers might not have
#'   `num` prompts to select. If TRUE, these writers will be dropped from the
#'   data frame. If FALSE, these writers will be included in the data frame.
#'
#' @returns A data frame
#' @export
#'
#' @examples
#' cvl <- select_cvl_docs(
#'   df = cvl_prompt_cfr,
#'   writers = unique(handwriterRF::train$writer),
#'   num = 2
#' )
#'
#' @md
select_cvl_docs <- function(df = cvl_prompt_cfr, writers, num, drop_German_prompt = TRUE, drop_writers_missing_docs = TRUE) {
  # Prevent note "no visible binding for global variable"
  prompt <- writer <- n <- NULL

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

  return(df)
}

#' Select Short and Long CVL Documents
#'
#' Randomly select CVL documents to construct two data frames - short and long -
#' with the following steps:
#' 1. Randomly select num_short + num_long documents
#' from each writer
#' 2. Assign the first num_short docs selected for each writer
#' to the short data frame and assign the remaining documents to the long data
#' frame
#' 3. For each document in the short data frame, randomly select num_lines
#' from the document and build a pseudo document with [`make_pseudo_docs()`]
#'
#' @param df A data frame of CVL handwriting documents. The data frame must
#'   contain writer and prompt columns
#' @param lines_df A data frame of lines of CVL handwriting documents. The
#'   individual lines from the documents in df must be included in lines_df.
#'   Lines from other documents may be included in lines_df as well, but they
#'   will not be used
#' @param writers A vector of CVL writer IDs
#' @param num_short Number of short documents to construct from lines for each writer
#' @param num_long Number of long documents to sample per writer
#' @param num_lines Number of lines to use to construct each short document
#' @param drop_German_prompt TRUE or FALSE. If TRUE, the prompt written in
#'   German will be dropped from the data frame before randomly selecting
#'   prompts. If FALSE, the German prompt could be randomly selected.
#'
#' @returns A list of two data frames
#' @export
#'
#' @examples
#' cvl_train <- select_short_long_cvl_docs(
#'   df = cvl_prompt_cfr,
#'   lines_df = cvl_line_cfc,
#'   writers = unique(handwriterRF::train$writer),
#'   num_short = 2,
#'   num_long = 2,
#'   num_lines = 1,
#'   drop_German_prompt = TRUE
#' )
#'
#' @md
select_short_long_cvl_docs <- function(
    df = cvl_prompt_cfr,
    lines_df = cvl_line_cfc,
    writers,
    num_short,
    num_long,
    num_lines,
    drop_German_prompt = TRUE) {

  # Prevent note "no visible binding for global variable"
  prompt_docname <- group <- NULL

  df <- select_cvl_docs(
    df = df,
    writers = writers,
    num = (num_short + num_long),
    drop_German_prompt = drop_German_prompt,
    drop_writers_missing_docs = TRUE
  )

  # WARNING: drop_writers_missing_docs must be TRUE in `select_cvl_docs()`
  # otherwise some writers in the data frame could have more documents than
  # others which would cause the group assignments to be wrong
  df$group <- rep(c(rep("short", num_short), rep("long", num_long)), length.out = nrow(df))

  cvl <- list()
  cvl$long <- df %>%
    dplyr::filter(group == "long")

  cvl$short <- df %>%
    dplyr::filter(group == "short")
  cvl$short <- select_cvl_lines(docnames = unique(cvl$short$docname), lines_df = cvl_line_cfc, num_lines = num_lines)
  cvl$short <- make_pseudo_docs(cvl$short)

  return(cvl)
}

#' Select Lines from CVL Documents
#'
#' Randomly select lines from CVL handwriting documents in a data frame.
#' `select_cvl_lines` filters the data frame for the document names. Then,
#' the function randomly selects the specified number of lines per document.
#'
#' @param docnames A vector of CVL document names
#' @param lines_df A data frame of lines from CVL documents
#' @param num_lines Number of lines to sample per document
#'
#' @returns A data frame
#' @export
#'
#' @examples
#' cvl <- select_cvl_lines(
#'   docnames = c("0001-1", "0003-2", "0005-3"),
#'   lines_df = cvl_line_cfc,
#'   num_lines = 2
#' )
#'
#' @md
select_cvl_lines <- function(docnames, lines_df = cvl_line_cfc, num_lines) {
  # Prevent note "no visible binding for global variable"
  prompt_docname <- group <- NULL

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
#' @param lines_df A data frame of writer profiles for lines from CVL documents
#'
#' @returns A data frame
#' @export
#'
#' @examples
#' cvl <- select_cvl_lines(
#'   docnames = c("0001-1", "0003-2", "0005-3"),
#'   lines_df = cvl_line_cfc,
#'   num_lines = 2
#' )
#' docs <- make_pseudo_docs(cvl)
#'
#' @md
make_pseudo_docs <- function(lines_df) {

  clusters <- .get_cluster_cols(lines_df)

  pseudo_docs <- lines_df %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(c("prompt_docname", "writer", "prompt"))) %>%
    dplyr::summarize(
      dplyr::across(tidyselect::all_of(clusters),
                    function(x) sum(x, na.rm = TRUE)),
      .groups = "drop")

  return(pseudo_docs)
}
