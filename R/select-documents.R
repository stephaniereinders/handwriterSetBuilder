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
select_short_long_csafe_docs <- function(df = csafe_prompt_cfr, writers, long_prompts, num_short, num_long) {
  csafe <- list()
  csafe$short <- select_csafe_docs(df = df, writers = writers, prompts = "pPHR", num = num_short)
  csafe$long <- select_csafe_docs(df = df, writers = writers, prompts = c("pLND", "pWOZ"), num = num_long)
  return(csafe)
}
