.get_cluster_rds <- function(main_dir, writer_subdirs = TRUE, drop_all_clusters = TRUE) {
  if (writer_subdirs) {
    writers <- list.files(main_dir, full.names = TRUE)
    files <- unlist(lapply(writers, function(w) list.files(w, full.names = TRUE)))
  } else {
    files <- list.files(main_dir, full.names = TRUE)
  }

  if (drop_all_clusters) {
    files <- files[!(basename(files) == "all_clusters.rds")]
  }

  return(files)
}

.load_cluster_rds <- function(paths) {
  clusters <- lapply(paths, readRDS)
  clusters <- do.call(rbind, clusters)
  return(clusters)
}

#' Add Columns to a CSAFE Prompt Data Frame
#'
#' Add writer, session, prompt, and rep columns to a CSAFE Cluster Fill Counts
#' (CFC) or Cluster Fill Rates (CFR) data frame. This function drops the doc
#' column if it is present.
#'
#' @param df A CSAFE CFC or CFR data frame
#' @returns An updated data frame
#' @noRd
.add_csafe_prompt_columns <- function(df) {
  # Drop writer and doc columns if they already exist. The writer column will be
  # added back in the next step when the docname columns is separated into
  # multiple columns.
  df <- df %>%
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::any_of(c("writer", "doc")))

  df <- df %>%
    tidyr::separate_wider_delim(tidyselect::all_of(c("docname")),
                                delim = "_",
                                names = c(
                                  "writer",
                                  "session",
                                  "prompt",
                                  "rep"
                                ),
                                cols_remove = FALSE
    )

  df <- df %>%
    dplyr::select(
      tidyselect::any_of(c("docname", "writer", "session", "prompt", "rep")),
      tidyselect::everything())

  return(df)
}


#' Add Columns to a CVL Prompt Data Frame
#'
#' Add writer and prompt columns to a CVL prompt Cluster Fill Counts
#' (CFC) or Cluster Fill Rates (CFR) data frame. This function drops the doc
#' column if it is present.
#'
#' @param df A CVL prompt CFC or CFR data frame
#' @returns An updated data frame
#' @noRd
.add_cvl_prompt_columns <- function(df) {
  # Drop writer and doc columns if they already exist. The writer column will be
  # added back in the next step when the docname columns is separated into
  # multiple columns.
  df <- df %>%
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::any_of(c("writer", "doc")))

  df <- df %>%
    tidyr::separate_wider_delim(tidyselect::all_of(c("docname")),
                                delim = "-",
                                names = c(
                                  "writer",
                                  "prompt"
                                ),
                                too_many = "drop",
                                cols_remove = FALSE
    )

  df <- df %>%
    dplyr::select(
      tidyselect::any_of(c("docname", "writer", "prompt")),
      tidyselect::everything())

  return(df)
}

#' Add Columns to a CVL Line Data Frame
#'
#' Add writer, prompt, and line columns to a CVL line Cluster Fill Counts
#' (CFC) or Cluster Fill Rates (CFR) data frame. This function drops the doc
#' column if it is present.
#'
#' @param df A CVL line CFC or CFR data frame
#' @returns An updated data frame
#' @noRd
.add_cvl_line_columns <- function(df) {
  # Drop writer and doc columns if they already exist. The writer column will be
  # added back in the next step when the docname columns is separated into
  # multiple columns.
  df <- df %>%
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::any_of(c("writer", "doc")))

  df <- df %>%
    tidyr::separate_wider_delim(tidyselect::all_of(c("docname")),
                                delim = "-",
                                names = c(
                                  "writer",
                                  "prompt",
                                  "line"
                                ),
                                too_many = "drop",
                                cols_remove = FALSE
    )

  # Drop file extension from line
  df$line <- stringr::str_replace(df$line, ".rds", "")
  df$line <- stringr::str_replace(df$line, ".png", "")

  df <- df %>%
    dplyr::select(
      tidyselect::any_of(c("docname", "writer", "prompt", "line")),
      tidyselect::everything())

  return(df)
}

#' Drop File Extension from Docname
#'
#' Drop the file extension from the docnames in a data frame.
#'
#' @param df A data frame with a docname column
#' @returns A data frame
#' @noRd
.drop_file_extension <- function(df) {
  df$docname <- stringr::str_replace(df$docname, ".png", "")
  return(df)
}

#' Drop Cropped from Docname
#'
#' Drop "-cropped" from the docnames in a data frame.
#'
#' @param df A data frame with a docname column
#' @returns A data frame
#' @noRd
.drop_cropped <- function(df) {
  df$docname <- stringr::str_replace(df$docname, "-cropped", "")
  return(df)
}
