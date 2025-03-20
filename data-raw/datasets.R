## code to prepare `csafe_prompt_cfr` dataset goes here

devtools::load_all()


# csafe_prompt_cfr --------------------------------------------------------

main_dir = "~/Documents/non_version_control/handwriting_datasets/CSAFE_Handwriting_Database/clusters"
paths <- .get_cluster_rds(main_dir = main_dir, writer_subdirs = TRUE, drop_all_clusters = TRUE)
clusters <- .load_cluster_rds(paths)
cfr <- handwriter::get_cluster_fill_rates(clusters)
csafe_prompt_cfr <- .add_csafe_prompt_columns(cfr)
usethis::use_data(csafe_prompt_cfr, overwrite = TRUE)


# cvl_prompt_cfr ----------------------------------------------------------

main_dir = "~/Documents/non_version_control/handwriting_datasets/CVL/pages_cropped/clusters"
paths <- .get_cluster_rds(main_dir = main_dir, writer_subdirs = TRUE, drop_all_clusters = TRUE)
clusters <- .load_cluster_rds(paths)
cfr <- handwriter::get_cluster_fill_rates(clusters)
cvl_prompt_cfr <- .add_cvl_prompt_columns(cfr)
cvl_prompt_cfr <- .drop_file_extension(cvl_prompt_cfr)
cvl_prompt_cfr <- .drop_cropped(cvl_prompt_cfr)
usethis::use_data(cvl_prompt_cfr, overwrite = TRUE)


# cvl_line_cfc ------------------------------------------------------------

main_dir = "~/Documents/non_version_control/handwriting_datasets/CVL/lines/clusters"
paths <- .get_cluster_rds(main_dir = main_dir, writer_subdirs = TRUE, drop_all_clusters = TRUE)
clusters <- .load_cluster_rds(paths)
cfc <- handwriter::get_cluster_fill_counts(clusters)
cvl_line_cfc <- .add_cvl_line_columns(cfc)
cvl_line_cfc <- .drop_file_extension(cvl_line_cfc)
usethis::use_data(cvl_line_cfc, overwrite = TRUE)
