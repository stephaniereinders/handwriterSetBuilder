devtools::load_all()

cvl_prompt_cfr %>% dplyr::group_by(prompt) %>% dplyr::summarize(n = dplyr::n())
