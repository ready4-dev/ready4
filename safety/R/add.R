add_lups <- function(template_lup,
                     new_lup,
                     key_var_nm_1L_chr,
                     priority_lup_for_dupls_1L_chr = "template"){
  testit::assert("Look up tables must have same column names", names(template_lup)==names(new_lup))
  if(priority_lup_for_dupls_1L_chr == "template"){
    new_lup <- new_lup %>%
      dplyr::filter(!(!!rlang::sym(key_var_nm_1L_chr) %in% (template_lup %>% dplyr::pull(!!rlang::sym(key_var_nm_1L_chr)))))
    labels_chr <- Hmisc::label(template_lup) %>% unname()
  }else{
    template_lup <- template_lup %>%
      dplyr::filter(!(!!rlang::sym(key_var_nm_1L_chr) %in% (new_lup %>% dplyr::pull(!!rlang::sym(key_var_nm_1L_chr)))))
    labels_chr <- Hmisc::label(new_lup) %>% unname()
  }
  if(!all(labels_chr %>% unique() =="")){
    template_lup <- template_lup %>% remove_lbls_from_df()
    new_lup <- new_lup %>% remove_lbls_from_df()
    Hmisc::label(template_lup) <-  as.list(labels_chr %>% unname())
    Hmisc::label(new_lup) <- as.list(labels_chr %>% unname())
  }
  combined_lups <- dplyr::bind_rows(template_lup,
                                    new_lup) %>%
    dplyr::arrange(!!rlang::sym(key_var_nm_1L_chr))
  combined_lups <- combined_lups[rowSums(is.na(combined_lups)) != ncol(combined_lups), ]
  return(combined_lups)
}
