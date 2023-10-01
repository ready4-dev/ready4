remove_lbls_from_df <- function(data_df){
  acknowledgement_1L_chr <- "This function is based on: https://rdrr.io/github/dlindholm/doctoR/src/R/clear_labels.R"
  unlabelled_data_df <- purrr::reduce(1:ncol(data_df),
                                      .init = data_df,
                                      ~ {
                                        class(.x[[.y]]) <- setdiff(class(.x[[.y]]), 'labelled')
                                        attr(.x[[.y]],"label") <- NULL
                                        .x
                                      }
  )
  return(unlabelled_data_df)
}
