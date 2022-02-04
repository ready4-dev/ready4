update_pt_fn_args_ls <- function(args_ls){
  arg_lgths_dbl <- args_ls %>% purrr::map_dbl(~length(.x))
  arg_max_lgth_1L_dbl <- max(arg_lgths_dbl)
  updated_args_ls <- purrr::map2(args_ls %>% unname(),
                                 unname(arg_lgths_dbl==0 & arg_lgths_dbl != arg_max_lgth_1L_dbl),
                                 ~{
                                   val_xx <- .x
                                   if(.y){
                                     val_xx <- paste0(
                                       ifelse(is.character(val_xx),
                                              "NA_character_",
                                              ifelse(is.integer(val_xx),
                                                     "NA_integer_",
                                                     ifelse(is.complex(val_xx),
                                                            "NA_complex_",
                                                            ifelse(is.numeric(val_xx),
                                                                   "NA_real_",
                                                                   ifelse(is.logical(val_xx),
                                                                          "NA",
                                                                          ifelse("list" %in% class(val_xx),
                                                                                 "list(NULL)",
                                                                                 "identity(.x)")))))))

                                     val_xx <- parse(text=val_xx) %>% eval()

                                   }
                                   val_xx
                                 }) %>%
    stats::setNames(names(args_ls))
  return(updated_args_ls)
}
update_tb_r3 <- function(tb_r3,
                         filter_cdn_1L_chr = NA_character_,
                         fn = NULL,
                         fn_env_ls = NULL,
                         slice_idxs_int = NA_integer_){
  if(!is.na(slice_idxs_int[1]))
    tb_r3 <- tb_r3 %>%
      dplyr::slice(slice_idxs_int)
  if(!is.na(filter_cdn_1L_chr[1]))
    tb_r3 <- tb_r3 %>%
      dplyr::filter(eval(parse(text=filter_cdn_1L_chr)))
  if(!is.null(fn_env_ls) & !is.null(fn))
    tb_r3 <- add_rows_from_fn_args(tb_r3,
                                   fn = fn,
                                   fn_env_ls = fn_env_ls)
  return(tb_r3)
}
