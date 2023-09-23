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
                         case_when_false_1L_chr = NA_character_,
                         case_when_true_1L_chr = NA_character_,
                         case_when_true_ls = NULL,
                         case_when_var_1L_chr = NA_character_,
                         filter_cdn_1L_chr = NA_character_,
                         fn = NULL,
                         fn_env_ls = NULL,
                         slice_indcs_int = NA_integer_,
                         tf_false_val_1L_lgl = F){
  if(!is.na(slice_indcs_int[1]))
    tb_r3 <- tb_r3 %>%
      dplyr::slice(slice_indcs_int)
  if(!is.na(filter_cdn_1L_chr[1]))
    tb_r3 <- tb_r3 %>%
      dplyr::filter(eval(parse(text=filter_cdn_1L_chr)))
  if(!is.null(case_when_true_ls) | (!is.na(case_when_true_1L_chr[1]) & !is.na(case_when_var_1L_chr[1]))){
    if(!is.null(case_when_true_ls)){
      tb_r3 <- purrr::reduce(1:length(case_when_true_ls),.init = tb_r3,
                                       ~ .x %>% update_tb_r3(case_when_true_1L_chr = case_when_true_ls %>% purrr::pluck(.y),
                                                             case_when_var_1L_chr = names(case_when_true_ls)[.y],
                                                             case_when_false_1L_chr = names(case_when_true_ls)[.y],
                                                             tf_false_val_1L_lgl = T))
      case_when_true_1L_chr <- paste0(paste0(case_when_true_ls %>% unname() %>%
                                               purrr::map_chr(~stringr::str_sub(.x, end = stringr::str_locate(.x,"~")[1]-1) %>% stringr::str_trim()), collapse = " | ")," ~ ", case_when_var_1L_chr)
    }
    if(!tf_false_val_1L_lgl){
      case_when_default_1L_chr <- case_when_false_1L_chr
      case_when_false_1L_chr <- "case_when_default_1L_chr"

    }
    tb_r3 <- tb_r3 %>%
      dplyr::mutate(!!rlang::sym(case_when_var_1L_chr) := dplyr::case_when(eval(parse(text = case_when_true_1L_chr)),
                                                                           T ~ !!rlang::sym(case_when_false_1L_chr)))

  }

  if(!is.null(fn_env_ls) & !is.null(fn))
    tb_r3 <- add_rows_from_fn_args(tb_r3,
                                   fn = fn,
                                   fn_env_ls = fn_env_ls)
  return(tb_r3)
}
