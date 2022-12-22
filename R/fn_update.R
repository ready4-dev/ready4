#' Update prototype function arguments list
#' @description update_pt_fn_args_ls() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update prototype function arguments list. Function argument args_ls specifies the object to be updated. The function returns Updated arguments (a list).
#' @param args_ls Arguments (a list)
#' @return Updated arguments (a list)
#' @rdname update_pt_fn_args_ls
#' @export 
#' @importFrom purrr map_dbl map2
#' @importFrom stats setNames
#' @keywords internal
update_pt_fn_args_ls <- function (args_ls) 
{
    arg_lgths_dbl <- args_ls %>% purrr::map_dbl(~length(.x))
    arg_max_lgth_1L_dbl <- max(arg_lgths_dbl)
    updated_args_ls <- purrr::map2(args_ls %>% unname(), unname(arg_lgths_dbl == 
        0 & arg_lgths_dbl != arg_max_lgth_1L_dbl), ~{
        val_xx <- .x
        if (.y) {
            val_xx <- paste0(ifelse(is.character(val_xx), "NA_character_", 
                ifelse(is.integer(val_xx), "NA_integer_", ifelse(is.complex(val_xx), 
                  "NA_complex_", ifelse(is.numeric(val_xx), "NA_real_", 
                    ifelse(is.logical(val_xx), "NA", ifelse("list" %in% 
                      class(val_xx), "list(NULL)", "identity(.x)")))))))
            val_xx <- parse(text = val_xx) %>% eval()
        }
        val_xx
    }) %>% stats::setNames(names(args_ls))
    return(updated_args_ls)
}
#' Update tibble ready4 S3
#' @description update_tb_r3() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update tibble ready4 s3. Function argument tb_r3 specifies the object to be updated. Argument filter_cdn_1L_chr provides the object to be updated. The function returns Tibble ready4 S3 (a ready4 S3 extension of tibble).
#' @param tb_r3 Tibble ready4 S3 (a ready4 S3 extension of tibble)
#' @param filter_cdn_1L_chr Filter condition (a character vector of length one), Default: 'NA'
#' @param fn Function (a function), Default: NULL
#' @param fn_env_ls Function (a list of environments), Default: NULL
#' @param slice_indcs_int Slice indices (an integer vector), Default: NA
#' @return Tibble ready4 S3 (a ready4 S3 extension of tibble)
#' @rdname update_tb_r3
#' @export 
#' @importFrom dplyr slice filter
#' @keywords internal
update_tb_r3 <- function (tb_r3, filter_cdn_1L_chr = NA_character_, fn = NULL, 
    fn_env_ls = NULL, slice_indcs_int = NA_integer_) 
{
    if (!is.na(slice_indcs_int[1])) 
        tb_r3 <- tb_r3 %>% dplyr::slice(slice_indcs_int)
    if (!is.na(filter_cdn_1L_chr[1])) 
        tb_r3 <- tb_r3 %>% dplyr::filter(eval(parse(text = filter_cdn_1L_chr)))
    if (!is.null(fn_env_ls) & !is.null(fn)) 
        tb_r3 <- add_rows_from_fn_args(tb_r3, fn = fn, fn_env_ls = fn_env_ls)
    return(tb_r3)
}
