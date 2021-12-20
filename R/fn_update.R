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
