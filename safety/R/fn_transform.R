#' Transform character vector to numeric
#' @description transform_chr_to_num() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform character vector to numeric. The function returns Digits (an output object of multiple potential types).
#' @param digits_chr Digits (a character vector)
#' @return Digits (an output object of multiple potential types)
#' @rdname transform_chr_to_num
#' @export 
transform_chr_to_num <- function (digits_chr) 
{
    acknowledgement_1L_chr <- "This function is based on: https://stackoverflow.com/questions/24129124/how-to-determine-if-a-character-vector-is-a-valid-numeric-or-integer-vector"
    tfd_digits_chr <- suppressWarnings(as.character(digits_chr[!is.na(digits_chr)]))
    if (!identical(tfd_digits_chr, character(0)) & suppressWarnings(all(!is.na(as.numeric(tfd_digits_chr))))) {
        digits_xx <- as.numeric(as.character(digits_chr))
    }
    else {
        digits_xx <- digits_chr
    }
    return(digits_xx)
}
#' Transform class type list
#' @description transform_cls_type_ls() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform class type list. The function returns Tfmd class type (a list).
#' @param cls_type_ls Class type (a list)
#' @return Tfmd class type (a list)
#' @rdname transform_cls_type_ls
#' @export 
#' @importFrom purrr map_int map
transform_cls_type_ls <- function (cls_type_ls) 
{
    max_lngth_1L_int <- purrr::map_int(cls_type_ls, ~length(.x)) %>% 
        max()
    tfmd_cls_type_ls <- cls_type_ls %>% purrr::map(~{
        c(.x, rep(NA_character_, max_lngth_1L_int - length(.x)))
    })
    return(tfmd_cls_type_ls)
}
