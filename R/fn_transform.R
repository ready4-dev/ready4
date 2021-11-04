#' Transform class type list
#' @description transform_cls_type_ls() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform class type list. Function argument cls_type_ls specifies the object to be updated. The function returns Tfmd class type (a list).
#' @param cls_type_ls Class type (a list)
#' @return Tfmd class type (a list)
#' @rdname transform_cls_type_ls
#' @export 
#' @importFrom purrr map_int map
#' @keywords internal
transform_cls_type_ls <- function (cls_type_ls) 
{
    max_lngth_1L_int <- purrr::map_int(cls_type_ls, ~length(.x)) %>% 
        max()
    tfmd_cls_type_ls <- cls_type_ls %>% purrr::map(~{
        c(.x, rep(NA_character_, max_lngth_1L_int - length(.x)))
    })
    return(tfmd_cls_type_ls)
}
