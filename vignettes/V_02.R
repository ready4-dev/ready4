## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----results='hide', message=FALSE--------------------------------------------
library("ready4") 

## ----echo=F-------------------------------------------------------------------
# get_fn_descs <- function(fn_nms_chr = NULL,
#                          functions_tb = NULL,
#                          gh_repo_1L_chr = "ready4-dev/ready4",
#                          gh_tag_1L_chr = "Documentation_0.0",
#                          return_1L_chr = "all"){
#   if(is.null(functions_tb))
#     functions_tb <- get_functions_tb(gh_repo_1L_chr = gh_repo_1L_chr,
#                              gh_tag_1L_chr = gh_tag_1L_chr,
#                              return_1L_chr = return_1L_chr)
#   if(is.null(fn_nms_chr)){
#         fn_descs_chr <- functions_tb$fn_type_desc_chr
#   }else{
#       fn_descs_chr <- purrr::map_chr(fn_nms_chr,
#                                  ~get_from_lup_obj(functions_tb,
#                                                    match_value_xx = .x,
#                                                    match_var_nm_1L_chr = "fn_type_nm_chr",               
#                                                    target_var_nm_1L_chr = "fn_type_desc_chr"))
#   }
#   return(fn_descs_chr)
# }
# NEED TO DEPRECATE GET_MTHD_TITLE IN READY4FUN


## ----warning=F----------------------------------------------------------------
x <- make_methods_tb()

## -----------------------------------------------------------------------------
print_methods(x,
              return_1L_chr = "core")

## -----------------------------------------------------------------------------
print_methods(x,
              return_1L_chr = "slot")

## -----------------------------------------------------------------------------
print_methods(x,
              exclude_mthds_for_chr = "Ready4Module",
              return_1L_chr = "extended")

## -----------------------------------------------------------------------------
get_methods()

