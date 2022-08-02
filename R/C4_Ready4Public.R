#' Ready4Public
#'
#' A virtual class denoting a module of the ready4 representational system hat is suitable for public dissemination in its current form.
#'
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @include C4_Ready4Module.R
#' @name Ready4Public-class
#' @rdname Ready4Public-class
#' @export Ready4Public
#' @exportClass Ready4Public
Ready4Public <- setClass("Ready4Public",
                         contains = "Ready4Module",
                         slots = c(dissemination_1L_chr = "character"),
                         prototype = list(dissemination_1L_chr = "Public"))
