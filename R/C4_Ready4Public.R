#' Ready4Public
#'
#' A module of the ready4 representational system hat is suitable for public dissemination in its current form.
#'
#' @include C4_Ready4Module.R
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @name Ready4Public-class
#' @rdname Ready4Public-class
#' @export Ready4Public
#' @exportClass Ready4Public
Ready4Public <- setClass("Ready4Public",
                         contains = "Ready4Module",
                         slots = c(dissemination_1L_chr = "character"),
                         prototype = list(dissemination_1L_chr = "Public: All of the data contained in this object is appropriate for public dissemination. This object can be shared publicly in its current form, under licensing terms defined by the data custodian."))
