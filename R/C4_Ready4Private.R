#' Ready4Private
#'
#' A module of the ready4 representational system that contains data not intended for public dissemination.
#'
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @include C4_Ready4Module.R
#' @name Ready4Private-class
#' @rdname Ready4Private-class
#' @export Ready4Private
#' @exportClass Ready4Private
Ready4Private <- setClass("Ready4Private",
                          contains = "Ready4Module",
                          slots = c(dissemination_1L_chr = "character"),
                          prototype = list(dissemination_1L_chr = "Private"))
