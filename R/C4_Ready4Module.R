#' Ready4Module
#'
#' A module of the ready4 representational system.
#'
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @name Ready4Module-class
#' @rdname Ready4Module-class
#' @export Ready4Module
#' @exportClass Ready4Module
Ready4Module <- setClass("Ready4Module",
                         slots = c(dissemination_1L_chr = "character"),
                         prototype = list(dissemination_1L_chr = "Undefined: Some or all of the data contained in this object may be appropriate for public dissemination, but this has yet to be confirmed or detailed by the relevant data custodians."))

