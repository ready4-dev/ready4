#' Ready4Private
#'
#' A module of the ready4 framework tfor storing data that is not intended for public dissemination.
#'
#' @include C4_Ready4Module.R
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @name Ready4Private-class
#' @rdname Ready4Private-class
#' @export Ready4Private
#' @exportClass Ready4Private
Ready4Private <- setClass("Ready4Private",
                          contains = "Ready4Module",
                          slots = c(dissemination_1L_chr = "character"),
                          prototype = list(dissemination_1L_chr = "Private: Some or all of the data contained in this object is not appropriate for public dissemination. This object cannot be shared publicly in its current form."))
