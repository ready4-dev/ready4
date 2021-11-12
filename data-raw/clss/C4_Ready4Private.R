#' Ready4Private
#'
#' A virtual S4 class used to signify that a class extending the ready4 representational system contains data not intended for public dissemination.
#'
#' @include C4_Ready4Launch.R
#' @name Ready4Private-class
#' @rdname Ready4Private-class
#' @export Ready4Private
#' @exportClass Ready4Private
Ready4Private <- setClass("Ready4Private", contains = "Ready4Launch")
