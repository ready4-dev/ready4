#' Ready4Public
#'
#' A virtual S4 class used to signify that a class extending the ready4 representational system is exclusively comprised of data appropriate for public dissemination.
#'
#' @include C4_Ready4Launch.R
#' @name Ready4Public-class
#' @rdname Ready4Public-class
#' @export Ready4Public
#' @exportClass Ready4Public
Ready4Public <- setClass("Ready4Public", contains = "Ready4Launch")
