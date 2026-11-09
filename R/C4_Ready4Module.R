#' Ready4Module
#'
#' Legacy module class maintained for backwards compatibility with
#' earlier versions of the ready4 framework.
#'
#' Ready4Module extends the slotless Ready4 root class and retains
#' the legacy dissemination_1L_chr slot used by existing ready4
#' ecosystem packages. New development should generally inherit
#' directly from Ready4 rather than Ready4Module.
#'
#' @slot dissemination_1L_chr Legacy dissemination metadata
#'   (a character vector of length one).
#'
#' @section Migration:
#' Ready4Module is retained to support existing ready4 ecosystem
#' objects. New framework development should preferentially inherit
#' from Ready4.
#'
#' @name Ready4Module-class
#' @rdname Ready4Module-class
#' @export Ready4Module
#' @exportClass Ready4Module
Ready4Module <- setClass(
  "Ready4Module",
  contains = "Ready4",
  slots = c(
    dissemination_1L_chr = "character"
  )
)

