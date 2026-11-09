#'
#' prognosticateSlot
#'
#' Apply the prognosticate method to a specified slot of a Ready4 object.
#'
#' @name prognosticateSlot-Ready4
#' @description Apply the prognosticate method to the value stored in a
#' specified slot of a Ready4 object.
#'
#' @param x An object that inherits from Ready4.
#' @param slot_nm_1L_chr Name of the slot to which the prognosticate method
#' should be applied (a character vector of length one).
#' @param ... Additional arguments passed to prognosticate.
#'
#' @return The object returned by applying prognosticate to the specified slot.
#'
#' @details
#' The Ready4 class itself is slotless. This method is therefore intended
#' primarily for classes that inherit from Ready4 and define one or more
#' slots.
#'
#' @rdname prognosticateSlot-methods
#' @aliases prognosticateSlot,Ready4-method
#' @export
methods::setMethod(
  "prognosticateSlot",
  "Ready4",
  function(x,
           slot_nm_1L_chr,
           ...) {

    y_xx <- prognosticate(
      procureSlot(
        x,
        slot_nm_1L_chr = slot_nm_1L_chr
      ),
      ...
    )

    return(y_xx)

  }
)
