#'
#' ratifySlot
#'
#' Apply the ratify method to a specified slot of a Ready4 object and
#' update the object with the result.
#'
#' @name ratifySlot-Ready4
#' @description Apply the ratify method to the value stored in a
#' specified slot of a Ready4 object and assign the resulting value
#' back to that slot.
#'
#' @param x An object that inherits from Ready4.
#' @param slot_nm_1L_chr Name of the slot to which the ratify method
#' should be applied (a character vector of length one).
#' @param ... Additional arguments passed to ratify.
#'
#' @return An updated version of the supplied object.
#'
#' @details
#' The Ready4 class itself is slotless. This method is therefore intended
#' primarily for classes that inherit from Ready4 and define one or more
#' slots.
#'
#' @rdname ratifySlot-methods
#' @aliases ratifySlot,Ready4-method
#' @export
methods::setMethod(
  "ratifySlot",
  "Ready4",
  function(x,
           slot_nm_1L_chr,
           ...) {

    slot_xx <- ratify(
      procureSlot(
        x,
        slot_nm_1L_chr = slot_nm_1L_chr
      ),
      ...
    )

    x <- renewSlot(
      x,
      slot_nm_1L_chr = slot_nm_1L_chr,
      new_val_xx = slot_xx
    )

    return(x)

  }
)
