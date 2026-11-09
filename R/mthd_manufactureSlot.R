#'
#' manufactureSlot
#'
#' Apply the manufacture method to a specified slot of a Ready4 object.
#'
#' @name manufactureSlot-Ready4
#' @description Apply the manufacture method to the value stored in a
#' specified slot of a Ready4 object.
#'
#' @param x An object that inherits from Ready4.
#' @param slot_nm_1L_chr Name of the slot to which the manufacture method
#' should be applied (a character vector of length one).
#' @param ... Additional arguments passed to manufacture.
#'
#' @return The object returned by applying manufacture to the specified slot.
#'
#' @details
#' The Ready4 class itself is slotless. This method is therefore intended
#' primarily for classes that inherit from Ready4 and define one or more
#' slots.
#'
#' @rdname manufactureSlot-methods
#' @aliases manufactureSlot,Ready4-method
#' @export
methods::setMethod(
  "manufactureSlot",
  "Ready4",
  function(x,
           slot_nm_1L_chr,
           ...) {

    y_xx <- manufacture(
      procureSlot(
        x,
        slot_nm_1L_chr = slot_nm_1L_chr
      ),
      ...
    )

    return(y_xx)

  }
)
