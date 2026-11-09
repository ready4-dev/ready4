#'
#' shareSlot
#'
#' Apply the share method to a specified slot of a Ready4 object.
#'
#' @name shareSlot-Ready4
#' @description Apply the share method to the value stored in a
#' specified slot of a Ready4 object.
#'
#' @param x An object that inherits from Ready4.
#' @param slot_nm_1L_chr Name of the slot to which the share method
#' should be applied (a character vector of length one).
#' @param ... Additional arguments passed to share.
#'
#' @return The object returned by applying share to the specified slot.
#'
#' @details
#' The Ready4 class itself is slotless. This method is therefore intended
#' primarily for classes that inherit from Ready4 and define one or more
#' slots.
#'
#' @rdname shareSlot-methods
#' @aliases shareSlot,Ready4-method
#' @export
methods::setMethod(
  "shareSlot",
  "Ready4",
  function(x,
           slot_nm_1L_chr,
           ...) {

    y_xx <- share(
      procureSlot(
        x,
        slot_nm_1L_chr = slot_nm_1L_chr
      ),
      ...
    )

    return(y_xx)

  }
)
