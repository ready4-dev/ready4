#'
#' exhibitSlot
#'
#' Apply the exhibit method to a specified slot of a Ready4 object.
#'
#' @name exhibitSlot-Ready4
#' @description Apply the exhibit method to the value stored in a
#' specified slot of a Ready4 object.
#'
#' @param x An object that inherits from Ready4.
#' @param slot_nm_1L_chr Name of the slot to which the exhibit method
#' should be applied (a character vector of length one).
#' @param ... Additional arguments passed to exhibit.
#'
#' @return The object returned by applying exhibit to the specified slot.
#'
#' @details
#' The Ready4 class itself is slotless. This method is therefore intended
#' primarily for classes that inherit from Ready4 and define one or more
#' slots.
#'
#' @rdname exhibitSlot-methods
#' @aliases exhibitSlot,Ready4-method
#' @export
methods::setMethod(
  "exhibitSlot",
  "Ready4",
  function(x,
           slot_nm_1L_chr,
           ...) {

    y_xx <- exhibit(
      procureSlot(
        x,
        slot_nm_1L_chr = slot_nm_1L_chr
      ),
      ...
    )

    return(y_xx)

  }
)
