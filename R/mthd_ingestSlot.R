#'
#' ingestSlot
#'
#' Apply the ingest method to a specified slot of a Ready4 object.
#'
#' @name ingestSlot-Ready4
#' @description Apply the ingest method to the value stored in a
#' specified slot of a Ready4 object.
#'
#' @param x An object that inherits from Ready4.
#' @param slot_nm_1L_chr Name of the slot to which the ingest method
#' should be applied (a character vector of length one).
#' @param ... Additional arguments passed to ingest.
#'
#' @return The object returned by applying ingest to the specified slot.
#'
#' @details
#' The Ready4 class itself is slotless. This method is therefore intended
#' primarily for classes that inherit from Ready4 and define one or more
#' slots.
#'
#' @rdname ingestSlot-methods
#' @aliases ingestSlot,Ready4-method
#' @export
methods::setMethod(
  "ingestSlot",
  "Ready4",
  function(x,
           slot_nm_1L_chr,
           ...) {

    y_xx <- ingest(
      procureSlot(
        x,
        slot_nm_1L_chr = slot_nm_1L_chr
      ),
      ...
    )

    return(y_xx)

  }
)
