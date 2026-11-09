#'
#' renewSlot
#'
#' Update the value stored in a specified slot of a Ready4 object.
#'
#' @name renewSlot-Ready4
#' @description Update the value stored in a specified slot of a
#' Ready4 object. If no replacement value is supplied, the renew
#' method is first applied to the slot value and the result is
#' written back to the slot.
#'
#' @param x An object that inherits from Ready4.
#' @param slot_nm_1L_chr Name of the slot to update
#' (a character vector of length one).
#' @param new_val_xx New value to assign to the specified slot.
#' If equal to "use_renew_mthd", the renew method is applied to
#' the current slot value and the result is assigned to the slot.
#' @param ... Additional arguments passed to renew when
#' new_val_xx is "use_renew_mthd".
#'
#' @return An updated version of the supplied object.
#'
#' @details
#' The Ready4 class itself is slotless. This method is therefore intended
#' primarily for classes that inherit from Ready4 and define one or more
#' slots.
#'
#' procureSlot and renewSlot provide the core slot access mechanisms used
#' by many Ready4 slot methods.
#'
#' @rdname renewSlot-methods
#' @aliases renewSlot,Ready4-method
#' @export
methods::setMethod(
  "renewSlot",
  "Ready4",
  function(x,
           slot_nm_1L_chr,
           new_val_xx = "use_renew_mthd",
           ...) {

    slot_nms_chr <- methods::slotNames(x)

    if (length(slot_nms_chr) == 0) {

      stop(
        paste0(
          class(x)[1],
          " does not define any slots. ",
          "renewSlot can only be used on classes that inherit from ",
          "Ready4 and define one or more slots."
        ),
        call. = FALSE
      )

    }

    if (!slot_nm_1L_chr %in% slot_nms_chr) {

      stop(
        paste0(
          "'",
          slot_nm_1L_chr,
          "' is not a slot of class ",
          class(x)[1],
          ". Available slots are: ",
          paste(
            slot_nms_chr,
            collapse = ", "
          )
        ),
        call. = FALSE
      )

    }

    if (identical(
      new_val_xx,
      "use_renew_mthd"
    )) {

      new_val_xx <- renew(
        procureSlot(
          x,
          slot_nm_1L_chr = slot_nm_1L_chr
        ),
        ...
      )

    }

    methods::slot(
      x,
      slot_nm_1L_chr
    ) <- new_val_xx

    return(x)

  }
)
