#'
#' procureSlot
#'
#' Retrieve the value stored in a specified slot of a Ready4 object.
#'
#' @name procureSlot-Ready4
#' @description Retrieve the value stored in a specified slot of a
#' Ready4 object. Optionally apply the procure method to the retrieved
#' value before returning it.
#'
#' @param x An object that inherits from Ready4.
#' @param slot_nm_1L_chr Name of the slot to retrieve
#' (a character vector of length one).
#' @param use_procure_mthd_1L_lgl Logical indicating whether the
#' procure method should be applied to the slot value before it
#' is returned.
#' @param ... Additional arguments passed to procure when
#' use_procure_mthd_1L_lgl is TRUE.
#'
#' @return The value stored in the specified slot, or the object
#' returned by applying procure to that value.
#'
#' @details
#' The Ready4 class itself is slotless. This method is therefore intended
#' primarily for classes that inherit from Ready4 and define one or more
#' slots.
#'
#' procureSlot and renewSlot provide the core slot access mechanisms used
#' by many Ready4 slot methods.
#'
#' @rdname procureSlot-methods
#' @aliases procureSlot,Ready4-method
#' @export
methods::setMethod(
  "procureSlot",
  "Ready4",
  function(x,
           slot_nm_1L_chr,
           use_procure_mthd_1L_lgl = FALSE,
           ...) {

    slot_nms_chr <- methods::slotNames(x)

    if (length(slot_nms_chr) == 0) {

      stop(
        paste0(
          class(x)[1],
          " does not define any slots. ",
          "procureSlot can only be used on classes that inherit from ",
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
          paste(slot_nms_chr,
                collapse = ", ")
        ),
        call. = FALSE
      )

    }

    y_xx <- methods::slot(
      x,
      slot_nm_1L_chr
    )

    if (use_procure_mthd_1L_lgl) {

      y_xx <- procure(
        y_xx,
        ...
      )

    }

    return(y_xx)

  }
)
