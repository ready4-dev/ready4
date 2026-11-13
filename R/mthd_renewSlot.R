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

    # 1. Validate the complete path
    validate_slot_path(x, slot_nm_1L_chr)

    # 2. Determine the new value to assign
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

    # 3. Handle single vs nested slot assignment
    slot_path_chr <- strsplit(slot_nm_1L_chr, "@", fixed = TRUE)[[1]]
    n_slots <- length(slot_path_chr)

    if (n_slots == 1) {

      methods::slot(x, slot_path_chr[1]) <- new_val_xx

    } else {

      # Walk down the tree and store intermediate objects
      objs <- list(x)
      for (i in 1:(n_slots - 1)) {
        objs[[i + 1]] <- methods::slot(objs[[i]], slot_path_chr[i])
      }

      # Assign the new value to the deepest slot
      methods::slot(objs[[n_slots]], slot_path_chr[n_slots]) <- new_val_xx

      # Walk backwards up the tree to re-insert the modified objects
      for (i in (n_slots - 1):1) {
        methods::slot(objs[[i]], slot_path_chr[i]) <- objs[[i + 1]]
      }

      x <- objs[[1]]

    }

    return(x)

  }
)
