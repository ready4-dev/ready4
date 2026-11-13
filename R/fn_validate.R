#' validate_slot_path
#'
#' Internal helper to validate a single or nested slot reference.
#'
#' @param x An object that inherits from Ready4.
#' @param slot_nm_1L_chr Name of the slot path to validate (e.g., "slotA" or "slotA@slotB").
#'
#' @return invisible(TRUE) if the path is fully valid. Throws an error otherwise.
#' @keywords internal
validate_slot_path <- function(x, slot_nm_1L_chr) {

  slot_path_chr <- strsplit(slot_nm_1L_chr, "@", fixed = TRUE)[[1]]
  y_xx <- x

  for (i in seq_along(slot_path_chr)) {
    current_slot_chr <- slot_path_chr[i]
    current_slot_nms_chr <- methods::slotNames(y_xx)

    if (length(current_slot_nms_chr) == 0) {

      path_context <- if (i == 1) {
        paste0(class(y_xx)[1], " does not define any slots. ")
      } else {
        paste0("The object retrieved at '",
               paste(slot_path_chr[1:(i-1)], collapse = "@"),
               "' (class ", class(y_xx)[1], ") does not define any slots. ")
      }

      stop(
        paste0(
          path_context,
          "This method can only be used on objects that define one or more slots."
        ),
        call. = FALSE
      )
    }

    if (!current_slot_chr %in% current_slot_nms_chr) {

      path_context <- if (i == 1) {
        paste0("class ", class(y_xx)[1])
      } else {
        paste0("class ", class(y_xx)[1], " (retrieved at '",
               paste(slot_path_chr[1:(i-1)], collapse = "@"), "')")
      }

      stop(
        paste0(
          "'",
          current_slot_chr,
          "' is not a slot of ",
          path_context,
          ". Available slots are: ",
          paste(current_slot_nms_chr, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    # Extract for the next iteration (unless we are on the final step)
    if (i < length(slot_path_chr)) {
      y_xx <- methods::slot(y_xx, current_slot_chr)
    }
  }

  return(invisible(TRUE))
}
