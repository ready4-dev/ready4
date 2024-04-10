#'
#' procureSlot
#' @name procureSlot-Ready4Module
#' @description procureSlot method applied to Ready4Module
#' @param x An object of class Ready4Module
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param use_procure_mthd_1L_lgl Use procure method (a length one logical vector)
#' @param ... Additional arguments
#' @return A ready4 model module (an instance of a class that inherits from Ready4Module) of the same class as that supplied to the method or an instance of a class contained in that Ready4Module's slots.
#' @rdname procureSlot-methods
#' @aliases procureSlot,Ready4Module-method
#' @export

#' @example man/examples/procureSlot_Ready4Module.R
methods::setMethod("procureSlot", "Ready4Module", function(x,
                                                           slot_nm_1L_chr,
                                                           use_procure_mthd_1L_lgl = FALSE,
                                                           ...){
  y_xx <- eval(parse(text = paste0("x@",slot_nm_1L_chr)))
  if(use_procure_mthd_1L_lgl)
    y_xx <- procure(y_xx,
                    ...)
  return(y_xx)
})
