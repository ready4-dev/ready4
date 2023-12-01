#'
#' renewSlot
#' @name renewSlot-Ready4Module
#' @description renewSlot method applied to Ready4Module
#' @param x An object of class Ready4Module
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param new_val_xx New value (slot dependent object type), Default 'use_renew_mthd'
#' @param ... Additional arguments
#' @return A Ready4Module of the same class as that supplied to the method.
#' @rdname renewSlot-methods
#' @aliases renewSlot,Ready4Module-method
#' @export
methods::setMethod("renewSlot", "Ready4Module", function(x,
                                                         slot_nm_1L_chr,
                                                         new_val_xx = "use_renew_mthd",
                                                         ...){
  if(identical(new_val_xx,
               "use_renew_mthd")){
    new_val_xx <- renew(procureSlot(x,
                                    slot_nm_1L_chr = slot_nm_1L_chr),
                        ...)

  }
  eval(parse(text = paste0("x@",slot_nm_1L_chr," <- new_val_xx")))
  return(x)
})
