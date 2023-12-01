#'
#' ratifySlot
#' @name ratifySlot-Ready4Module
#' @description ratifySlot method applied to Ready4Module
#' @param x An object of class Ready4Module
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return A Ready4Module of the same class as that supplied to the method.
#' @rdname ratifySlot-methods
#' @aliases ratifySlot,Ready4Module-method
#' @export

methods::setMethod("ratifySlot", "Ready4Module", function(x,
                                                          slot_nm_1L_chr,
                                                                 ...){
  slot_xx <- eval(parse(text = paste0("x@",slot_nm_1L_chr)))
  slot_xx <- ratify(slot_xx,...)
  x <- renewSlot(x,
                 new_val_xx = slot_xx,
                 slot_nm_1L_chr = slot_nm_1L_chr)
  return(x)
})
