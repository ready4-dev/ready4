#'
#' enhanceSlot
#' @name enhanceSlot-Ready4Module
#' @description enhanceSlot method applied to Ready4Module
#' @param x An object of class Ready4Module
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return A ready4 model module (an instance of a class that inherits from Ready4Module) of the same class as that supplied to the method.
#' @rdname enhanceSlot-methods
#' @aliases enhanceSlot,Ready4Module-method
#' @export

methods::setMethod("enhanceSlot", "Ready4Module", function(x,
                                                                slot_nm_1L_chr,
                                                                ...){
  slot_xx <- eval(parse(text = paste0("x@",slot_nm_1L_chr)))
  y_xx<- enhance(slot_xx,...)
  x <- renewSlot(x,
                 new_val_xx = y_xx,
                 slot_nm_1L_chr = slot_nm_1L_chr)
  return(x)
})
