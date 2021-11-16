#'
#' enhanceSlot
#' @name enhanceSlot-Ready4Launch
#' @description enhanceSlot method applied to Ready4Launch
#' @param x An object of class Ready4Launch
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return NULL
#' @rdname enhanceSlot-methods
#' @aliases enhanceSlot,Ready4Launch-method
#' @export

methods::setMethod("enhanceSlot", "Ready4Launch", function(x,
                                                                slot_nm_1L_chr,
                                                                ...){
  slot_xx <- eval(parse(text = paste0("x@",slot_nm_1L_chr)))
  y_xx<- enhance(slot_xx,...)
  x <- renewSlot(x,
                 new_val_xx = y_xx,
                 slot_nm_1L_chr = slot_nm_1L_chr)
  return(x)
})
