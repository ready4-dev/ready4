#'
#' ratifySlot
#' @name ratifySlot-Ready4Launch
#' @description ratifySlot method applied to Ready4Launch
#' @param x An object of class Ready4Launch
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return NULL
#' @rdname ratifySlot-methods
#' @aliases ratifySlot,Ready4Launch-method
#' @export

methods::setMethod("ratifySlot", "Ready4Launch", function(x,
                                                                 slot_nm_1L_chr,
                                                                 ...){
  slot_xx <- eval(parse(text = paste0("x@",slot_nm_1L_chr)))
  slot_xx <- ratify(slot_xx,...)
  x <- renewSlot(x,
                 new_val_xx = slot_xx,
                 slot_nm_1L_chr = slot_nm_1L_chr)
  return(x)
})
