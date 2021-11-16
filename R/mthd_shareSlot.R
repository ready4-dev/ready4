#'
#' shareSlot
#' @name shareSlot-Ready4Launch
#' @description shareSlot method applied to Ready4Launch
#' @param x An object of class Ready4Launch
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return NULL
#' @rdname shareSlot-methods
#' @aliases shareSlot,Ready4Launch-method
#' @export
methods::setMethod("shareSlot", "Ready4Launch", function(x,
                                                         slot_nm_1L_chr,
                                                         ...){
  slot_xx <- eval(parse(text = paste0("x@",slot_nm_1L_chr)))
  share(slot_xx,...)
})
