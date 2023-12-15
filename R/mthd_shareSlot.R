#'
#' shareSlot
#' @name shareSlot-Ready4Module
#' @description shareSlot method applied to Ready4Module
#' @param x An object of class Ready4Module
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return Either a ready4 model module (an instance of a class that inherits from Ready4Module) of the same class as that supplied to the method or no value (when called purely for side effects).
#' @rdname shareSlot-methods
#' @aliases shareSlot,Ready4Module-method
#' @export
methods::setMethod("shareSlot", "Ready4Module", function(x,
                                                         slot_nm_1L_chr,
                                                         ...){
  slot_xx <- eval(parse(text = paste0("x@",slot_nm_1L_chr)))
  share(slot_xx,...)
})
