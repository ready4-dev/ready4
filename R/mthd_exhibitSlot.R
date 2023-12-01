#'
#' exhibitSlot
#' @name exhibitSlot-Ready4Module
#' @description exhibitSlot method applied to Ready4Module
#' @param x An object of class Ready4Module
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return Either a Ready4Module of the same class as that supplied to the method or no return value (when called purely for side effects).
#' @rdname exhibitSlot-methods
#' @aliases exhibitSlot,Ready4Module-method
#' @export
methods::setMethod("exhibitSlot", "Ready4Module", function (x,
                                                                 slot_nm_1L_chr,
                                                                 ...){
  slot_xx <- eval(parse(text = paste0("x@",slot_nm_1L_chr)))
  y_xx<- exhibit(slot_xx,...)
  return(y_xx)
})

