#'
#' reckonSlot
#' @name reckonSlot-Ready4Module
#' @description reckonSlot method applied to Ready4Module
#' @param x An object of class Ready4Module
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return A numeric class.
#' @rdname reckonSlot-methods
#' @aliases reckonSlot,Ready4Module-method
#' @export
methods::setMethod("reckonSlot", "Ready4Module", function (x,
                                                                 slot_nm_1L_chr,
                                                                 ...){
  slot_xx <- eval(parse(text = paste0("x@",slot_nm_1L_chr)))
  y_xx<- reckon(slot_xx,...)
  return(y_xx)
})

