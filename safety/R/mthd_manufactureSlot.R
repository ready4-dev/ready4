#'
#' manufactureSlot
#' @name manufactureSlot-Ready4Module
#' @description manufactureSlot method applied to Ready4Module
#' @param x An object of class Ready4Module
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return An object that is not the the same class as that supplied to the method.
#' @rdname manufactureSlot-methods
#' @aliases manufactureSlot,Ready4Module-method
#' @export
methods::setMethod("manufactureSlot", "Ready4Module", function (x,
                                                                 slot_nm_1L_chr,
                                                                 ...){
  slot_xx <- eval(parse(text = paste0("x@",slot_nm_1L_chr)))
  y_xx<- manufacture(slot_xx,...)
  return(y_xx)
})

