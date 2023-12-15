#'
#' characterizeSlot
#' @name characterizeSlot-Ready4Module
#' @description characterizeSlot method applied to Ready4Module
#' @param x An object of class Ready4Module
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return Either a ready4 model module (an instance of a class that inherits from Ready4Module) of the same class as that supplied to the method or a data.frame, tibble or other table class.
#' @rdname characterizeSlot-methods
#' @aliases characterizeSlot,Ready4Module-method
#' @export
methods::setMethod("characterizeSlot", "Ready4Module", function (x,
                                                                 slot_nm_1L_chr,
                                                                 ...){
  slot_xx <- eval(parse(text = paste0("x@",slot_nm_1L_chr)))
  y_xx<- characterize(slot_xx,...)
  return(y_xx)
})

