#'
#' prognosticateSlot
#' @name prognosticateSlot-Ready4Module
#' @description prognosticateSlot method applied to Ready4Module
#' @param x An object of class Ready4Module
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return A Ready4Module.
#' @rdname prognosticateSlot-methods
#' @aliases prognosticateSlot,Ready4Module-method
#' @export
methods::setMethod("prognosticateSlot", "Ready4Module", function (x,
                                                                 slot_nm_1L_chr,
                                                                 ...){
  slot_xx <- eval(parse(text = paste0("x@",slot_nm_1L_chr)))
  y_xx<- prognosticate(slot_xx,...)
  return(y_xx)
})

