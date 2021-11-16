#'
#' characterizeSlot
#' @name characterizeSlot-Ready4Launch
#' @description characterizeSlot method applied to Ready4Launch
#' @param x An object of class Ready4Launch
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return NULL
#' @rdname characterizeSlot-methods
#' @aliases characterizeSlot,Ready4Launch-method
#' @export
methods::setMethod("characterizeSlot", "Ready4Launch", function (x,
                                                                 slot_nm_1L_chr,
                                                                 ...){
  slot_xx <- eval(parse(text = paste0("x@",slot_nm_1L_chr)))
  y_xx<- characterize(slot_xx,...)
  return(y_xx)
})

