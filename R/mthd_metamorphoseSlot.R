#'
#' metamorphoseSlot
#' @name metamorphoseSlot-Ready4Launch
#' @description metamorphoseSlot method applied to Ready4Launch
#' @param x An object of class Ready4Launch
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return NULL
#' @rdname metamorphoseSlot-methods
#' @aliases metamorphoseSlot,Ready4Launch-method
#' @export

methods::setMethod("metamorphoseSlot", "Ready4Launch", function(x,
                                                                slot_nm_1L_chr,
                                                                ...){
  slot_xx <- eval(parse(text = paste0("x@",slot_nm_1L_chr)))
  y_xx <- metamorphose(slot_xx,...)
  return(y_xx)
})
