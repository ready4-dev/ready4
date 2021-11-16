#'
#' renewSlot
#' @name renewSlot-Ready4Launch
#' @description renewSlot method applied to Ready4Launch
#' @param x An object of class Ready4Launch
#' @param new_val_xx New value (slot dependent object type)
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @return NULL
#' @rdname renewSlot-methods
#' @aliases renewSlot,Ready4Launch-method
#' @export
methods::setMethod("renewSlot", "Ready4Launch", function(x,
                                                         new_val_xx,
                                                         slot_nm_1L_chr){
  eval(parse(text = paste0("x@",slot_nm_1L_chr," <- new_val_xx")))
  return(x)
})
