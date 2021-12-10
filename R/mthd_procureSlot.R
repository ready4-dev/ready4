#'
#' procureSlot
#' @name procureSlot-Ready4Module
#' @description procureSlot method applied to Ready4Module
#' @param x An object of class Ready4Module
#' @param slot_nm_1L_chr Slot name (a length one character vector)
#' @param ... Additional arguments
#' @return NULL
#' @rdname procureSlot-methods
#' @aliases procureSlot,Ready4Module-method
#' @export
methods::setMethod("procureSlot", "Ready4Module", function(x,
                                                           slot_nm_1L_chr){
  y_xx <- eval(parse(text = paste0("x@",slot_nm_1L_chr)))
  return(y_xx)
})
