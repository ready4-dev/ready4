methods::setMethod("procureSlot", "Ready4Module", function(x,
                                                           slot_nm_1L_chr){
  y_xx <- eval(parse(text = paste0("x@",slot_nm_1L_chr)))
  return(y_xx)
})
