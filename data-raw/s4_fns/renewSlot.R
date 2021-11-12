renewSlot_Ready4Launch <- function(x,
                                   new_val_xx,
                                   slot_nm_1L_chr){
  eval(parse(text = paste0("x@",slot_nm_1L_chr," <- new_val_xx")))
  return(x)
}
