enhanceSlot_Ready4Launch <- function(x,
                                     slot_nm_1L_chr,
                                     ...){
  slot_xx <- eval(parse(text = paste0("x@",slot_nm_1L_chr)))
  y_xx<- enhance(slot_xx,...)
  x <- renewSlot(x,
                 new_val_xx = y_xx,
                 slot_nm_1L_chr = slot_nm_1L_chr)
  return(x)
}
