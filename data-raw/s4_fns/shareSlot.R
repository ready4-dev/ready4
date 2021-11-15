shareSlot_Ready4Launch <- function(x,
                                   slot_nm_1L_chr,
                                   ...){
  slot_xx <- eval(parse(text = paste0("x@",slot_nm_1L_chr)))
  share(slot_xx,...)
}
