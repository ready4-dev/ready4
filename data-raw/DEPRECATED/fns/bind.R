bind_tables_from_loc_files <- function(paths_chr, # Add to ready4use ingest method
                                       force_numeric_1L_lgl = FALSE,
                                       force_tb_1L_lgl = FALSE,
                                       heading_rows_1L_int = 1L){
  table_xx <- purrr::map(paths_chr,
                         ~ get_table_from_loc_file(.x,
                                                   force_numeric_1L_lgl = force_numeric_1L_lgl,
                                                   force_tb_1L_lgl = force_tb_1L_lgl,
                                                   heading_rows_1L_int = heading_rows_1L_int)) %>%
    purrr::reduce(~dplyr::bind_rows(.x,.y ))
  return(table_xx)
}
