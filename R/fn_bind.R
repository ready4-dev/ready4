#' Bind tables from local files
#' @description bind_tables_from_loc_files() is a Bind function that binds two objects together to create a composite object. Specifically, this function implements an algorithm to bind tables from local files. The function returns Table (an output object of multiple potential types).
#' @param paths_chr Paths (a character vector)
#' @param force_numeric_1L_lgl Force numeric (a logical vector of length one), Default: F
#' @param force_tb_1L_lgl Force tibble (a logical vector of length one), Default: F
#' @param heading_rows_1L_int Heading rows (an integer vector of length one), Default: 1
#' @return Table (an output object of multiple potential types)
#' @rdname bind_tables_from_loc_files
#' @export 
#' @importFrom purrr map reduce
#' @importFrom dplyr bind_rows
#' @keywords internal
bind_tables_from_loc_files <- function (paths_chr, force_numeric_1L_lgl = F, force_tb_1L_lgl = F, 
    heading_rows_1L_int = 1L) 
{
    table_xx <- purrr::map(paths_chr, ~get_table_from_loc_file(.x, 
        force_numeric_1L_lgl = force_numeric_1L_lgl, force_tb_1L_lgl = force_tb_1L_lgl, 
        heading_rows_1L_int = heading_rows_1L_int)) %>% purrr::reduce(~dplyr::bind_rows(.x, 
        .y))
    return(table_xx)
}
