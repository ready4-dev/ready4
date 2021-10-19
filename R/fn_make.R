make_files_tb <- function(paths_to_dirs_chr, # Make output into a class? Make fn a mthd?
                          recode_ls,
                          inc_fl_types_chr = NA_character_){
    files_tb <- purrr::map_dfr(paths_to_dirs_chr,
                               ~{
                                   files_chr_vec <- list.files(.x)
                                   if(!identical(files_chr_vec,character(0))){
                                       tb <- tibble::tibble(dir_chr = rep(.x,length(files_chr_vec)),
                                                            file_chr = files_chr_vec %>%
                                                                purrr::map_chr(~stringr::str_sub(.x,
                                                                                                 end = as.vector(stringi::stri_locate_last_regex(.x, "\\.")[,1])-1)),
                                                            file_type_chr = files_chr_vec %>%
                                                                purrr::map_chr(~stringr::str_sub(.x,
                                                                                                 start = as.vector(stringi::stri_locate_last_regex(.x, "\\.")[,1]))))

                                       tb
                                   }
                               })
    if(!is.na(inc_fl_types_chr))
        files_tb <- files_tb %>%
            dplyr::filter(file_type_chr %in% inc_fl_types_chr)
    files_tb <- files_tb %>%
        dplyr::filter(file_chr %in% names(recode_ls))
    description_chr <- purrr::map_chr(files_tb$file_chr,
                                      ~ {
                                          arg_ls <- append(list(EXPR=.x),recode_ls)
                                          rlang::exec(.fn = switch, !!!arg_ls)
                                      })
    files_tb <- files_tb %>%
        dplyr::mutate(description_chr = description_chr,
                      ds_file_ext_chr = purrr::map_chr(file_type_chr,
                                                       ~ ifelse(.x %in% c(".csv", ".xls",".xlsx"),
                                                                ".tab",
                                                                ".zip")))
    assertthat::are_equal(nrow(files_tb),
                          paste0(files_tb$file_chr,
                                 files_tb$file_type_chr) %>%
                              unique() %>%
                              length())
    return(files_tb)
}
#' Make argument description
#' @description make_arg_desc() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make argument description. The function returns Argument description (a character vector).
#' @param fn_args_chr Function arguments (a character vector)

#' @title make_gnrc_imports
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[methods]{GenericFunctions}}
#'  \code{\link[stats]{setNames}}
#' @rdname make_gnrc_imports
#' @export
#' @importFrom methods getGenerics
#' @importFrom stats setNames
make_gnrc_imports <- function(){
  generics_chr <- methods::getGenerics("package:ready4")@.Data
    # c(
    # "author","authorClasses","authorData","authorFunctions",
    # "characterize","enhance","ingest","investigate","manufacture",
    # "metamorphose","procure","prognosticate",
    # "reckon","renew","ratify","report","share")
  gnrc_imports_chr <- rep("ready4",length(generics_chr)) %>% stats::setNames(generics_chr)
  more_generics_chr <- ls("package:generics")
  more_generics_chr <- setdiff(more_generics_chr, generics_chr)
  gnrc_imports_chr <- c(gnrc_imports_chr,
                        rep("generics",length(more_generics_chr)) %>% stats::setNames(more_generics_chr))

  return(gnrc_imports_chr)
}
#' Make list phrase
#' @description make_list_phrase() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make list phrase. The function returns List phrase (a character vector of length one).
#' @param items_chr Items (a character vector)
#' @return List phrase (a character vector of length one)
#' @rdname make_list_phrase
#' @export
#' @importFrom stringr str_c
#' @importFrom stringi stri_replace_last
#' @keywords internal
make_list_phrase <- function (items_chr)
{
    list_phrase_1L_chr <- items_chr %>% stringr::str_c(sep = "",
        collapse = ", ") %>% stringi::stri_replace_last(fixed = ",",
        replacement = " and")
    return(list_phrase_1L_chr)
}
make_local_path_to_dv_data <- function(save_dir_path_1L_chr,
                                       fl_nm_1L_chr,
                                       save_fmt_1L_chr){
    path_chr <- paste0(ifelse(save_dir_path_1L_chr!="",paste0(save_dir_path_1L_chr,"/"),""),
                       fl_nm_1L_chr,
                       save_fmt_1L_chr)
    return(path_chr)
}
#' Make prompt
#' @description make_prompt() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make prompt. The function returns Response (a character vector of length one).
#' @param prompt_1L_chr Prompt (a character vector of length one)
#' @param options_chr Options (a character vector), Default: NULL
#' @param force_from_opts_1L_chr Force from opts (a character vector of length one), Default: F
#' @return Response (a character vector of length one)
#' @rdname make_prompt
#' @export
#' @keywords internal
make_prompt <- function (prompt_1L_chr, options_chr = NULL, force_from_opts_1L_chr = F)
{
    acknowledgement_1L_chr <- "This function is based on: https://debruine.github.io/posts/interactive-test/"
    con_conn <- getOption("prompt_opts.con", stdin())
    options_1L_chr <- paste(options_chr, collapse = "|")
    prompt_with_options_1L_chr <- paste0(prompt_1L_chr, " [",
        options_1L_chr, "]\n")
    cat(prompt_with_options_1L_chr)
    response_1L_chr <- readLines(con = con_conn, n = 1)
    if (!is.null(options_chr) & !response_1L_chr %in% options_chr &
        force_from_opts_1L_chr) {
        response_1L_chr <- make_prompt(prompt_1L_chr, options_chr,
            force_from_opts_1L_chr = T)
    }
    return(response_1L_chr)
}
