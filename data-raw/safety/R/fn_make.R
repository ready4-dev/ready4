#' Make files tibble
#' @description make_files_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make files tibble. The function returns Files (a tibble).
#' @param paths_to_dirs_chr Paths to directories (a character vector)
#' @param recode_ls Recode (a list)
#' @param inc_fl_types_chr Include file types (a character vector), Default: 'NA'
#' @return Files (a tibble)
#' @rdname make_files_tb
#' @export
#' @importFrom purrr map_dfr map_chr
#' @importFrom tibble tibble
#' @importFrom stringr str_sub
#' @importFrom stringi stri_locate_last_regex
#' @importFrom dplyr filter mutate
#' @importFrom rlang exec
#' @importFrom assertthat are_equal
#' @keywords internal
make_files_tb <- function (paths_to_dirs_chr, recode_ls, inc_fl_types_chr = NA_character_)
{
  files_tb <- purrr::map_dfr(paths_to_dirs_chr, ~{
    files_chr_vec <- list.files(.x)
    if (!identical(files_chr_vec, character(0))) {
      tb <- tibble::tibble(dir_chr = rep(.x, length(files_chr_vec)),
                           file_chr = files_chr_vec %>% purrr::map_chr(~stringr::str_sub(.x,
                                                                                         end = as.vector(stringi::stri_locate_last_regex(.x,
                                                                                                                                         "\\.")[, 1]) - 1)), file_type_chr = files_chr_vec %>%
                             purrr::map_chr(~stringr::str_sub(.x, start = as.vector(stringi::stri_locate_last_regex(.x,
                                                                                                                    "\\.")[, 1]))))
      tb
    }
  })
  if (!is.na(inc_fl_types_chr))
    files_tb <- files_tb %>% dplyr::filter(file_type_chr %in%
                                             inc_fl_types_chr)
  files_tb <- files_tb %>% dplyr::filter(file_chr %in% names(recode_ls))
  description_chr <- purrr::map_chr(files_tb$file_chr, ~{
    arg_ls <- append(list(EXPR = .x), recode_ls)
    rlang::exec(.fn = switch, !!!arg_ls)
  })
  files_tb <- files_tb %>% dplyr::mutate(description_chr = description_chr,
                                         ds_file_ext_chr = purrr::map_chr(file_type_chr, ~ifelse(.x %in%
                                                                                                   c(".csv", ".xls", ".xlsx"), ".tab", ".zip")))
  assertthat::are_equal(nrow(files_tb), paste0(files_tb$file_chr,
                                               files_tb$file_type_chr) %>% unique() %>% length())
  return(files_tb)
}
#' Make list phrase
#' @description make_list_phrase() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make list phrase. The function returns List phrase (a character vector of length one).
#' @param items_chr Items (a character vector)
#' @return List phrase (a character vector of length one)
#' @rdname make_list_phrase
#' @export
#' @importFrom lifecycle deprecate_soft
#' @importFrom stringr str_c
#' @importFrom stringi stri_replace_last
#' @keywords internal
make_list_phrase <- function (items_chr)
{
  lifecycle::deprecate_soft("0.0.0.9446", "ready4fun::make_list_phrase()",
                            "make_list_phrase()")
  list_phrase_1L_chr <- items_chr %>% stringr::str_c(sep = "",
                                                     collapse = ", ") %>% stringi::stri_replace_last(fixed = ",",
                                                                                                     replacement = " and")
  return(list_phrase_1L_chr)
}
#' Make local path to dataverse data
#' @description make_local_path_to_dv_data() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make local path to dataverse data. The function returns Path (a character vector).
#' @param save_dir_path_1L_chr Save directory path (a character vector of length one)
#' @param fl_nm_1L_chr File name (a character vector of length one)
#' @param save_fmt_1L_chr Save format (a character vector of length one)
#' @return Path (a character vector)
#' @rdname make_local_path_to_dv_data
#' @export
#' @keywords internal
make_local_path_to_dv_data <- function (save_dir_path_1L_chr, fl_nm_1L_chr, save_fmt_1L_chr)
{
  path_chr <- paste0(ifelse(save_dir_path_1L_chr != "", paste0(save_dir_path_1L_chr,
                                                               "/"), ""), fl_nm_1L_chr, save_fmt_1L_chr)
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
#' @importFrom lifecycle deprecate_soft
#' @importFrom ready4 make_prompt
#' @keywords internal
make_prompt <- function (prompt_1L_chr, options_chr = NULL, force_from_opts_1L_chr = F)
{
  lifecycle::deprecate_soft("0.0.0.9446", "ready4fun::make_prompt()",
                            "make_prompt()")
  acknowledgement_1L_chr <- "This function is based on: https://debruine.github.io/posts/interactive-test/"
  con_conn <- getOption("prompt_opts.con", stdin())
  options_1L_chr <- paste(options_chr, collapse = "|")
  prompt_with_options_1L_chr <- paste0(prompt_1L_chr, " [",
                                       options_1L_chr, "]\n")
  cat(prompt_with_options_1L_chr)
  response_1L_chr <- readLines(con = con_conn, n = 1)
  if (!is.null(options_chr) & !response_1L_chr %in% options_chr &
      force_from_opts_1L_chr) {
    response_1L_chr <- make_prompt(prompt_1L_chr,
                                           options_chr, force_from_opts_1L_chr = T)
  }
  return(response_1L_chr)
}
