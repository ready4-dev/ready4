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
make_list_phrase <- function(items_chr){
  list_phrase_1L_chr <- items_chr %>%
    stringr::str_c(sep="",collapse=", ") %>%
    stringi::stri_replace_last(fixed=",", replacement = " and")
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
make_modules_tb <- function(pkg_extensions_tb = NULL,
                            cls_extensions_tb = NULL,
                            gh_repo_1L_chr = "ready4-dev/ready4",
                            gh_tag_1L_chr = "Documentation_0.0"){
  if(is.null(pkg_extensions_tb))
    pkg_extensions_tb <- make_pkg_extensions_tb()
  if(is.null(cls_extensions_tb))
    cls_extensions_tb <- get_cls_extensions(pkg_extensions_tb,
                                            gh_repo_1L_chr = gh_repo_1L_chr,
                                            gh_tag_1L_chr = gh_tag_1L_chr)
  modules_tb <- dplyr::inner_join(cls_extensions_tb,
                                  pkg_extensions_tb) %>%
    dplyr::mutate(Class = purrr::pmap(list(pt_ns_chr,
                                           type_chr,
                                           old_class_lgl),

                                      ~ {
                                        kableExtra::cell_spec(..2,
                                                              "html",
                                                              link = paste0("https://ready4-dev.github.io/",
                                                                            ..1,
                                                                            "/reference/",
                                                                            ifelse(..3,
                                                                                   ..2,
                                                                                   paste0(..2,"-class")),
                                                                            ".html"
                                                              ))


                                      }
    )) %>%
    dplyr::mutate(Examples = purrr::map2(Vignette_URLs,
                                         type_chr,
                                         ~ get_examples(.x,
                                                        term_1L_chr = .y))) %>%
    dplyr::mutate(Description = purrr::map2_chr(Class,
                                                old_class_lgl,
                                                ~{
                                                  rvest::read_html((.x %>%
                                                                      stringr::str_match("href=\"\\s*(.*?)\\s*\" style"))[,2]) %>%
                                                    rvest::html_elements(ifelse(.y,"h1","p")) %>%
                                                    rvest::html_text2() %>%
                                                    purrr::pluck(1)

                                                }) %>% stringi::stri_replace_last_regex("\\.","")) %>%
    # dplyr::rename(Class = type_chr) %>%
    dplyr::select(Class,Description,Library,
                  Examples,old_class_lgl)
  return(modules_tb)
}


make_prompt <- function(prompt_1L_chr, options_chr = NULL, force_from_opts_1L_chr = F) {
  acknowledgement_1L_chr <- "This function is based on: https://debruine.github.io/posts/interactive-test/"
  con_conn <- getOption("prompt_opts.con", stdin())
  options_1L_chr <- paste(options_chr, collapse = "|")
  prompt_with_options_1L_chr <- paste0(prompt_1L_chr, " [", options_1L_chr, "]\n")
  cat(prompt_with_options_1L_chr)
  response_1L_chr <- readLines(con = con_conn, n = 1)
  if (!is.null(options_chr) & !response_1L_chr %in% options_chr & force_from_opts_1L_chr) {
    response_1L_chr  <- make_prompt(prompt_1L_chr, options_chr, force_from_opts_1L_chr = T)
  }
  return(response_1L_chr)
}
# make_gnrc_imports <- function(){
#   generics_chr <- methods::getGenerics("package:ready4")@.Data
#     # c(
#     # "author","authorClasses","authorData","authorFunctions",
#     # "characterize","enhance","ingest","investigate","manufacture",
#     # "metamorphose","procure","prognosticate",
#     # "reckon","renew","ratify","report","share")
#   gnrc_imports_chr <- rep("ready4",length(generics_chr)) %>% stats::setNames(generics_chr)
#   more_generics_chr <- ls("package:generics")
#   more_generics_chr <- setdiff(more_generics_chr, generics_chr)
#   gnrc_imports_chr <- c(gnrc_imports_chr,
#                         rep("generics",length(more_generics_chr)) %>% stats::setNames(more_generics_chr))
#
#   return(gnrc_imports_chr)
# }
