#' Add lookup tables
#' @description add_lups() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add lookup tables. Function argument template_lup specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param template_lup Template (a lookup table)
#' @param new_lup New (a lookup table)
#' @param key_var_nm_1L_chr Key variable name (a character vector of length one)
#' @param priority_lup_for_dupls_1L_chr Priority lookup table for duplicates (a character vector of length one), Default: 'template'
#' @return Combined (lookup tables)
#' @rdname add_lups
#' @export 
#' @importFrom testit assert
#' @importFrom dplyr filter pull bind_rows arrange
#' @importFrom rlang sym
#' @importFrom Hmisc label
#' @keywords internal
add_lups <- function (template_lup, new_lup, key_var_nm_1L_chr, priority_lup_for_dupls_1L_chr = "template") 
{
    testit::assert("Look up tables must have same column names", 
        names(template_lup) == names(new_lup))
    if (priority_lup_for_dupls_1L_chr == "template") {
        new_lup <- new_lup %>% dplyr::filter(!(!!rlang::sym(key_var_nm_1L_chr) %in% 
            (template_lup %>% dplyr::pull(!!rlang::sym(key_var_nm_1L_chr)))))
        labels_chr <- Hmisc::label(template_lup) %>% unname()
    }
    else {
        template_lup <- template_lup %>% dplyr::filter(!(!!rlang::sym(key_var_nm_1L_chr) %in% 
            (new_lup %>% dplyr::pull(!!rlang::sym(key_var_nm_1L_chr)))))
        labels_chr <- Hmisc::label(new_lup) %>% unname()
    }
    if (!all(labels_chr %>% unique() == "")) {
        template_lup <- template_lup %>% remove_lbls_from_df()
        new_lup <- new_lup %>% remove_lbls_from_df()
        Hmisc::label(template_lup) <- as.list(labels_chr %>% 
            unname())
        Hmisc::label(new_lup) <- as.list(labels_chr %>% unname())
    }
    combined_lups <- dplyr::bind_rows(template_lup, new_lup) %>% 
        dplyr::arrange(!!rlang::sym(key_var_nm_1L_chr))
    combined_lups <- combined_lups[rowSums(is.na(combined_lups)) != 
        ncol(combined_lups), ]
    return(combined_lups)
}
#' Add vignette links
#' @description add_vignette_links() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add vignette links. Function argument pkg_extensions_tb specifies the object to be updated. The function returns Package extensions (a tibble).
#' @param pkg_extensions_tb Package extensions (a tibble)
#' @param ns_var_nm_1L_chr Namespace variable name (a character vector of length one), Default: 'pt_ns_chr'
#' @param reference_var_nm_1L_chr Reference variable name (a character vector of length one), Default: 'Reference'
#' @param url_stub_1L_chr Url stub (a character vector of length one), Default: 'https://ready4-dev.github.io/'
#' @param vignette_var_nm_1L_chr Vignette variable name (a character vector of length one), Default: 'Vignettes'
#' @param vignette_url_var_nm_1L_chr Vignette url variable name (a character vector of length one), Default: 'Vignettes_URLs'
#' @return Package extensions (a tibble)
#' @rdname add_vignette_links
#' @export 
#' @importFrom dplyr mutate pull
#' @importFrom rlang sym
#' @importFrom purrr map keep map2 compact flatten_chr reduce pluck
#' @importFrom rvest read_html html_elements html_attr
#' @importFrom stringr str_remove
#' @importFrom kableExtra cell_spec
#' @keywords internal
add_vignette_links <- function (pkg_extensions_tb, ns_var_nm_1L_chr = "pt_ns_chr", 
    reference_var_nm_1L_chr = "Reference", url_stub_1L_chr = "https://ready4-dev.github.io/", 
    vignette_var_nm_1L_chr = "Vignettes", vignette_url_var_nm_1L_chr = "Vignettes_URLs") 
{
    pkg_extensions_tb <- pkg_extensions_tb %>% dplyr::mutate(`:=`(!!rlang::sym(vignette_var_nm_1L_chr), 
        purrr::map(!!rlang::sym(ns_var_nm_1L_chr), ~rvest::read_html(paste0(url_stub_1L_chr, 
            .x, "/index.html")) %>% rvest::html_elements(".dropdown-item") %>% 
            rvest::html_attr("href") %>% stringr::str_remove("articles/") %>% 
            purrr::keep(~startsWith(.x, "V_")) %>% sort()))) %>% 
        dplyr::mutate(`:=`(!!rlang::sym(vignette_var_nm_1L_chr), 
            purrr::map2(!!rlang::sym(ns_var_nm_1L_chr), !!rlang::sym(vignette_var_nm_1L_chr), 
                ~if (!identical(.y, character(0))) {
                  paste0(url_stub_1L_chr, .x, "/articles/", .y)
                }
                else {
                  NA_character_
                })))
    examples_1L_int <- pkg_extensions_tb %>% dplyr::pull(!!rlang::sym(vignette_var_nm_1L_chr)) %>% 
        purrr::compact() %>% purrr::flatten_chr() %>% length()
    pkg_extensions_tb <- pkg_extensions_tb %>% dplyr::mutate(`:=`(!!rlang::sym(reference_var_nm_1L_chr), 
        !!rlang::sym(vignette_var_nm_1L_chr) %>% purrr::reduce(.init = list(ref_int = c(0, 
            0), content_ls = NULL), ~{
            if (is.null(.y) | identical(.y, character(0)) | is.na(.y[1])) {
                .x$content_ls <- append(.x$content_ls, list(NA_integer_))
            }
            else {
                .x$ref_int <- c(.x$ref_int[2] + 1, .x$ref_int[2] + 
                  length(.y))
                .x$content_ls <- append(.x$content_ls, list((1:examples_1L_int)[.x$ref_int]))
            }
            .x
        }) %>% purrr::pluck(2))) %>% dplyr::mutate(`:=`(!!rlang::sym(vignette_url_var_nm_1L_chr), 
        purrr::map2(!!rlang::sym(reference_var_nm_1L_chr), !!rlang::sym(vignette_var_nm_1L_chr), 
            ~{
                if (is.na(.x[1])) {
                  NA_character_
                }
                else {
                  kableExtra::cell_spec(unique(.x), "html", link = .y)
                }
            })))
    return(pkg_extensions_tb)
}
