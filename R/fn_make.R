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
#' Make methods tibble
#' @description make_methods_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make methods tibble. The function returns Methods (a tibble).
#' @param packages_tb Packages (a tibble), Default: NULL
#' @param exclude_mthds_for_chr Exclude methods for (a character vector), Default: 'NA'
#' @param ns_var_nm_1L_chr Namespace variable name (a character vector of length one), Default: 'pt_ns_chr'
#' @param reference_var_nm_1L_chr Reference variable name (a character vector of length one), Default: 'Reference'
#' @param return_1L_lgl Return (a logical vector of length one), Default: 'all'
#' @param url_stub_1L_chr Url stub (a character vector of length one), Default: 'https://ready4-dev.github.io/'
#' @param vignette_var_nm_1L_chr Vignette variable name (a character vector of length one), Default: 'Vignettes'
#' @param vignette_url_var_nm_1L_chr Vignette url variable name (a character vector of length one), Default: 'Vignettes_URLs'
#' @return Methods (a tibble)
#' @rdname make_methods_tb
#' @export 
#' @importFrom tibble tibble
#' @importFrom purrr map flatten_chr discard
make_methods_tb <- function (packages_tb = NULL, exclude_mthds_for_chr = NA_character_, 
    ns_var_nm_1L_chr = "pt_ns_chr", reference_var_nm_1L_chr = "Reference", 
    return_1L_lgl = "all", url_stub_1L_chr = "https://ready4-dev.github.io/", 
    vignette_var_nm_1L_chr = "Vignettes", vignette_url_var_nm_1L_chr = "Vignettes_URLs") 
{
    packages_tb <- make_pkg_extensions_tb(ns_var_nm_1L_chr = ns_var_nm_1L_chr, 
        reference_var_nm_1L_chr = reference_var_nm_1L_chr, url_stub_1L_chr = url_stub_1L_chr, 
        vignette_var_nm_1L_chr = vignette_var_nm_1L_chr, vignette_url_var_nm_1L_chr = vignette_url_var_nm_1L_chr)
    methods_tb <- tibble::tibble(Method = get_generics(exclude_mthds_for_chr = exclude_mthds_for_chr, 
        return_1L_lgl = return_1L_lgl), Purpose = get_mthd_titles(Method), 
        Examples = purrr::map(Method, ~get_examples(packages_tb$Vignettes_URLs %>% 
            purrr::flatten_chr() %>% unique() %>% purrr::discard(is.na), 
            term_1L_chr = .x)))
    return(methods_tb)
}
#' Make modules tibble
#' @description make_modules_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make modules tibble. The function returns Modules (a tibble).
#' @param pkg_extensions_tb Package extensions (a tibble), Default: NULL
#' @param cls_extensions_tb Class extensions (a tibble), Default: NULL
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @return Modules (a tibble)
#' @rdname make_modules_tb
#' @export 
#' @importFrom dplyr inner_join mutate select
#' @importFrom purrr pmap map2 map2_chr pluck
#' @importFrom kableExtra cell_spec
#' @importFrom rvest read_html html_elements html_text2
#' @importFrom stringr str_match
#' @importFrom stringi stri_replace_last_regex
make_modules_tb <- function (pkg_extensions_tb = NULL, cls_extensions_tb = NULL, 
    gh_repo_1L_chr = "ready4-dev/ready4", gh_tag_1L_chr = "Documentation_0.0") 
{
    if (is.null(pkg_extensions_tb)) 
        pkg_extensions_tb <- make_pkg_extensions_tb()
    if (is.null(cls_extensions_tb)) 
        cls_extensions_tb <- get_cls_extensions(pkg_extensions_tb, 
            gh_repo_1L_chr = gh_repo_1L_chr, gh_tag_1L_chr = gh_tag_1L_chr)
    modules_tb <- dplyr::inner_join(cls_extensions_tb, pkg_extensions_tb) %>% 
        dplyr::mutate(Class = purrr::pmap(list(pt_ns_chr, type_chr, 
            old_class_lgl), ~{
            kableExtra::cell_spec(..2, "html", link = paste0("https://ready4-dev.github.io/", 
                ..1, "/reference/", ifelse(..3, ..2, paste0(..2, 
                  "-class")), ".html"))
        })) %>% dplyr::mutate(Examples = purrr::map2(Vignettes_URLs, 
        type_chr, ~get_examples(.x, term_1L_chr = .y))) %>% dplyr::mutate(Description = purrr::map2_chr(Class, 
        old_class_lgl, ~{
            rvest::read_html((.x %>% stringr::str_match("href=\"\\s*(.*?)\\s*\" style"))[, 
                2]) %>% rvest::html_elements(ifelse(.y, "h1", 
                "p")) %>% rvest::html_text2() %>% purrr::pluck(1)
        }) %>% stringi::stri_replace_last_regex("\\.", "")) %>% 
        dplyr::select(Class, Description, Library, Examples, 
            old_class_lgl)
    return(modules_tb)
}
#' Make package extensions tibble
#' @description make_pkg_extensions_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make package extensions tibble. The function returns Package extensions (a tibble).
#' @param ns_var_nm_1L_chr Namespace variable name (a character vector of length one), Default: 'pt_ns_chr'
#' @param reference_var_nm_1L_chr Reference variable name (a character vector of length one), Default: 'Reference'
#' @param url_stub_1L_chr Url stub (a character vector of length one), Default: 'https://ready4-dev.github.io/'
#' @param vignette_var_nm_1L_chr Vignette variable name (a character vector of length one), Default: 'Vignettes'
#' @param vignette_url_var_nm_1L_chr Vignette url variable name (a character vector of length one), Default: 'Vignettes_URLs'
#' @return Package extensions (a tibble)
#' @rdname make_pkg_extensions_tb
#' @export 
#' @importFrom tibble tibble
#' @importFrom dplyr mutate case_when arrange select rename left_join
#' @importFrom purrr map_chr map2 map_dfr
#' @importFrom kableExtra cell_spec
#' @importFrom rvest read_html html_elements html_text2
#' @importFrom bib2df bib2df
make_pkg_extensions_tb <- function (ns_var_nm_1L_chr = "pt_ns_chr", reference_var_nm_1L_chr = "Reference", 
    url_stub_1L_chr = "https://ready4-dev.github.io/", vignette_var_nm_1L_chr = "Vignettes", 
    vignette_url_var_nm_1L_chr = "Vignettes_URLs") 
{
    pkg_extensions_tb <- tibble::tibble(pt_ns_chr = c("scorz", 
        "specific", "TTU", "youthvars", "ready4show", "ready4use", 
        "ready4fun", "ready4class", "ready4pack", "youthu")) %>% 
        dplyr::mutate(Purpose = dplyr::case_when(pt_ns_chr == 
            "ready4class" ~ "Authoring (code - classes)", pt_ns_chr == 
            "ready4fun" ~ "Authoring (code - functions)", pt_ns_chr == 
            "ready4pack" ~ "Authoring (code - libraries)", pt_ns_chr == 
            "ready4show" ~ "Authoring (code - programs)", pt_ns_chr == 
            "ready4use" ~ "Authoring (datasets)", pt_ns_chr == 
            "youthvars" ~ "Description (datasets)", pt_ns_chr == 
            "scorz" ~ "Description (variable scoring)", pt_ns_chr == 
            "specific" ~ "Modelling (inverse problems)", pt_ns_chr == 
            "heterodox" ~ "Modelling (heterogeneity)", pt_ns_chr == 
            "TTU" ~ "Modelling (health utility)", pt_ns_chr == 
            "youthu" ~ "Prediction (health utility)", T ~ "")) %>% 
        dplyr::arrange(Purpose) %>% dplyr::mutate(Link = purrr::map_chr(pt_ns_chr, 
        ~paste0(url_stub_1L_chr, .x, "/index", ".html"))) %>% 
        dplyr::mutate(Library = kableExtra::cell_spec(pt_ns_chr, 
            "html", link = Link))
    pkg_extensions_tb <- add_vignette_links(pkg_extensions_tb, 
        ns_var_nm_1L_chr = ns_var_nm_1L_chr, reference_var_nm_1L_chr = reference_var_nm_1L_chr, 
        url_stub_1L_chr = url_stub_1L_chr, vignette_var_nm_1L_chr = vignette_var_nm_1L_chr, 
        vignette_url_var_nm_1L_chr = vignette_url_var_nm_1L_chr)
    pkg_extensions_tb <- pkg_extensions_tb %>% dplyr::mutate(manual_urls_ls = purrr::map2(pt_ns_chr, 
        Link, ~get_manual_urls(.x, pkg_url_1L_chr = .y))) %>% 
        dplyr::mutate(code_urls_ls = purrr::map2(pt_ns_chr, Link, 
            ~get_source_code_urls(.x, pkg_url_1L_chr = .y)))
    y_tb <- purrr::map_dfr(pkg_extensions_tb$pt_ns_chr, ~{
        if (!.x %in% c("TTU", "youthu")) {
            f <- tempfile(fileext = ".bib")
            sink(f)
            writeLines(rvest::read_html(paste0(url_stub_1L_chr, 
                .x, "/authors.html")) %>% rvest::html_elements("pre") %>% 
                rvest::html_text2())
            sink(NULL)
            bib2df::bib2df(f) %>% dplyr::select(AUTHOR, TITLE, 
                DOI)
        }
        else {
            if (.x == "TTU") {
                tibble::tibble(AUTHOR = list(c("Caroline Gao", 
                  "Matthew Hamilton")), TITLE = "TTU: Specify, Report and Share Transfer to Utility Mapping Algorithms", 
                  DOI = "10.5281/zenodo.5646593")
            }
            else {
                tibble::tibble(AUTHOR = list(c("Matthew Hamilton", 
                  "Caroline Gao")), TITLE = "youthu: Map Youth Outcomes to Health Utility", 
                  DOI = "10.5281/zenodo.5646668")
            }
        }
    }) %>% dplyr::mutate(pt_ns_chr = pkg_extensions_tb$pt_ns_chr) %>% 
        dplyr::rename(DOI_chr = DOI, Title = TITLE, Authors = AUTHOR)
    pkg_extensions_tb <- dplyr::left_join(pkg_extensions_tb, 
        y_tb, by = "pt_ns_chr")
    return(pkg_extensions_tb)
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
