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
#' @keywords internal
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
        })) %>% dplyr::mutate(Examples = purrr::map2(Vignette_URLs, 
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

#' @return Package extensions (a tibble)
#' @rdname make_pkg_extensions_tb
#' @export 
#' @importFrom tibble tibble
#' @importFrom dplyr mutate case_when arrange select rename left_join
#' @importFrom purrr map_chr map keep compact flatten_chr reduce pluck pmap map_dfr
#' @importFrom kableExtra cell_spec
#' @importFrom rvest read_html html_elements html_attr html_text2
#' @importFrom stringr str_remove
#' @importFrom bib2df bib2df
#' @keywords internal
make_pkg_extensions_tb <- function () 
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
        ~paste0("https://ready4-dev.github.io/", .x, "/index", 
            ".html"))) %>% dplyr::mutate(Library = kableExtra::cell_spec(pt_ns_chr, 
        "html", link = Link)) %>% dplyr::mutate(Vignettes = purrr::map(pt_ns_chr, 
        ~rvest::read_html(paste0("https://ready4-dev.github.io/", 
            .x, "/index.html")) %>% rvest::html_elements(".dropdown-item") %>% 
            rvest::html_attr("href") %>% stringr::str_remove("articles/") %>% 
            purrr::keep(~startsWith(.x, "V_")) %>% sort()))
    examples_1L_int <- pkg_extensions_tb$Vignettes %>% purrr::compact() %>% 
        purrr::flatten_chr() %>% length()
    pkg_extensions_tb <- pkg_extensions_tb %>% dplyr::mutate(Reference = Vignettes %>% 
        purrr::reduce(.init = list(ref_int = c(0, 0), content_ls = NULL), 
            ~{
                if (is.null(.y) | identical(.y, character(0))) {
                  .x$content_ls <- append(.x$content_ls, list(NA_integer_))
                }
                else {
                  .x$ref_int <- .x$ref_int + 1:length(.y)
                  .x$content_ls <- append(.x$content_ls, list((1:examples_1L_int)[.x$ref_int]))
                }
                .x
            }) %>% purrr::pluck(2)) %>% dplyr::mutate(Vignette_URLs = purrr::pmap(list(pt_ns_chr, 
        Vignettes, Reference), ~{
        if (is.na(..3[1])) {
            NA_character_
        }
        else {
            kableExtra::cell_spec(..3, "html", link = paste0("https://ready4-dev.github.io/", 
                ..1, "/articles/", ..2))
        }
    }))
    y_tb <- purrr::map_dfr(pkg_extensions_tb$pt_ns_chr, ~{
        if (!.x %in% c("TTU", "youthu")) {
            f <- tempfile(fileext = ".bib")
            sink(f)
            writeLines(rvest::read_html(paste0("https://ready4-dev.github.io/", 
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
        dplyr::rename(DOI_chr = DOI, Package = TITLE, Authors = AUTHOR)
    pkg_extensions_tb <- dplyr::left_join(pkg_extensions_tb, 
        y_tb)
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
