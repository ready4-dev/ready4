#' Get badge urls
#' @description get_badge_urls() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get badge urls. Function argument pkg_nm_1L_chr specifies the where to look for the required object. The function returns Badge urls (a list).
#' @param pkg_nm_1L_chr Package name (a character vector of length one)
#' @param project_badges_url_1L_chr Project badges url (a character vector of length one), Default: 'https://img.shields.io/badge/ready4'
#' @param url_stub_1L_chr Url stub (a character vector of length one), Default: 'https://ready4-dev.github.io/'
#' @return Badge urls (a list)
#' @rdname get_badge_urls
#' @export 
#' @importFrom rvest read_html html_elements html_attr
#' @keywords internal
get_badge_urls <- function (pkg_nm_1L_chr, project_badges_url_1L_chr = "https://img.shields.io/badge/ready4", 
    url_stub_1L_chr = "https://ready4-dev.github.io/") 
{
    images_chr <- rvest::read_html(paste0(url_stub_1L_chr, pkg_nm_1L_chr, 
        "/index.html")) %>% rvest::html_elements("img") %>% rvest::html_attr("src")
    badge_urls_ls <- list(ready4_1L_chr = images_chr[images_chr %>% 
        startsWith(project_badges_url_1L_chr)], zenodo_1L_chr = images_chr[images_chr %>% 
        startsWith("https://zenodo.org/badge/DOI/")])
    return(badge_urls_ls)
}
#' Get badges lookup table
#' @description get_badges_lup() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get badges lookup table. Function argument ends_with_1L_chr specifies the where to look for the required object. The function returns Ready4 badges (a lookup table).
#' @param ends_with_1L_chr Ends with (a character vector of length one), Default: 'ready4_badges_lup.RDS'
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @return Ready4 badges (a lookup table)
#' @rdname get_badges_lup
#' @export 
#' @importFrom piggyback pb_download_url
#' @keywords internal
get_badges_lup <- function (ends_with_1L_chr = "ready4_badges_lup.RDS", gh_repo_1L_chr = "ready4-dev/ready4", 
    gh_tag_1L_chr = "Documentation_0.0") 
{
    dmt_urls_chr <- piggyback::pb_download_url(repo = gh_repo_1L_chr, 
        tag = gh_tag_1L_chr, .token = "")
    ready4_badges_lup <- readRDS(url(dmt_urls_chr[dmt_urls_chr %>% 
        endsWith(ends_with_1L_chr)]))
    return(ready4_badges_lup)
}
#' Get class extensions
#' @description get_cls_extensions() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get class extensions. Function argument pkg_extensions_tb specifies the where to look for the required object. The function returns Class extensions (a tibble).
#' @param pkg_extensions_tb Package extensions (a tibble)
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @param url_stub_1L_chr Url stub (a character vector of length one), Default: 'https://ready4-dev.github.io/'
#' @param validate_1L_lgl Validate (a logical vector of length one), Default: F
#' @return Class extensions (a tibble)
#' @rdname get_cls_extensions
#' @export 
#' @importFrom piggyback pb_download_url
#' @importFrom tibble as_tibble
#' @importFrom dplyr arrange filter select
#' @importFrom purrr map_dfr
#' @importFrom rvest read_html html_elements html_text2
#' @importFrom stringi stri_replace_last_fixed
#' @keywords internal
get_cls_extensions <- function (pkg_extensions_tb, gh_repo_1L_chr = "ready4-dev/ready4", 
    gh_tag_1L_chr = "Documentation_0.0", url_stub_1L_chr = "https://ready4-dev.github.io/", 
    validate_1L_lgl = F) 
{
    dmt_urls_chr <- piggyback::pb_download_url(repo = gh_repo_1L_chr, 
        tag = gh_tag_1L_chr, .token = "")
    cls_extensions_tb <- readRDS(url(dmt_urls_chr[dmt_urls_chr %>% 
        endsWith("classes_lup.RDS")])) %>% tibble::as_tibble() %>% 
        dplyr::arrange(pt_ns_chr) %>% dplyr::filter(pt_ns_chr %in% 
        pkg_extensions_tb$pt_ns_chr) %>% dplyr::arrange(pt_ns_chr) %>% 
        dplyr::select(type_chr, pt_ns_chr, old_class_lgl)
    if (validate_1L_lgl) {
        cls_extensions_tb <- cls_extensions_tb$pt_ns_chr %>% 
            unique() %>% purrr::map_dfr(~{
            url_1L_chr <- paste0(url_stub_1L_chr, .x, "/reference/index", 
                ".html")
            allowable_chr <- rvest::read_html((url_1L_chr)) %>% 
                rvest::html_elements("a") %>% rvest::html_text2()
            allowable_chr <- allowable_chr[(allowable_chr %>% 
                endsWith("-class") | allowable_chr %>% startsWith(.x)) & 
                allowable_chr != .x] %>% stringi::stri_replace_last_fixed("-class", 
                "") %>% stringi::stri_replace_last_fixed("()", 
                "")
            cls_extensions_tb %>% dplyr::filter(pt_ns_chr == 
                .x) %>% dplyr::filter(type_chr %in% allowable_chr)
        })
    }
    return(cls_extensions_tb)
}
#' Get datasets tibble
#' @description get_datasets_tb() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get datasets tibble. Function argument gh_repo_1L_chr specifies the where to look for the required object. The function returns Datasets (a tibble).
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @return Datasets (a tibble)
#' @rdname get_datasets_tb
#' @export 
#' @importFrom piggyback pb_download_url
#' @keywords internal
get_datasets_tb <- function (gh_repo_1L_chr = "ready4-dev/ready4", gh_tag_1L_chr = "Documentation_0.0") 
{
    dmt_urls_chr <- piggyback::pb_download_url(repo = gh_repo_1L_chr, 
        tag = gh_tag_1L_chr, .token = "")
    datasets_tb <- readRDS(url(dmt_urls_chr[dmt_urls_chr %>% 
        endsWith("datasets_tb.RDS")]))
    return(datasets_tb)
}
#' Get digits from text
#' @description get_digits_from_text() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get digits from text. Function argument text_1L_chr specifies the where to look for the required object. The function returns Digits (a character vector).
#' @param text_1L_chr Text (a character vector of length one)
#' @return Digits (a character vector)
#' @rdname get_digits_from_text
#' @export 
#' @keywords internal
get_digits_from_text <- function (text_1L_chr) 
{
    fn_attribution_1L_chr <- "This function is based on: http://stla.github.io/stlapblog/posts/Numextract.html"
    digits_chr <- unlist(regmatches(text_1L_chr, gregexpr("[[:digit:]]+\\.*[[:digit:]]*", 
        text_1L_chr)))
    return(digits_chr)
}
#' Get dataverse files urls
#' @description get_dv_fls_urls() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get dataverse files urls. Function argument file_nms_chr specifies the where to look for the required object. The function returns Urls (a character vector).
#' @param file_nms_chr File names (a character vector)
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one)
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: character(0)
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @return Urls (a character vector)
#' @rdname get_dv_fls_urls
#' @export 
#' @importFrom dataverse dataset_files
#' @importFrom purrr map_chr
#' @keywords internal
get_dv_fls_urls <- function (file_nms_chr, dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = character(0), 
    server_1L_chr = Sys.getenv("DATAVERSE_SERVER"), key_1L_chr = NULL) 
{
    if (identical(dv_url_pfx_1L_chr, character(0))) 
        dv_url_pfx_1L_chr <- paste0("https://", server_1L_chr, 
            "/api/access/datafile/")
    ds_ls <- dataverse::dataset_files(dv_ds_nm_1L_chr, server = server_1L_chr, 
        key = key_1L_chr)
    all_items_chr <- purrr::map_chr(ds_ls, ~.x$label)
    urls_chr <- file_nms_chr %>% purrr::map_chr(~{
        idx_1L_int <- which(all_items_chr == .x)
        if (identical(idx_1L_int, integer(0))) {
            NA_character_
        }
        else {
            paste0(dv_url_pfx_1L_chr, ds_ls[[idx_1L_int]]$dataFile$id)
        }
    })
    return(urls_chr)
}
#' Get examples
#' @description get_examples() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get examples. Function argument vignettes_chr specifies the where to look for the required object. The function returns Examples (a character vector).
#' @param vignettes_chr Vignettes (a character vector)
#' @param term_1L_chr Term (a character vector of length one)
#' @return Examples (a character vector)
#' @rdname get_examples
#' @export 
#' @importFrom purrr map_chr discard
#' @importFrom rvest read_html html_elements html_text2
#' @importFrom stringr str_match str_detect
#' @keywords internal
get_examples <- function (vignettes_chr, term_1L_chr) 
{
    if (is.na(vignettes_chr[1])) {
        examples_chr <- ""
    }
    else {
        examples_chr <- vignettes_chr %>% purrr::map_chr(~{
            code_chr <- rvest::read_html((.x %>% stringr::str_match("href=\"\\s*(.*?)\\s*\" style"))[, 
                2]) %>% rvest::html_elements(".r") %>% rvest::html_text2()
            ifelse(stringr::str_detect(paste0(code_chr, collapse = "/n"), 
                paste0(term_1L_chr, "\\(")), .x, NA_character_)
        }) %>% purrr::discard(is.na)
        if (identical(character(0), examples_chr)) 
            examples_chr <- ""
    }
    return(examples_chr)
}
#' Get excluded repositories
#' @description get_excluded_repos() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get excluded repositories. Function argument gh_repo_1L_chr specifies the where to look for the required object. The function returns Exclude (a character vector).
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @return Exclude (a character vector)
#' @rdname get_excluded_repos
#' @export 
#' @importFrom piggyback pb_download_url
#' @keywords internal
get_excluded_repos <- function (gh_repo_1L_chr = "ready4-dev/ready4", gh_tag_1L_chr = "Documentation_0.0") 
{
    dmt_urls_chr <- piggyback::pb_download_url(repo = gh_repo_1L_chr, 
        tag = gh_tag_1L_chr, .token = "")
    if (any(dmt_urls_chr %>% endsWith("exclude_chr.RDS"))) {
        exclude_chr <- readRDS(url(dmt_urls_chr[dmt_urls_chr %>% 
            endsWith("exclude_chr.RDS")]))
    }
    else {
        exclude_chr <- character(0)
    }
    return(exclude_chr)
}
#' Get file identity from dataverse list
#' @description get_fl_id_from_dv_ls() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get file identity from dataverse list. Function argument ds_ls specifies the where to look for the required object. The function returns Identity (a character vector of length one).
#' @param ds_ls Dataset (a list)
#' @param fl_nm_1L_chr File name (a character vector of length one)
#' @param nms_chr Names (a character vector), Default: 'NA'
#' @return Identity (a character vector of length one)
#' @rdname get_fl_id_from_dv_ls
#' @export 
#' @importFrom purrr map_chr
#' @importFrom tibble as_tibble
#' @keywords internal
get_fl_id_from_dv_ls <- function (ds_ls, fl_nm_1L_chr, nms_chr = NA_character_) 
{
    if (is.na(nms_chr[1])) {
        nms_chr <- purrr::map_chr(ds_ls$files$filename, ~ifelse(is.na(.x), 
            .y, .x))
    }
    if (fl_nm_1L_chr %in% nms_chr) {
        id_1L_chr <- get_from_lup_obj(ds_ls$files[, names(ds_ls$files) %>% 
            unique()] %>% tibble::as_tibble(), match_var_nm_1L_chr = "filename", 
            match_value_xx = fl_nm_1L_chr, target_var_nm_1L_chr = "id", 
            evaluate_1L_lgl = F)
    }
    else {
        id_1L_chr <- NA_character_
    }
    return(id_1L_chr)
}
#' Get from lookup table object
#' @description get_from_lup_obj() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get from lookup table object. Function argument data_lookup_tb specifies the where to look for the required object. The function returns Return object (an output object of multiple potential types).
#' @param data_lookup_tb Data lookup (a tibble)
#' @param match_value_xx Match value (an output object of multiple potential types)
#' @param match_var_nm_1L_chr Match variable name (a character vector of length one)
#' @param target_var_nm_1L_chr Target variable name (a character vector of length one)
#' @param evaluate_1L_lgl Evaluate (a logical vector of length one), Default: FALSE
#' @return Return object (an output object of multiple potential types)
#' @rdname get_from_lup_obj
#' @export 
#' @importFrom dplyr filter select pull
#' @importFrom rlang sym
#' @importFrom stringr str_detect str_locate str_sub
get_from_lup_obj <- function (data_lookup_tb, match_value_xx, match_var_nm_1L_chr, 
    target_var_nm_1L_chr, evaluate_1L_lgl = FALSE) 
{
    return_object_ref <- data_lookup_tb %>% dplyr::filter(!!rlang::sym(match_var_nm_1L_chr) == 
        match_value_xx) %>% dplyr::select(!!target_var_nm_1L_chr) %>% 
        dplyr::pull()
    if (evaluate_1L_lgl) {
        if (stringr::str_detect(return_object_ref, "::")) {
            colon_positions <- stringr::str_locate(return_object_ref, 
                "::")
            namespace_ref <- stringr::str_sub(return_object_ref, 
                start = 1, end = colon_positions[1, "start"] - 
                  1)
            object_ref <- stringr::str_sub(return_object_ref, 
                start = colon_positions[1, "end"] + 1)
            if (sum(stringr::str_detect(search(), paste0("package:", 
                namespace_ref))) == 0) {
                namespace_ref_sym <- rlang::sym(namespace_ref)
                attachNamespace(namespace_ref)
                return_object_xx <- get(x = object_ref, envir = as.environment(paste0("package:", 
                  namespace_ref)))
                detach(paste0("package:", namespace_ref), character.only = TRUE)
            }
            else {
                return_object_xx <- get(x = object_ref, envir = as.environment(paste0("package:", 
                  namespace_ref)))
            }
        }
        else {
            return_object_xx <- get(x = return_object_ref)
        }
    }
    else {
        return_object_xx <- return_object_ref
    }
    return(return_object_xx)
}
#' Get functions tibble
#' @description get_functions_tb() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get functions tibble. Function argument gh_repo_1L_chr specifies the where to look for the required object. The function returns Functions (a tibble).
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @param return_1L_chr Return (a character vector of length one), Default: 'all'
#' @return Functions (a tibble)
#' @rdname get_functions_tb
#' @export 
#' @importFrom piggyback pb_download_url
#' @importFrom dplyr filter select
#' @keywords internal
get_functions_tb <- function (gh_repo_1L_chr = "ready4-dev/ready4", gh_tag_1L_chr = "Documentation_0.0", 
    return_1L_chr = "all") 
{
    dmt_urls_chr <- piggyback::pb_download_url(repo = gh_repo_1L_chr, 
        tag = gh_tag_1L_chr, .token = "")
    functions_tb <- readRDS(url(dmt_urls_chr[dmt_urls_chr %>% 
        endsWith("fn_types_lup.RDS")]))
    if (return_1L_chr == "methods") {
        functions_tb <- functions_tb %>% dplyr::filter(is_method_lgl)
    }
    if (return_1L_chr == "types") {
        functions_tb <- functions_tb %>% dplyr::filter(!is_method_lgl)
    }
    functions_tb <- functions_tb %>% dplyr::select(fn_type_nm_chr, 
        fn_type_desc_chr)
    return(functions_tb)
}
#' Get generics
#' @description get_generics() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get generics. Function argument pkg_nm_1L_chr specifies the where to look for the required object. The function returns Generics (a character vector).
#' @param pkg_nm_1L_chr Package name (a character vector of length one), Default: 'ready4'
#' @param return_1L_chr Return (a character vector of length one), Default: 'all'
#' @param exclude_mthds_for_chr Exclude methods for (a character vector), Default: 'NA'
#' @param framework_only_1L_lgl Framework only (a logical vector of length one), Default: T
#' @return Generics (a character vector)
#' @rdname get_generics
#' @export 
#' @importFrom methods getGenerics
#' @importFrom purrr map_lgl map flatten_chr
#' @keywords internal
get_generics <- function (pkg_nm_1L_chr = "ready4", return_1L_chr = "all", exclude_mthds_for_chr = NA_character_, 
    framework_only_1L_lgl = T) 
{
    generics_chr <- methods::getGenerics(paste0("package:", pkg_nm_1L_chr))@.Data
    generics_chr <- generics_chr[generics_chr %>% purrr::map_lgl(~{
        generic_1L_chr <- .x
        any(letters %>% purrr::map_lgl(~startsWith(generic_1L_chr, 
            .x)))
    })]
    if (!is.na(exclude_mthds_for_chr[1])) {
        generics_chr <- setdiff(generics_chr, purrr::map(exclude_mthds_for_chr, 
            ~get_methods(pkg_nm_1L_chr = pkg_nm_1L_chr, cls_nm_1L_chr = .x)) %>% 
            purrr::flatten_chr() %>% unique())
    }
    if (framework_only_1L_lgl) {
        generics_chr <- setdiff(generics_chr, c("addNextMethod", 
            "body<-", "cbind2", "coerce", "coerce<-", "kronecker", 
            "loadMethod", "rbind2", "show", "slotsFromS3"))
    }
    if (return_1L_chr == "core") {
        generics_chr <- generics_chr[tolower(generics_chr) == 
            generics_chr]
    }
    if (return_1L_chr == "extended") 
        generics_chr <- generics_chr[tolower(generics_chr) != 
            generics_chr]
    if (return_1L_chr == "slot") {
        generics_chr <- generics_chr[endsWith(generics_chr, "Slot")]
    }
    return(generics_chr)
}
#' Get libraries list
#' @description get_libraries_ls() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get libraries list. Function argument gh_repo_1L_chr specifies the where to look for the required object. The function returns Libraries (a list).
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @return Libraries (a list)
#' @rdname get_libraries_ls
#' @export 
#' @importFrom piggyback pb_download_url
#' @keywords internal
get_libraries_ls <- function (gh_repo_1L_chr = "ready4-dev/ready4", gh_tag_1L_chr = "Documentation_0.0") 
{
    dmt_urls_chr <- piggyback::pb_download_url(repo = gh_repo_1L_chr, 
        tag = gh_tag_1L_chr, .token = "")
    if (any(dmt_urls_chr %>% endsWith("libraries_ls.RDS"))) {
        libraries_ls <- readRDS(url(dmt_urls_chr[dmt_urls_chr %>% 
            endsWith("libraries_ls.RDS")]))
    }
    else {
        libraries_ls <- NULL
    }
    return(libraries_ls)
}
#' Get libraries tibble
#' @description get_libraries_tb() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get libraries tibble. Function argument gh_repo_1L_chr specifies the where to look for the required object. The function returns Libraries (a tibble).
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @return Libraries (a tibble)
#' @rdname get_libraries_tb
#' @export 
#' @importFrom piggyback pb_download_url
#' @keywords internal
get_libraries_tb <- function (gh_repo_1L_chr = "ready4-dev/ready4", gh_tag_1L_chr = "Documentation_0.0") 
{
    dmt_urls_chr <- piggyback::pb_download_url(repo = gh_repo_1L_chr, 
        tag = gh_tag_1L_chr, .token = "")
    if (any(dmt_urls_chr %>% endsWith("libraries_tb.RDS"))) {
        libraries_tb <- readRDS(url(dmt_urls_chr[dmt_urls_chr %>% 
            endsWith("libraries_tb.RDS")]))
    }
    else {
        libraries_tb <- NULL
    }
    return(libraries_tb)
}
#' Get manual urls
#' @description get_manual_urls() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get manual urls. Function argument pkg_nm_1L_chr specifies the where to look for the required object. The function returns Urls (a character vector).
#' @param pkg_nm_1L_chr Package name (a character vector of length one), Default: 'ready4'
#' @param pkg_url_1L_chr Package url (a character vector of length one), Default: 'https://ready4-dev.github.io/ready4/index.html'
#' @return Urls (a character vector)
#' @rdname get_manual_urls
#' @export 
#' @importFrom rvest read_html html_elements html_attr
#' @importFrom purrr map_lgl
#' @keywords internal
get_manual_urls <- function (pkg_nm_1L_chr = "ready4", pkg_url_1L_chr = "https://ready4-dev.github.io/ready4/index.html") 
{
    urls_chr <- rvest::read_html(pkg_url_1L_chr) %>% rvest::html_elements(".external-link") %>% 
        rvest::html_attr("href")
    indcs_int <- urls_chr %>% purrr::map_lgl(~endsWith(.x, paste0(pkg_nm_1L_chr, 
        "_User.pdf")) | endsWith(.x, paste0(pkg_nm_1L_chr, "_Developer.pdf"))) %>% 
        which()
    urls_chr <- sort(urls_chr[indcs_int], decreasing = T)
    return(urls_chr)
}
#' Get methods
#' @description get_methods() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get methods. Function argument pkg_nm_1L_chr specifies the where to look for the required object. The function returns Methods (a character vector).
#' @param pkg_nm_1L_chr Package name (a character vector of length one), Default: 'ready4'
#' @param cls_nm_1L_chr Class name (a character vector of length one), Default: 'Ready4Module'
#' @return Methods (a character vector)
#' @rdname get_methods
#' @export 
#' @importFrom stringr str_detect str_remove_all
get_methods <- function (pkg_nm_1L_chr = "ready4", cls_nm_1L_chr = "Ready4Module") 
{
    methods_chr <- showMethods(class = "Ready4Module", printTo = FALSE)
    methods_chr <- methods_chr[which(methods_chr %>% stringr::str_detect(cls_nm_1L_chr)) - 
        1]
    methods_chr <- methods_chr[which(methods_chr %>% stringr::str_detect(paste0("package ", 
        pkg_nm_1L_chr)))] %>% stringr::str_remove_all("Function: ") %>% 
        stringr::str_remove_all(paste0(" \\(package ", pkg_nm_1L_chr, 
            "\\)"))
    return(methods_chr)
}
#' Get methods tibble
#' @description get_methods_tb() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get methods tibble. Function argument gh_repo_1L_chr specifies the where to look for the required object. The function returns Methods (a tibble).
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @return Methods (a tibble)
#' @rdname get_methods_tb
#' @export 
#' @importFrom piggyback pb_download_url
#' @keywords internal
get_methods_tb <- function (gh_repo_1L_chr = "ready4-dev/ready4", gh_tag_1L_chr = "Documentation_0.0") 
{
    dmt_urls_chr <- piggyback::pb_download_url(repo = gh_repo_1L_chr, 
        tag = gh_tag_1L_chr, .token = "")
    methods_tb <- readRDS(url(dmt_urls_chr[dmt_urls_chr %>% endsWith("methods_tb.RDS")]))
    return(methods_tb)
}
#' Get modules tibble
#' @description get_modules_tb() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get modules tibble. Function argument gh_repo_1L_chr specifies the where to look for the required object. The function returns Modules (a tibble).
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @return Modules (a tibble)
#' @rdname get_modules_tb
#' @export 
#' @importFrom piggyback pb_download_url
#' @keywords internal
get_modules_tb <- function (gh_repo_1L_chr = "ready4-dev/ready4", gh_tag_1L_chr = "Documentation_0.0") 
{
    dmt_urls_chr <- piggyback::pb_download_url(repo = gh_repo_1L_chr, 
        tag = gh_tag_1L_chr, .token = "")
    modules_tb <- readRDS(url(dmt_urls_chr[dmt_urls_chr %>% endsWith("modules_tb.RDS")]))
    return(modules_tb)
}
#' Get method titles
#' @description get_mthd_titles() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get method titles. Function argument mthd_nms_chr specifies the where to look for the required object. The function returns Method titles (a character vector).
#' @param mthd_nms_chr Method names (a character vector)
#' @param pkg_nm_1L_chr Package name (a character vector of length one), Default: 'ready4'
#' @param path_1L_chr Path (a character vector of length one), Default: character(0)
#' @return Method titles (a character vector)
#' @rdname get_mthd_titles
#' @export 
#' @importFrom purrr map_chr pluck
#' @importFrom stringr str_locate str_sub
#' @importFrom tools Rd_db
#' @keywords internal
get_mthd_titles <- function (mthd_nms_chr, pkg_nm_1L_chr = "ready4", path_1L_chr = character(0)) 
{
    mthd_titles_chr <- mthd_nms_chr %>% purrr::map_chr(~{
        mthd_nm_1L_chr <- .x
        df <- mthd_nm_1L_chr %>% stringr::str_locate("\\.")
        if (!is.na(df[[1, 1]])) {
            mthd_nm_1L_chr <- stringr::str_sub(mthd_nm_1L_chr, 
                end = (df[[1, 1]] - 1))
        }
        if (!identical(path_1L_chr, character(0))) {
            dmtn_tb <- tools::Rd_db(dir = path_1L_chr)
        }
        else {
            dmtn_tb <- tools::Rd_db(pkg_nm_1L_chr)
        }
        gnrc_dmt_ls <- dmtn_tb %>% purrr::pluck(paste0(mthd_nm_1L_chr, 
            "-methods.Rd"))
        ifelse(!is.null(gnrc_dmt_ls), gnrc_dmt_ls %>% purrr::pluck(1) %>% 
            purrr::pluck(1) %>% as.vector(), mthd_nm_1L_chr)
    })
    return(mthd_titles_chr)
}
#' Get ready4 S4 object slots
#' @description get_r4_obj_slots() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get ready4 s4 object slots. Function argument fn_name_1L_chr specifies the where to look for the required object. The function returns Slots (a character vector).
#' @param fn_name_1L_chr Function name (a character vector of length one)
#' @param package_1L_chr Package (a character vector of length one), Default: ''
#' @return Slots (a character vector)
#' @rdname get_r4_obj_slots
#' @export 
#' @importFrom methods className getSlots
#' @importFrom purrr map_chr
#' @keywords internal
get_r4_obj_slots <- function (fn_name_1L_chr, package_1L_chr = "") 
{
    slots_ls <- methods::className(fn_name_1L_chr, ifelse(package_1L_chr == 
        "", ".GlobalEnv", package_1L_chr)) %>% methods::getSlots()
    slots_chr <- purrr::map_chr(slots_ls, ~.x)
    return(slots_chr)
}
#' Get rds from dataverse
#' @description get_rds_from_dv() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get rds from dataverse. Function argument file_nm_1L_chr specifies the where to look for the required object. The function returns R object (an output object of multiple potential types).
#' @param file_nm_1L_chr File name (a character vector of length one)
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'https://doi.org/10.7910/DVN/2Y9VF9'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: character(0)
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return R object (an output object of multiple potential types)
#' @rdname get_rds_from_dv
#' @export 
#' @importFrom dataverse dataset_files
#' @importFrom purrr map_chr
#' @importFrom stringi stri_replace_last_regex
#' @keywords internal
get_rds_from_dv <- function (file_nm_1L_chr, dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9", 
    dv_url_pfx_1L_chr = character(0), key_1L_chr = NULL, server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    if (identical(dv_url_pfx_1L_chr, character(0))) 
        dv_url_pfx_1L_chr <- paste0("https://", server_1L_chr, 
            "/api/access/datafile/")
    ds_ls <- dataverse::dataset_files(dv_ds_nm_1L_chr, server = server_1L_chr, 
        key = key_1L_chr)
    all_items_chr <- purrr::map_chr(ds_ls, ~.x$label) %>% stringi::stri_replace_last_regex("\\.RDS", 
        "") %>% stringi::stri_replace_last_regex("\\.Rds", "") %>% 
        stringi::stri_replace_last_regex("\\.rds", "")
    idx_1L_int <- which(all_items_chr == file_nm_1L_chr)
    if (identical(idx_1L_int, integer(0))) {
        r_object_xx <- NULL
    }
    else {
        r_object_xx <- readRDS(url(paste0(dv_url_pfx_1L_chr, 
            ds_ls[[idx_1L_int]]$dataFile$id)))
    }
    return(r_object_xx)
}
#' Get source code urls
#' @description get_source_code_urls() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get source code urls. Function argument pkg_nm_1L_chr specifies the where to look for the required object. The function returns Urls (a character vector).
#' @param pkg_nm_1L_chr Package name (a character vector of length one), Default: 'ready4'
#' @param pkg_url_1L_chr Package url (a character vector of length one), Default: 'https://ready4-dev.github.io/ready4/index.html'
#' @return Urls (a character vector)
#' @rdname get_source_code_urls
#' @export 
#' @importFrom rvest read_html html_elements html_attr html_text
#' @keywords internal
get_source_code_urls <- function (pkg_nm_1L_chr = "ready4", pkg_url_1L_chr = "https://ready4-dev.github.io/ready4/index.html") 
{
    urls_chr <- rvest::read_html(pkg_url_1L_chr) %>% rvest::html_elements(".external-link") %>% 
        rvest::html_attr("href")
    indcs_int <- c(which((rvest::read_html(pkg_url_1L_chr) %>% 
        rvest::html_elements(".external-link") %>% rvest::html_text()) == 
        "Browse source code"), which(startsWith(urls_chr, "https://doi.org/10.5281/zenodo.")))
    urls_chr <- urls_chr[indcs_int]
    return(urls_chr)
}
#' Get subroutine repositories
#' @description get_subroutine_repos() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get subroutine repositories. Function argument gh_repo_1L_chr specifies the where to look for the required object. The function returns Subroutine repositories (a character vector).
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @return Subroutine repositories (a character vector)
#' @rdname get_subroutine_repos
#' @export 
#' @importFrom piggyback pb_download_url
#' @keywords internal
get_subroutine_repos <- function (gh_repo_1L_chr = "ready4-dev/ready4", gh_tag_1L_chr = "Documentation_0.0") 
{
    dmt_urls_chr <- piggyback::pb_download_url(repo = gh_repo_1L_chr, 
        tag = gh_tag_1L_chr, .token = "")
    if (any(dmt_urls_chr %>% endsWith("subroutine_repos_chr.RDS"))) {
        subroutine_repos_chr <- readRDS(url(dmt_urls_chr[dmt_urls_chr %>% 
            endsWith("subroutine_repos_chr.RDS")]))
    }
    else {
        subroutine_repos_chr <- character(0)
    }
    return(subroutine_repos_chr)
}
#' Get table from local file
#' @description get_table_from_loc_file() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get table from local file. Function argument path_1L_chr specifies the where to look for the required object. The function returns Table (an output object of multiple potential types).
#' @param path_1L_chr Path (a character vector of length one)
#' @param force_numeric_1L_lgl Force numeric (a logical vector of length one), Default: F
#' @param force_tb_1L_lgl Force tibble (a logical vector of length one), Default: F
#' @param heading_rows_1L_int Heading rows (an integer vector of length one), Default: 1
#' @return Table (an output object of multiple potential types)
#' @rdname get_table_from_loc_file
#' @export 
#' @importFrom tools file_ext
#' @importFrom readr read_csv
#' @importFrom readxl read_excel read_xlsx
#' @importFrom rlang exec
#' @importFrom tibble as_tibble
#' @importFrom dplyr slice n mutate across
#' @keywords internal
get_table_from_loc_file <- function (path_1L_chr, force_numeric_1L_lgl = F, force_tb_1L_lgl = F, 
    heading_rows_1L_int = 1L) 
{
    file_type_1L_chr <- path_1L_chr %>% tools::file_ext()
    fn <- switch(file_type_1L_chr, csv = readr::read_csv, xls = readxl::read_excel, 
        xlsx = readxl::read_xlsx, RDS = readRDS())
    table_xx <- rlang::exec(fn, path_1L_chr)
    if (force_tb_1L_lgl) 
        table_xx <- tibble::as_tibble(table_xx)
    if (heading_rows_1L_int > 1L) 
        table_xx <- table_xx %>% dplyr::slice(heading_rows_1L_int:dplyr::n())
    if (force_numeric_1L_lgl) {
        table_xx <- table_xx %>% dplyr::mutate(dplyr::across(where(is.character), 
            ~transform_chr_to_num(.x)))
    }
    return(table_xx)
}
