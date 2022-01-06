#' Get badge urls
#' @description get_badge_urls() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get badge urls. Function argument pkg_nm_1L_chr specifies the where to look for the required object. The function returns Badge urls (a list).
#' @param pkg_nm_1L_chr Package name (a character vector of length one)
#' @return Badge urls (a list)
#' @rdname get_badge_urls
#' @export 
#' @importFrom rvest read_html html_elements html_attr
#' @keywords internal
get_badge_urls <- function (pkg_nm_1L_chr) 
{
    images_chr <- rvest::read_html(paste0("https://ready4-dev.github.io/", 
        pkg_nm_1L_chr, "/index.html")) %>% rvest::html_elements("img") %>% 
        rvest::html_attr("src")
    badge_urls_ls <- list(ready4_1L_chr = images_chr[images_chr %>% 
        startsWith("https://img.shields.io/badge/ready4")], zenodo_1L_chr = images_chr[images_chr %>% 
        startsWith("https://zenodo.org/badge/DOI/")])
    return(badge_urls_ls)
}
#' Get badges lookup table
#' @description get_badges_lup() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get badges lookup table. Function argument gh_repo_1L_chr specifies the where to look for the required object. The function returns Ready4 badges (a lookup table).
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @return Ready4 badges (a lookup table)
#' @rdname get_badges_lup
#' @export 
#' @importFrom piggyback pb_download_url
#' @keywords internal
get_badges_lup <- function (gh_repo_1L_chr = "ready4-dev/ready4", gh_tag_1L_chr = "Documentation_0.0") 
{
    dmt_urls_chr <- piggyback::pb_download_url(repo = gh_repo_1L_chr, 
        tag = gh_tag_1L_chr, .token = "")
    ready4_badges_lup <- readRDS(url(dmt_urls_chr[dmt_urls_chr %>% 
        endsWith("ready4_badges_lup.RDS")]))
    return(ready4_badges_lup)
}
#' Get class extensions
#' @description get_cls_extensions() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get class extensions. Function argument pkg_extensions_tb specifies the where to look for the required object. The function returns Class extensions (a tibble).
#' @param pkg_extensions_tb Package extensions (a tibble)
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @return Class extensions (a tibble)
#' @rdname get_cls_extensions
#' @export 
#' @importFrom piggyback pb_download_url
#' @importFrom tibble as_tibble
#' @importFrom dplyr arrange filter select
#' @keywords internal
get_cls_extensions <- function (pkg_extensions_tb, gh_repo_1L_chr = "ready4-dev/ready4", 
    gh_tag_1L_chr = "Documentation_0.0") 
{
    dmt_urls_chr <- piggyback::pb_download_url(repo = gh_repo_1L_chr, 
        tag = gh_tag_1L_chr, .token = "")
    cls_extensions_tb <- readRDS(url(dmt_urls_chr[dmt_urls_chr %>% 
        endsWith("classes_lup.RDS")])) %>% tibble::as_tibble() %>% 
        dplyr::arrange(pt_ns_chr) %>% dplyr::filter(pt_ns_chr %in% 
        pkg_extensions_tb$pt_ns_chr) %>% dplyr::arrange(pt_ns_chr) %>% 
        dplyr::select(type_chr, pt_ns_chr, old_class_lgl)
    return(cls_extensions_tb)
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
#' @param return_1L_lgl Return (a logical vector of length one), Default: 'all'
#' @param exclude_mthds_for_chr Exclude methods for (a character vector), Default: 'NA'
#' @return Generics (a character vector)
#' @rdname get_generics
#' @export 
#' @importFrom methods getGenerics
#' @importFrom purrr map_lgl map flatten_chr
#' @keywords internal
get_generics <- function (pkg_nm_1L_chr = "ready4", return_1L_lgl = "all", exclude_mthds_for_chr = NA_character_) 
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
    if (return_1L_lgl == "core") {
        generics_chr <- generics_chr[tolower(generics_chr) == 
            generics_chr]
    }
    if (return_1L_lgl == "extended") 
        generics_chr <- generics_chr[tolower(generics_chr) != 
            generics_chr]
    if (return_1L_lgl == "slot") {
        generics_chr <- generics_chr[endsWith(generics_chr, "Slot")]
    }
    return(generics_chr)
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
#' Get method titles
#' @description get_mthd_titles() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get method titles. Function argument mthd_nms_chr specifies the where to look for the required object. The function returns Method titles (a character vector).
#' @param mthd_nms_chr Method names (a character vector)
#' @param pkg_nm_1L_chr Package name (a character vector of length one), Default: 'ready4'
#' @return Method titles (a character vector)
#' @rdname get_mthd_titles
#' @export 
#' @importFrom purrr map_chr pluck
#' @importFrom stringr str_locate str_sub
#' @importFrom tools Rd_db
#' @keywords internal
get_mthd_titles <- function (mthd_nms_chr, pkg_nm_1L_chr = "ready4") 
{
    mthd_titles_chr <- mthd_nms_chr %>% purrr::map_chr(~{
        mthd_nm_1L_chr <- .x
        df <- mthd_nm_1L_chr %>% stringr::str_locate("\\.")
        if (!is.na(df[[1, 1]])) {
            mthd_nm_1L_chr <- stringr::str_sub(mthd_nm_1L_chr, 
                end = (df[[1, 1]] - 1))
        }
        gnrc_dmt_ls <- tools::Rd_db(pkg_nm_1L_chr) %>% purrr::pluck(paste0(mthd_nm_1L_chr, 
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
