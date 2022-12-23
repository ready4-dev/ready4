#' Make code releases table
#' @description make_code_releases_tbl() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make code releases table. The function returns Releases (an output object of multiple potential types).
#' @param repo_type_1L_chr Repository type (a character vector of length one), Default: 'Framework'
#' @param as_kbl_1L_lgl As kable (a logical vector of length one), Default: T
#' @param brochure_repos_chr Brochure repositories (a character vector), Default: character(0)
#' @param exclude_chr Exclude (a character vector), Default: character(0)
#' @param format_1L_chr Format (a character vector of length one), Default: '%d-%b-%Y'
#' @param framework_repos_chr Framework repositories (a character vector), Default: character(0)
#' @param model_repos_chr Model repositories (a character vector), Default: character(0)
#' @param program_repos_chr Program repositories (a character vector), Default: character(0)
#' @param org_1L_chr Organisation (a character vector of length one), Default: 'ready4-dev'
#' @param repos_chr Repositories (a character vector), Default: character(0)
#' @param subroutine_repos_chr Subroutine repositories (a character vector), Default: character(0)
#' @param tidy_desc_1L_lgl Tidy description (a logical vector of length one), Default: T
#' @param ... Additional arguments
#' @return Releases (an output object of multiple potential types)
#' @rdname make_code_releases_tbl
#' @export 
#' @importFrom natmanager list_repo
#' @importFrom purrr map_dfr map2_chr map_chr
#' @importFrom tidyRSS tidyfeed
#' @importFrom dplyr arrange desc select mutate rename filter pull
#' @importFrom stringr str_remove_all str_remove
#' @importFrom rlang sym
#' @importFrom kableExtra cell_spec kable kable_styling column_spec spec_image
#' @keywords internal
make_code_releases_tbl <- function (repo_type_1L_chr = "Framework", as_kbl_1L_lgl = T, 
    brochure_repos_chr = character(0), exclude_chr = character(0), 
    format_1L_chr = "%d-%b-%Y", framework_repos_chr = character(0), 
    model_repos_chr = character(0), program_repos_chr = character(0), 
    org_1L_chr = "ready4-dev", repos_chr = character(0), subroutine_repos_chr = character(0), 
    tidy_desc_1L_lgl = T, ...) 
{
    if (identical(brochure_repos_chr, character(0))) {
        brochure_repos_chr <- "ready4web"
    }
    if (identical(exclude_chr, character(0))) {
        exclude_chr <- "rebuild"
    }
    if (identical(framework_repos_chr, character(0))) {
        framework_repos_chr <- make_framework_pkgs_chr()
    }
    if (identical(model_repos_chr, character(0))) {
        model_repos_chr <- make_modules_pkgs_chr(what_chr = "all")
    }
    if (identical(subroutine_repos_chr, character(0))) {
        subroutine_repos_chr <- make_subroutine_repos()
    }
    if (identical(program_repos_chr, character(0))) {
        program_repos_chr <- setdiff(natmanager::list_repo(org_1L_chr), 
            c(brochure_repos_chr, exclude_chr, framework_repos_chr, 
                model_repos_chr, subroutine_repos_chr))
    }
    if (identical(repos_chr, character(0))) {
        if (repo_type_1L_chr == "Framework") {
            repos_chr <- framework_repos_chr
        }
        if (repo_type_1L_chr == "Module") {
            repos_chr <- model_repos_chr
        }
        if (repo_type_1L_chr %in% c("Program", "Subroutine", 
            "Program_and_Subroutine")) {
            if (repo_type_1L_chr == "Subroutine") {
                repos_chr <- subroutine_repos_chr
            }
            if (repo_type_1L_chr == "Program") {
                repos_chr <- program_repos_chr
            }
            if (repo_type_1L_chr == "Program_and_Subroutine") {
                repos_chr <- c(program_repos_chr, subroutine_repos_chr)
            }
        }
        else {
            repo_type_1L_chr <- "Package"
        }
    }
    releases_xx <- repos_chr %>% purrr::map_dfr(~tidyRSS::tidyfeed(paste0("https://github.com/", 
        org_1L_chr, "/", .x, "/releases.atom"))) %>% dplyr::arrange(dplyr::desc(entry_last_updated)) %>% 
        dplyr::select(feed_title, entry_title, entry_last_updated, 
            entry_content, entry_link) %>% dplyr::mutate(feed_title = feed_title %>% 
        stringr::str_remove_all("Release notes from ")) %>% dplyr::rename(`:=`(!!rlang::sym(repo_type_1L_chr), 
        feed_title), Release = entry_title, Date = entry_last_updated, 
        Description = entry_content, URL = entry_link) %>% dplyr::filter(Release != 
        "Documentation_0.0")
    if (tidy_desc_1L_lgl) {
        releases_xx <- releases_xx %>% dplyr::mutate(Description = Description %>% 
            purrr::map2_chr(Program, ~stringr::str_remove(.x, 
                paste0(.y, ": "))))
    }
    if (as_kbl_1L_lgl) {
        releases_xx <- releases_xx %>% dplyr::mutate(Release = Release %>% 
            stringr::str_remove_all("Release ") %>% stringr::str_remove_all("v") %>% 
            kableExtra::cell_spec(format = "html", link = URL), 
            Date = Date %>% format.Date(format_1L_chr) %>% as.character()) %>% 
            dplyr::select(Date, !!rlang::sym(repo_type_1L_chr), 
                Release, Description)
        if (repo_type_1L_chr %in% c("Package", "Module", "Framework")) {
            logos_chr <- purrr::map_chr(releases_xx %>% dplyr::pull(repo_type_1L_chr), 
                ~paste0("https://ready4-dev.github.io/", .x, 
                  "/logo.png"))
            releases_xx <- releases_xx %>% dplyr::mutate(`:=`(!!rlang::sym(repo_type_1L_chr), 
                ""))
            indx_1L_int <- which(names(releases_xx) %in% c("Package", 
                "Module", "Framework"))
        }
        releases_xx <- releases_xx %>% kableExtra::kable("html", 
            escape = FALSE) %>% kableExtra::kable_styling(...)
        if (repo_type_1L_chr %in% c("Package", "Module", "Framework")) 
            releases_xx <- releases_xx %>% kableExtra::column_spec(indx_1L_int, 
                image = kableExtra::spec_image(logos_chr, height = 160, 
                  width = 160))
    }
    return(releases_xx)
}
#' Make datasets tibble
#' @description make_datasets_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make datasets tibble. The function returns Dataverses (a tibble).
#' @param dv_nm_1L_chr Dataverse name (a character vector of length one), Default: 'ready4'
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: 'dataverse.harvard.edu'
#' @return Dataverses (a tibble)
#' @rdname make_datasets_tb
#' @export 
#' @importFrom dataverse dataverse_contents get_dataverse dataset_metadata
#' @importFrom purrr map_lgl map_dfr map map_chr map2 discard flatten_chr compact
#' @importFrom tibble tibble
#' @importFrom dplyr mutate arrange
#' @keywords internal
make_datasets_tb <- function (dv_nm_1L_chr = "ready4", key_1L_chr = NULL, server_1L_chr = "dataverse.harvard.edu") 
{
    contents_ls <- dataverse::dataverse_contents(dv_nm_1L_chr, 
        key = key_1L_chr, server = server_1L_chr)
    dv_ls <- contents_ls[contents_ls %>% purrr::map_lgl(~.x$type == 
        "dataverse")]
    ds_ls <- contents_ls[contents_ls %>% purrr::map_lgl(~.x$type == 
        "dataset")]
    if (identical(ds_ls, list())) {
        ds_ls <- NULL
    }
    else {
        extra_dv_ls <- dataverse::get_dataverse(dv_nm_1L_chr, 
            key = key_1L_chr, server = server_1L_chr)
        dv_ls <- append(extra_dv_ls, dv_ls)
    }
    dvs_tb <- dv_ls %>% purrr::map_dfr(~{
        dv_ls <- dataverse::get_dataverse(.x, key = key_1L_chr, 
            server = server_1L_chr)
        tb <- tibble::tibble(Dataverse = dv_ls$alias, Name = dv_ls$name, 
            Description = dv_ls$description, Creator = dv_ls$affiliation)
        tb %>% dplyr::mutate(Contents = purrr::map(Dataverse, 
            ~{
                dv_all_ls <- dataverse::dataverse_contents(.x, 
                  key = key_1L_chr, server = server_1L_chr)
                dv_all_ls[dv_all_ls %>% purrr::map_lgl(~.x$type == 
                  "dataset")] %>% purrr::map_chr(~if ("persistentUrl" %in% 
                  names(.x)) {
                  .x$persistentUrl
                }
                else {
                  NA_character_
                })
            }))
    })
    dvs_tb <- dvs_tb %>% dplyr::mutate(Datasets_Meta = Contents %>% 
        purrr::map(~.x %>% purrr::map(~.x %>% dataverse::dataset_metadata(key = key_1L_chr, 
            server = server_1L_chr) %>% tryCatch(error = function(e) "ERROR")))) %>% 
        dplyr::mutate(Contents = Contents %>% purrr::map2(Datasets_Meta, 
            ~{
                entry_ls <- .x %>% purrr::map2(.y, ~if (identical(.y, 
                  "ERROR")) {
                  NA_character_
                }
                else {
                  .x
                }) %>% purrr::discard(is.na)
                if (identical(entry_ls, list())) {
                  NA_character_
                }
                else {
                  entry_ls %>% purrr::flatten_chr()
                }
            }))
    dvs_tb <- dvs_tb %>% dplyr::mutate(Datasets_Meta = Datasets_Meta %>% 
        purrr::map(~{
            entry_ls <- .x %>% purrr::map(~if (identical(.x, 
                "ERROR")) {
                NULL
            }
            else {
                .x
            }) %>% purrr::compact()
            if (identical(entry_ls, list())) {
                NULL
            }
            else {
                entry_ls
            }
        })) %>% dplyr::arrange(Dataverse)
    return(dvs_tb)
}
#' Make dataset releases table
#' @description make_ds_releases_tbl() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make dataset releases table. The function is called for its side effects and does not return a value.
#' @param ds_dois_chr Dataset digital object identifiers (a character vector)
#' @param format_1L_chr Format (a character vector of length one), Default: '%d-%b-%Y'
#' @param server_1L_chr Server (a character vector of length one), Default: 'dataverse.harvard.edu'
#' @return NULL
#' @rdname make_ds_releases_tbl
#' @export 
#' @importFrom purrr map_dfr
#' @importFrom dataverse dataset_versions
#' @importFrom tibble tibble
#' @importFrom dplyr arrange desc mutate
#' @keywords internal
make_ds_releases_tbl <- function (ds_dois_chr, format_1L_chr = "%d-%b-%Y", server_1L_chr = "dataverse.harvard.edu") 
{
    ds_dois_chr %>% purrr::map_dfr(~{
        meta_ls <- dataverse::dataset_versions(.x, server = server_1L_chr)
        doi_1L_chr <- .x
        1:length(meta_ls) %>% purrr::map_dfr(~tibble::tibble(Date = meta_ls[[.x]]$releaseTime, 
            DOI = paste0("https://doi.org/", doi_1L_chr), Version = paste0(meta_ls[[.x]]$versionNumber, 
                ".", meta_ls[[.x]]$versionMinorNumber), `Number of files` = length(meta_ls[[1]]$files)))
    }) %>% dplyr::arrange(dplyr::desc(Date)) %>% dplyr::mutate(Date = Date %>% 
        format.Date(format_1L_chr) %>% as.character())
}
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
#' Make function defaults list
#' @description make_fn_defaults_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make function defaults list. The function returns Function defaults (a list).
#' @param fn Function (a function)
#' @return Function defaults (a list)
#' @rdname make_fn_defaults_ls
#' @export 
#' @importFrom purrr map_lgl
#' @keywords internal
make_fn_defaults_ls <- function (fn) 
{
    fn_defaults_ls <- as.list(args(fn))
    fn_defaults_ls <- fn_defaults_ls[fn_defaults_ls %>% purrr::map_lgl(~!identical(.x %>% 
        deparse(), "deprecated()"))]
    fn_defaults_ls <- fn_defaults_ls[2:(length(fn_defaults_ls) - 
        1)]
    return(fn_defaults_ls)
}
#' Make framework packages character vector
#' @description make_framework_pkgs_chr() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make framework packages character vector. The function returns Framework packages (a character vector).

#' @return Framework packages (a character vector)
#' @rdname make_framework_pkgs_chr
#' @export 
#' @keywords internal
make_framework_pkgs_chr <- function () 
{
    framework_pkgs_chr <- c("ready4", "ready4fun", "ready4class", 
        "ready4pack", "ready4use", "ready4show")
    return(framework_pkgs_chr)
}
#' Make libraries tibble
#' @description make_libraries_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make libraries tibble. The function returns Package extensions (a tibble).
#' @param include_1L_chr Include (a character vector of length one), Default: 'modules'
#' @param ns_var_nm_1L_chr Namespace variable name (a character vector of length one), Default: 'pt_ns_chr'
#' @param reference_var_nm_1L_chr Reference variable name (a character vector of length one), Default: 'Reference'
#' @param url_stub_1L_chr Url stub (a character vector of length one), Default: 'https://ready4-dev.github.io/'
#' @param vignette_var_nm_1L_chr Vignette variable name (a character vector of length one), Default: 'Vignettes'
#' @param vignette_url_var_nm_1L_chr Vignette url variable name (a character vector of length one), Default: 'Vignettes_URLs'
#' @return Package extensions (a tibble)
#' @rdname make_libraries_tb
#' @export 
#' @importFrom tibble tibble
#' @importFrom dplyr mutate case_when arrange desc select rename left_join
#' @importFrom purrr map_chr map2 map_dfr
#' @importFrom kableExtra cell_spec
#' @importFrom rvest read_html html_elements html_text2
#' @importFrom bib2df bib2df
make_libraries_tb <- function (include_1L_chr = "modules", ns_var_nm_1L_chr = "pt_ns_chr", 
    reference_var_nm_1L_chr = "Reference", url_stub_1L_chr = "https://ready4-dev.github.io/", 
    vignette_var_nm_1L_chr = "Vignettes", vignette_url_var_nm_1L_chr = "Vignettes_URLs") 
{
    modules_chr <- make_modules_pkgs_chr()
    fw_chr <- make_framework_pkgs_chr()
    if (include_1L_chr == "modules") {
        libraries_chr <- modules_chr
    }
    else {
        if (include_1L_chr == "framework") {
            libraries_chr <- fw_chr
        }
        if (include_1L_chr == "all") 
            libraries_chr <- c(fw_chr, modules_chr)
    }
    pkg_extensions_tb <- tibble::tibble(pt_ns_chr = libraries_chr) %>% 
        dplyr::mutate(Type = dplyr::case_when(pt_ns_chr == "ready4" ~ 
            "Foundation", pt_ns_chr == "ready4class" ~ "Authoring (code - classes)", 
            pt_ns_chr == "ready4fun" ~ "Authoring (code - functions)", 
            pt_ns_chr == "ready4pack" ~ "Authoring (code - libraries)", 
            pt_ns_chr == "ready4show" ~ "Authoring (code - programs)", 
            pt_ns_chr == "ready4use" ~ "Authoring (datasets)", 
            pt_ns_chr == "youthvars" ~ "Description (datasets)", 
            pt_ns_chr == "scorz" ~ "Description (variable scoring)", 
            pt_ns_chr == "specific" ~ "Modelling (inverse problems)", 
            pt_ns_chr == "heterodox" ~ "Modelling (heterogeneity)", 
            pt_ns_chr == "mychoice" ~ "Modelling (choice)", pt_ns_chr == 
                "TTU" ~ "Modelling (health utility)", pt_ns_chr == 
                "youthu" ~ "Prediction (health utility)", T ~ 
                ""))
    if (include_1L_chr == "framework") {
        pkg_extensions_tb <- pkg_extensions_tb %>% dplyr::arrange(dplyr::desc(Type), 
            pt_ns_chr)
    }
    else {
        pkg_extensions_tb <- pkg_extensions_tb %>% dplyr::arrange(Type, 
            pt_ns_chr)
    }
    pkg_extensions_tb <- pkg_extensions_tb %>% dplyr::mutate(Link = purrr::map_chr(pt_ns_chr, 
        ~paste0(url_stub_1L_chr, .x, "/index", ".html"))) %>% 
        dplyr::mutate(Library = kableExtra::cell_spec(pt_ns_chr, 
            "html", link = Link))
    pkg_extensions_tb <- add_vignette_links(pkg_extensions_tb, 
        ns_var_nm_1L_chr = ns_var_nm_1L_chr, reference_var_nm_1L_chr = reference_var_nm_1L_chr, 
        url_stub_1L_chr = url_stub_1L_chr, vignette_var_nm_1L_chr = vignette_var_nm_1L_chr, 
        vignette_url_var_nm_1L_chr = vignette_url_var_nm_1L_chr)
    pkg_extensions_tb <- pkg_extensions_tb %>% dplyr::mutate(Citation = paste0(url_stub_1L_chr, 
        pt_ns_chr, "/authors.html")) %>% dplyr::mutate(manual_urls_ls = purrr::map2(pt_ns_chr, 
        Link, ~get_manual_urls(.x, pkg_url_1L_chr = .y))) %>% 
        dplyr::mutate(code_urls_ls = purrr::map2(pt_ns_chr, Link, 
            ~get_source_code_urls(.x, pkg_url_1L_chr = .y)))
    y_tb <- purrr::map_dfr(pkg_extensions_tb$Citation, ~{
        if (T) {
            f <- tempfile(fileext = ".bib")
            sink(f)
            writeLines(rvest::read_html(.x) %>% rvest::html_elements("pre") %>% 
                rvest::html_text2())
            sink(NULL)
            suppressWarnings(bib2df::bib2df(f)) %>% dplyr::select(AUTHOR, 
                TITLE, DOI)
        }
        else {
        }
    }) %>% dplyr::mutate(pt_ns_chr = pkg_extensions_tb$pt_ns_chr) %>% 
        dplyr::rename(DOI_chr = DOI, Title = TITLE, Authors = AUTHOR)
    pkg_extensions_tb <- dplyr::left_join(pkg_extensions_tb, 
        y_tb, by = "pt_ns_chr")
    return(pkg_extensions_tb)
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
#' @param include_1L_chr Include (a character vector of length one), Default: 'modules'
#' @param ns_var_nm_1L_chr Namespace variable name (a character vector of length one), Default: 'pt_ns_chr'
#' @param path_1L_chr Path (a character vector of length one), Default: character(0)
#' @param reference_var_nm_1L_chr Reference variable name (a character vector of length one), Default: 'Reference'
#' @param return_1L_chr Return (a character vector of length one), Default: 'all'
#' @param url_stub_1L_chr Url stub (a character vector of length one), Default: 'https://ready4-dev.github.io/'
#' @param vignette_var_nm_1L_chr Vignette variable name (a character vector of length one), Default: 'Vignettes'
#' @param vignette_url_var_nm_1L_chr Vignette url variable name (a character vector of length one), Default: 'Vignettes_URLs'
#' @return Methods (a tibble)
#' @rdname make_methods_tb
#' @export 
#' @importFrom tibble tibble
#' @importFrom purrr map flatten_chr discard
make_methods_tb <- function (packages_tb = NULL, exclude_mthds_for_chr = NA_character_, 
    include_1L_chr = "modules", ns_var_nm_1L_chr = "pt_ns_chr", 
    path_1L_chr = character(0), reference_var_nm_1L_chr = "Reference", 
    return_1L_chr = "all", url_stub_1L_chr = "https://ready4-dev.github.io/", 
    vignette_var_nm_1L_chr = "Vignettes", vignette_url_var_nm_1L_chr = "Vignettes_URLs") 
{
    if (is.null(packages_tb)) {
        packages_tb <- make_libraries_tb(include_1L_chr = include_1L_chr, 
            ns_var_nm_1L_chr = ns_var_nm_1L_chr, reference_var_nm_1L_chr = reference_var_nm_1L_chr, 
            url_stub_1L_chr = url_stub_1L_chr, vignette_var_nm_1L_chr = vignette_var_nm_1L_chr, 
            vignette_url_var_nm_1L_chr = vignette_url_var_nm_1L_chr)
    }
    methods_tb <- tibble::tibble(Method = get_generics(exclude_mthds_for_chr = exclude_mthds_for_chr, 
        return_1L_chr = return_1L_chr), Purpose = get_mthd_titles(Method, 
        path_1L_chr = path_1L_chr), Examples = purrr::map(Method, 
        ~get_examples(packages_tb$Vignettes_URLs %>% purrr::flatten_chr() %>% 
            unique() %>% purrr::discard(is.na), term_1L_chr = .x)))
    return(methods_tb)
}
#' Make modules packages character vector
#' @description make_modules_pkgs_chr() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make modules packages character vector. The function returns Modules packages (a character vector).
#' @param what_chr What (a character vector), Default: 'all'
#' @return Modules packages (a character vector)
#' @rdname make_modules_pkgs_chr
#' @export 
#' @keywords internal
make_modules_pkgs_chr <- function (what_chr = "all") 
{
    modules_pkgs_chr <- character(0)
    if ("people" %in% what_chr | "all" %in% what_chr) 
        modules_pkgs_chr <- c(modules_pkgs_chr, c("youthvars", 
            "scorz", "specific", "TTU", "youthu", "mychoice", 
            "heterodox"))
    if ("places" %in% what_chr) 
        modules_pkgs_chr <- c(modules_pkgs_chr, character(0))
    if ("platforms" %in% what_chr) 
        modules_pkgs_chr <- c(modules_pkgs_chr, ccharacter(0))
    if ("programs" %in% what_chr) 
        modules_pkgs_chr <- c(modules_pkgs_chr, c("bimp"))
    return(modules_pkgs_chr)
}
#' Make modules tibble
#' @description make_modules_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make modules tibble. The function returns Modules (a tibble).
#' @param pkg_extensions_tb Package extensions (a tibble), Default: NULL
#' @param cls_extensions_tb Class extensions (a tibble), Default: NULL
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @param include_1L_chr Include (a character vector of length one), Default: 'modules'
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
    gh_repo_1L_chr = "ready4-dev/ready4", gh_tag_1L_chr = "Documentation_0.0", 
    include_1L_chr = "modules") 
{
    if (is.null(pkg_extensions_tb)) 
        pkg_extensions_tb <- make_libraries_tb(include_1L_chr = include_1L_chr)
    if (is.null(cls_extensions_tb)) 
        cls_extensions_tb <- get_cls_extensions(pkg_extensions_tb, 
            gh_repo_1L_chr = gh_repo_1L_chr, gh_tag_1L_chr = gh_tag_1L_chr, 
            validate_1L_lgl = T)
    modules_tb <- dplyr::inner_join(cls_extensions_tb, pkg_extensions_tb, 
        by = "pt_ns_chr") %>% dplyr::mutate(Class = purrr::pmap(list(pt_ns_chr, 
        type_chr, old_class_lgl), ~{
        kableExtra::cell_spec(..2, "html", link = paste0("https://ready4-dev.github.io/", 
            ..1, "/reference/", ifelse(..3, ..2, paste0(..2, 
                "-class")), ".html"))
    })) %>% dplyr::mutate(Examples = purrr::map2(Vignettes_URLs, 
        type_chr, ~get_examples(.x, term_1L_chr = .y)))
    modules_tb <- modules_tb %>% dplyr::mutate(Description = purrr::map2_chr(Class, 
        old_class_lgl, ~{
            rvest::read_html((.x %>% stringr::str_match("href=\"\\s*(.*?)\\s*\" style"))[, 
                2]) %>% rvest::html_elements(ifelse(.y, "h1", 
                "p")) %>% rvest::html_text2() %>% purrr::pluck(1)
        }) %>% stringi::stri_replace_last_regex("\\.", "")) %>% 
        dplyr::select(Class, Description, Library, Examples, 
            old_class_lgl)
    return(modules_tb)
}
#' Make programs table
#' @description make_programs_tbl() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make programs table. The function returns Programs (an output object of multiple potential types).
#' @param what_1L_chr What (a character vector of length one), Default: 'Program'
#' @param as_kbl_1L_lgl As kable (a logical vector of length one), Default: F
#' @param exclude_chr Exclude (a character vector), Default: 'dce_sa_cards'
#' @param format_1L_chr Format (a character vector of length one), Default: '%d-%b-%Y'
#' @param tidy_desc_1L_lgl Tidy description (a logical vector of length one), Default: T
#' @param zenodo_1L_chr Zenodo (a character vector of length one), Default: 'ready4'
#' @param ... Additional arguments
#' @return Programs (an output object of multiple potential types)
#' @rdname make_programs_tbl
#' @export 
#' @importFrom dplyr group_by filter row_number arrange ungroup mutate select
#' @importFrom rlang sym
#' @importFrom zen4R ZenodoManager
#' @importFrom purrr map_chr map_int map2_chr pmap
#' @importFrom rvest html_text read_html
#' @importFrom stringr str_remove_all str_replace_all str_remove
#' @importFrom kableExtra cell_spec kable kable_styling
#' @keywords internal
make_programs_tbl <- function (what_1L_chr = "Program", as_kbl_1L_lgl = F, exclude_chr = "dce_sa_cards", 
    format_1L_chr = "%d-%b-%Y", tidy_desc_1L_lgl = T, zenodo_1L_chr = "ready4", 
    ...) 
{
    programs_xx <- make_code_releases_tbl(what_1L_chr, as_kbl_1L_lgl = F, 
        exclude_chr = exclude_chr, tidy_desc_1L_lgl = F) %>% 
        dplyr::group_by(!!rlang::sym(what_1L_chr)) %>% dplyr::filter(dplyr::row_number() == 
        1) %>% dplyr::arrange(!!rlang::sym(what_1L_chr)) %>% 
        dplyr::ungroup()
    zenodo_records_ls <- zen4R::ZenodoManager$new()
    zenodo_records_ls <- zenodo_records_ls$getRecords(q = paste0("communities:(", 
        zenodo_1L_chr, ")"))
    descriptions_chr <- zenodo_records_ls %>% purrr::map_chr(~rvest::html_text(rvest::read_html(.x$metadata$description %>% 
        stringr::str_remove_all("&nbsp;"))) %>% stringr::str_replace_all("[\r\n]", 
        ""))
    indices_int <- programs_xx$Description %>% purrr::map_int(~which(.x == 
        descriptions_chr))
    programs_xx$DOI <- indices_int %>% purrr::map_chr(~zenodo_records_ls[[.x]]$links$conceptdoi)
    programs_xx$GitHub <- gsub("/releases/.*", "", programs_xx$URL)
    if (tidy_desc_1L_lgl) 
        programs_xx <- programs_xx %>% dplyr::mutate(Description = Description %>% 
            purrr::map2_chr(!!rlang::sym(what_1L_chr), ~stringr::str_remove(.x, 
                paste0(.y, ": "))))
    if (as_kbl_1L_lgl) {
        programs_xx <- programs_xx %>% dplyr::mutate(Release = Release %>% 
            stringr::str_remove_all("Release ") %>% stringr::str_remove_all("v"), 
            Date = Date %>% format.Date(format_1L_chr) %>% as.character()) %>% 
            dplyr::mutate(Source = purrr::pmap(list(GitHub, DOI), 
                ~{
                  kableExtra::cell_spec(c("Dev", "Archive"), 
                    format = "html", link = c(..1, ..2))
                })) %>% dplyr::select(!!rlang::sym(what_1L_chr), 
            Release, Date, Description, Source)
        programs_xx <- programs_xx %>% kableExtra::kable("html", 
            escape = FALSE) %>% kableExtra::kable_styling(...)
    }
    return(programs_xx)
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
#' Make subroutine repositories
#' @description make_subroutine_repos() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make subroutine repositories. The function returns Subroutine repositories (a character vector).

#' @return Subroutine repositories (a character vector)
#' @rdname make_subroutine_repos
#' @export 
#' @keywords internal
make_subroutine_repos <- function () 
{
    subroutine_repos_chr <- c("mychoice_results", "ttu_mdl_ctlg", 
        "ms_tmpl", "ttu_lng_ss")
    return(subroutine_repos_chr)
}
