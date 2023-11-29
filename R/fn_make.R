#' Make additions tibble
#' @description make_additions_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make additions tibble. The function returns Additions (a tibble).
#' @param category_chr Category (a character vector), Default: character(0)
#' @param library_chr Library (a character vector), Default: character(0)
#' @param type_chr Type (a character vector), Default: character(0)
#' @param url_stub_1L_chr Url stub (a character vector of length one), Default: 'https://ready4-dev.github.io/'
#' @return Additions (a tibble)
#' @rdname make_additions_tb
#' @export 
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @keywords internal
make_additions_tb <- function (category_chr = character(0), library_chr = character(0), 
    type_chr = character(0), url_stub_1L_chr = "https://ready4-dev.github.io/") 
{
    additions_tb <- tibble::tibble(library_chr = library_chr, 
        category_chr = category_chr, type_chr = type_chr) %>% 
        dplyr::mutate(Link = paste0(url_stub_1L_chr, library_chr, 
            "/index.html"))
    return(additions_tb)
}
#' Make a tabular summary of release history of ready4 code libraries and executables
#' @description make_code_releases_tbl() scrapes the details of a specified GitHub repository to generate a release history of ready libraries and executables. To work all repositories without any release need to be supplied using the 'exclude_chr' argument.
#' @param repo_type_1L_chr Repository type (a character vector of length one), Default: c("Framework", "Module", "Package", "Program", "Subroutine", 
#'    "Program_and_Subroutine")
#' @param as_kbl_1L_lgl As kable (a logical vector of length one), Default: T
#' @param brochure_repos_chr Brochure repositories (a character vector), Default: character(0)
#' @param exclude_chr Exclude (a character vector), Default: character(0)
#' @param format_1L_chr Format (a character vector of length one), Default: '%d-%b-%Y'
#' @param framework_repos_chr Framework repositories (a character vector), Default: character(0)
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @param model_repos_chr Model repositories (a character vector), Default: character(0)
#' @param program_repos_chr Program repositories (a character vector), Default: character(0)
#' @param org_1L_chr Organisation (a character vector of length one), Default: 'ready4-dev'
#' @param repos_chr Repositories (a character vector), Default: character(0)
#' @param subroutine_repos_chr Subroutine repositories (a character vector), Default: character(0)
#' @param tidy_desc_1L_lgl Tidy description (a logical vector of length one), Default: T
#' @param url_stub_1L_chr Url stub (a character vector of length one), Default: 'https://ready4-dev.github.io/'
#' @param ... Additional arguments
#' @return Releases (an output object of multiple potential types)
#' @rdname make_code_releases_tbl
#' @export 
#' @importFrom purrr map_dfr map2_chr map_chr
#' @importFrom tidyRSS tidyfeed
#' @importFrom dplyr arrange desc select mutate rename filter pull
#' @importFrom stringr str_remove_all str_remove
#' @importFrom rlang sym
#' @importFrom kableExtra cell_spec kable kable_styling column_spec spec_image
#' @examplesIf interactive()
#'   # Likely to take more than one minute to execute.
#'     if(requireNamespace("tidyRSS", quietly = TRUE)) {
#'       make_code_releases_tbl("Framework",
#'                              gh_repo_1L_chr = "ready4-dev/ready4")
#'       make_code_releases_tbl("Module",
#'                              gh_repo_1L_chr = "ready4-dev/ready4")
#'       make_code_releases_tbl("Program",
#'                              gh_repo_1L_chr = "ready4-dev/ready4")
#'       make_code_releases_tbl("Subroutine",
#'                              gh_repo_1L_chr = "ready4-dev/ready4")
#'     }
make_code_releases_tbl <- function (repo_type_1L_chr = c("Framework", "Module", "Package", 
    "Program", "Subroutine", "Program_and_Subroutine"), as_kbl_1L_lgl = T, 
    brochure_repos_chr = character(0), exclude_chr = character(0), 
    format_1L_chr = "%d-%b-%Y", framework_repos_chr = character(0), 
    gh_repo_1L_chr = "ready4-dev/ready4", gh_tag_1L_chr = "Documentation_0.0", 
    model_repos_chr = character(0), program_repos_chr = character(0), 
    org_1L_chr = "ready4-dev", repos_chr = character(0), subroutine_repos_chr = character(0), 
    tidy_desc_1L_lgl = T, url_stub_1L_chr = "https://ready4-dev.github.io/", 
    ...) 
{
    if (!requireNamespace("tidyRSS", quietly = TRUE)) {
        stop("tidyRSS package is required - please install it and rerun the last command.")
    }
    repo_type_1L_chr <- match.arg(repo_type_1L_chr)
    if (identical(brochure_repos_chr, character(0))) {
        brochure_repos_chr <- "ready4web"
    }
    if (identical(exclude_chr, character(0))) {
        exclude_chr <- get_excluded_repos(gh_repo_1L_chr = gh_repo_1L_chr, 
            gh_tag_1L_chr = gh_tag_1L_chr)
    }
    if (identical(framework_repos_chr, character(0))) {
        framework_repos_chr <- make_framework_pkgs_chr(gh_repo_1L_chr = gh_repo_1L_chr, 
            gh_tag_1L_chr = gh_tag_1L_chr)
    }
    if (identical(model_repos_chr, character(0))) {
        model_repos_chr <- make_modules_pkgs_chr(gh_repo_1L_chr = gh_repo_1L_chr, 
            gh_tag_1L_chr = gh_tag_1L_chr, what_chr = "all")
    }
    if (identical(subroutine_repos_chr, character(0))) {
        subroutine_repos_chr <- get_subroutine_repos(gh_repo_1L_chr = gh_repo_1L_chr, 
            gh_tag_1L_chr = gh_tag_1L_chr)
    }
    if (identical(program_repos_chr, character(0))) {
        program_repos_chr <- setdiff(get_gh_repos(org_1L_chr), 
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
        org_1L_chr, "/", .x, "/releases.atom"))) %>% dplyr::arrange(dplyr::desc(.data$entry_last_updated)) %>% 
        dplyr::select("feed_title", "entry_title", "entry_last_updated", 
            "entry_content", "entry_link") %>% dplyr::mutate(feed_title = .data$feed_title %>% 
        stringr::str_remove_all("Release notes from ")) %>% dplyr::rename(`:=`(!!rlang::sym(repo_type_1L_chr), 
        "feed_title"), Release = "entry_title", Date = "entry_last_updated", 
        Description = "entry_content", URL = "entry_link") %>% 
        dplyr::filter(.data$Release != "Documentation_0.0")
    if (tidy_desc_1L_lgl) {
        releases_xx <- releases_xx %>% dplyr::mutate(Description = .data$Description %>% 
            purrr::map2_chr(!!rlang::sym(repo_type_1L_chr), ~stringr::str_remove(.x, 
                paste0(.y, ": "))))
    }
    if (as_kbl_1L_lgl) {
        releases_xx <- releases_xx %>% dplyr::mutate(Release = .data$Release %>% 
            stringr::str_remove_all("Release ") %>% stringr::str_remove_all("v") %>% 
            kableExtra::cell_spec(format = "html", link = .data$URL), 
            Date = .data$Date %>% format.Date(format_1L_chr) %>% 
                as.character()) %>% dplyr::select("Date", !!rlang::sym(repo_type_1L_chr), 
            "Release", "Description")
        if (repo_type_1L_chr %in% c("Package", "Module", "Framework")) {
            logos_chr <- purrr::map_chr(releases_xx %>% dplyr::pull(repo_type_1L_chr), 
                ~paste0(url_stub_1L_chr, .x, "/logo.png"))
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
#' Make a tabular summary of ready4 model data collections
#' @description make_datasts_tb() function searches the contents of a specified Dataverse collection and returns a summary of the the data collections it contains.
#' @param dv_nm_1L_chr Dataverse name (a character vector of length one), Default: 'ready4'
#' @param dvs_tb Dataverses (a tibble), Default: NULL
#' @param filter_cdns_ls Filter conditions (a list), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: 'dataverse.harvard.edu'
#' @param toy_data_dv_1L_chr Toy data dataverse (a character vector of length one), Default: 'fakes'
#' @param type_1L_chr Type (a character vector of length one), Default: c("collections", "datasets")
#' @param what_1L_chr What (a character vector of length one), Default: 'all'
#' @return Datasets (a tibble)
#' @rdname make_datasets_tb
#' @export 
#' @importFrom dataverse dataverse_contents get_dataverse dataset_metadata
#' @importFrom purrr map_lgl map_dfr map map_chr map2 discard flatten_chr compact
#' @importFrom tibble tibble
#' @importFrom dplyr mutate arrange filter
#' @examplesIf interactive()
#'   # Likely to take more than one minute to execute.
#'   make_datasets_tb("ready4")
#'   dvs_tb <- get_datasets_tb("ready4-dev/ready4")
#'   make_datasets_tb("ready4", dvs_tb = dvs_tb)
#'   make_datasets_tb("ready4", dvs_tb = dvs_tb, what_1L_chr = "real")
#'   make_datasets_tb("ready4", dvs_tb = dvs_tb, what_1L_chr = "fakes")
#'   make_datasets_tb("ready4", dvs_tb = dvs_tb, type_1L_chr = "datasets")
#'   make_datasets_tb("ready4", dvs_tb = dvs_tb, type_1L_chr = "datasets", what_1L_chr = "real")
#'   make_datasets_tb("ready4", dvs_tb = dvs_tb, type_1L_chr = "datasets", what_1L_chr = "fakes")
make_datasets_tb <- function (dv_nm_1L_chr = "ready4", dvs_tb = NULL, filter_cdns_ls = NULL, 
    key_1L_chr = NULL, server_1L_chr = "dataverse.harvard.edu", 
    toy_data_dv_1L_chr = "fakes", type_1L_chr = c("collections", 
        "datasets"), what_1L_chr = "all") 
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (is.null(dvs_tb)) {
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
            tb %>% dplyr::mutate(Contents = purrr::map(.data$Dataverse, 
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
        dvs_tb <- dvs_tb %>% dplyr::mutate(Datasets_Meta = .data$Contents %>% 
            purrr::map(~.x %>% purrr::map(~.x %>% dataverse::dataset_metadata(key = key_1L_chr, 
                server = server_1L_chr) %>% tryCatch(error = function(e) "ERROR")))) %>% 
            dplyr::mutate(Contents = .data$Contents %>% purrr::map2(.data$Datasets_Meta, 
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
        dvs_tb <- dvs_tb %>% dplyr::mutate(Datasets_Meta = .data$Datasets_Meta %>% 
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
            })) %>% dplyr::arrange(.data$Dataverse)
    }
    if (type_1L_chr == "datasets") {
        datasets_tb <- make_dss_tb(dvs_tb, filter_cdns_ls = filter_cdns_ls, 
            toy_data_dv_1L_chr = toy_data_dv_1L_chr, what_1L_chr = what_1L_chr)
    }
    else {
        if (what_1L_chr == "real") 
            dvs_tb <- dvs_tb %>% dplyr::filter(.data$Dataverse != 
                toy_data_dv_1L_chr)
        if (what_1L_chr == "fakes") 
            dvs_tb <- dvs_tb %>% dplyr::filter(.data$Dataverse == 
                toy_data_dv_1L_chr)
        datasets_tb <- dvs_tb
    }
    return(datasets_tb)
}
#' Make a tabular summary of release history of ready4 model data collections
#' @description make_datasts_tb() scrapes metadata from a specified Dataverse collection to create a summary table of its contents. The contents table can detail either subsidiary data collections or individual datasets from those subsidiary data collections.
#' @param ds_dois_chr Dataset digital object identifiers (a character vector)
#' @param format_1L_chr Format (a character vector of length one), Default: '%d-%b-%Y'
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: 'dataverse.harvard.edu'
#' @param as_kbl_1L_lgl As kable (a logical vector of length one), Default: T
#' @param ... Additional arguments
#' @return Dataset releases (an output object of multiple potential types)
#' @rdname make_ds_releases_tbl
#' @export 
#' @importFrom purrr map_dfr
#' @importFrom dataverse dataset_versions
#' @importFrom tibble tibble
#' @importFrom dplyr arrange desc mutate filter select
#' @importFrom kableExtra cell_spec kable kable_styling
#' @example man/examples/make_ds_releases_tbl.R
make_ds_releases_tbl <- function (ds_dois_chr, format_1L_chr = "%d-%b-%Y", key_1L_chr = NULL, 
    server_1L_chr = "dataverse.harvard.edu", as_kbl_1L_lgl = T, 
    ...) 
{
    ds_releases_xx <- ds_dois_chr %>% purrr::map_dfr(~{
        meta_ls <- dataverse::dataset_versions(.x, server = server_1L_chr, 
            key = key_1L_chr)
        doi_1L_chr <- .x
        1:length(meta_ls) %>% purrr::map_dfr(~tibble::tibble(Date = meta_ls[[.x]]$releaseTime, 
            Dataset = meta_ls[[1]]$metadataBlocks$citation$fields[[1]]$value, 
            DOI = paste0("https://doi.org/", doi_1L_chr), Version = paste0(meta_ls[[.x]]$versionNumber, 
                ".", meta_ls[[.x]]$versionMinorNumber), `Number of files` = length(meta_ls[[1]]$files)))
    }) %>% dplyr::arrange(dplyr::desc(.data$Date)) %>% dplyr::mutate(Date = .data$Date %>% 
        format.Date(format_1L_chr) %>% as.character()) %>% dplyr::filter(!is.na(.data$Date))
    if (as_kbl_1L_lgl) {
        ds_releases_xx <- ds_releases_xx %>% dplyr::mutate(Dataset = .data$Dataset %>% 
            kableExtra::cell_spec(format = "html", link = .data$DOI)) %>% 
            dplyr::select("Date", "Dataset", "Version", "Number of files")
        ds_releases_xx <- ds_releases_xx %>% kableExtra::kable("html", 
            escape = FALSE) %>% kableExtra::kable_styling(...)
    }
    return(ds_releases_xx)
}
#' Make datasets tibble
#' @description make_dss_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make datasets tibble. The function returns Datasets (a tibble).
#' @param dvs_tb Dataverses (a tibble)
#' @param filter_cdns_ls Filter conditions (a list), Default: list()
#' @param toy_data_dv_1L_chr Toy data dataverse (a character vector of length one), Default: 'fakes'
#' @param what_1L_chr What (a character vector of length one), Default: 'all'
#' @return Datasets (a tibble)
#' @rdname make_dss_tb
#' @export 
#' @importFrom dplyr filter select mutate
#' @importFrom purrr pmap_dfr map_dfr reduce pluck
#' @importFrom tibble tibble
#' @keywords internal
make_dss_tb <- function (dvs_tb, filter_cdns_ls = list(), toy_data_dv_1L_chr = "fakes", 
    what_1L_chr = "all") 
{
    if ("Datasets_Meta" %in% names(dvs_tb)) {
        dss_tb <- dvs_tb %>% dplyr::filter(!is.na(.data$Contents)) %>% 
            dplyr::select("Contents", "Datasets_Meta", "Dataverse") %>% 
            purrr::pmap_dfr(~{
                ..2 %>% purrr::map_dfr(~{
                  fields_ls <- .x$fields
                  tibble::tibble(Title = fields_ls$value[which(fields_ls$typeName == 
                    "title")][[1]], Description = fields_ls$value[which(fields_ls$typeName == 
                    "dsDescription")][[1]][[1]][[4]])
                }) %>% dplyr::mutate(Dataverse = ..3, DOI = ..1)
            })
    }
    else {
        dss_tb <- dvs_tb
    }
    if (!is.null(filter_cdns_ls)) {
        if (identical(filter_cdns_ls, list())) {
            filter_cdns_ls <- list(people = "Dataverse %in% c(\"TTU\", \"springtolife\")", 
                places = "Dataverse %in% c(\"springtides\") | DOI == \"https://doi.org/10.7910/DVN/JHSCDJ\"")
        }
        dss_tb <- purrr::reduce(1:length(filter_cdns_ls), .init = dss_tb, 
            ~{
                condition_1L_chr = filter_cdns_ls %>% purrr::pluck(.y)
                if (names(filter_cdns_ls)[.y] == what_1L_chr) {
                  .x %>% update_tb_r3(filter_cdn_1L_chr = condition_1L_chr)
                }
                else {
                  .x
                }
            })
    }
    if (what_1L_chr == "real") 
        dss_tb <- dss_tb %>% dplyr::filter(.data$Dataverse != 
            toy_data_dv_1L_chr)
    if (what_1L_chr == "fakes") 
        dss_tb <- dss_tb %>% dplyr::filter(.data$Dataverse == 
            toy_data_dv_1L_chr)
    return(dss_tb)
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
        files_tb <- files_tb %>% dplyr::filter(.data$file_type_chr %in% 
            inc_fl_types_chr)
    files_tb <- files_tb %>% dplyr::filter(.data$file_chr %in% 
        names(recode_ls))
    description_chr <- purrr::map_chr(files_tb$file_chr, ~{
        arg_ls <- append(list(EXPR = .x), recode_ls)
        rlang::exec(.fn = switch, !!!arg_ls)
    })
    files_tb <- files_tb %>% dplyr::mutate(description_chr = description_chr, 
        ds_file_ext_chr = purrr::map_chr(.data$file_type_chr, 
            ~ifelse(.x %in% c(".csv", ".xls", ".xlsx"), ".tab", 
                ".zip")))
    if (nrow(files_tb) != (paste0(files_tb$file_chr, files_tb$file_type_chr) %>% 
        unique() %>% length())) {
        stop("The columns file_chr and file_type_chr must be of the same length.")
    }
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
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @return Framework packages (a character vector)
#' @rdname make_framework_pkgs_chr
#' @export 
#' @importFrom purrr flatten_chr
#' @keywords internal
make_framework_pkgs_chr <- function (gh_repo_1L_chr = "ready4-dev/ready4", gh_tag_1L_chr = "Documentation_0.0") 
{
    framework_pkgs_chr <- get_libraries_ls(gh_repo_1L_chr = gh_repo_1L_chr, 
        gh_tag_1L_chr = gh_tag_1L_chr) %>% update_libraries_ls(keep_chr = "Framework") %>% 
        purrr::flatten_chr()
    return(framework_pkgs_chr)
}
#' Make libraries list
#' @description make_libraries_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make libraries list. The function returns Libraries (a list).
#' @param additions_tb Additions (a tibble), Default: make_additions_tb()
#' @param libraries_tb Libraries (a tibble), Default: NULL
#' @param ns_var_nm_1L_chr Namespace variable name (a character vector of length one), Default: 'pt_ns_chr'
#' @return Libraries (a list)
#' @rdname make_libraries_ls
#' @export 
#' @importFrom purrr map
#' @importFrom stats setNames
#' @keywords internal
make_libraries_ls <- function (additions_tb = make_additions_tb(), libraries_tb = NULL, 
    ns_var_nm_1L_chr = "pt_ns_chr") 
{
    if (is.null(libraries_tb)) {
        libraries_ls <- NULL
    }
    else {
        names_chr <- libraries_tb$Section %>% unique()
        libraries_ls <- names_chr %>% purrr::map(~get_from_lup_obj(libraries_tb, 
            match_var_nm_1L_chr = "Section", match_value_xx = .x, 
            target_var_nm_1L_chr = ns_var_nm_1L_chr)) %>% stats::setNames(names_chr)
    }
    libraries_ls <- update_libraries_ls(libraries_ls, additions_tb)
    return(libraries_ls)
}
#' Make libraries tibble
#' @description make_libraries_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make libraries tibble. The function returns Libraries (a tibble).
#' @param additions_tb Additions (a tibble), Default: make_additions_tb()
#' @param include_1L_chr Include (a character vector of length one), Default: 'modules'
#' @param module_pkgs_chr Module packages (a character vector), Default: character(0)
#' @param ns_var_nm_1L_chr Namespace variable name (a character vector of length one), Default: 'pt_ns_chr'
#' @param reference_var_nm_1L_chr Reference variable name (a character vector of length one), Default: 'Reference'
#' @param url_stub_1L_chr Url stub (a character vector of length one), Default: 'https://ready4-dev.github.io/'
#' @param vignette_var_nm_1L_chr Vignette variable name (a character vector of length one), Default: 'Vignettes'
#' @param vignette_url_var_nm_1L_chr Vignette url variable name (a character vector of length one), Default: 'Vignettes_URLs'
#' @param what_chr What (a character vector), Default: 'all'
#' @return Libraries (a tibble)
#' @rdname make_libraries_tb
#' @export 
#' @importFrom purrr flatten_chr map_chr map2 map_dfr discard map_dfc pluck
#' @importFrom tibble tibble
#' @importFrom rlang sym
#' @importFrom dplyr mutate arrange desc pull rename left_join filter
#' @importFrom kableExtra cell_spec
#' @importFrom rvest read_html html_elements html_text2
#' @importFrom stringr str_match
#' @importFrom stats setNames
#' @keywords internal
make_libraries_tb <- function (additions_tb = make_additions_tb(), include_1L_chr = "modules", 
    module_pkgs_chr = character(0), ns_var_nm_1L_chr = "pt_ns_chr", 
    reference_var_nm_1L_chr = "Reference", url_stub_1L_chr = "https://ready4-dev.github.io/", 
    vignette_var_nm_1L_chr = "Vignettes", vignette_url_var_nm_1L_chr = "Vignettes_URLs", 
    what_chr = "all") 
{
    if (identical(additions_tb, make_additions_tb())) {
        additions_tb <- make_additions_tb("Framework", "ready4", 
            c("Foundation"))
        include_1L_chr <- "framework"
        empty_1L_lgl <- T
    }
    else {
        empty_1L_lgl <- F
    }
    if (include_1L_chr %in% c("framework", "Framework") && !"Framework" %in% 
        additions_tb$category_chr) {
        stop("No framework library included in additions_tb")
    }
    libraries_ls <- NULL
    libraries_ls <- update_libraries_ls(libraries_ls, additions_tb)
    if (is.null(libraries_ls$Framework)) {
        fw_chr <- character(0)
    }
    else {
        fw_chr <- libraries_ls$Framework
    }
    if (identical(module_pkgs_chr, character(0))) 
        module_pkgs_chr <- setdiff(libraries_ls %>% purrr::flatten_chr(), 
            fw_chr) %>% sort()
    if (include_1L_chr %in% c("modules", "Modules")) {
        libraries_chr <- module_pkgs_chr
    }
    else {
        if (include_1L_chr %in% c("framework", "Framework")) {
            libraries_chr <- fw_chr
        }
        if (include_1L_chr %in% c("all", "All")) 
            libraries_chr <- c(fw_chr, module_pkgs_chr)
    }
    libraries_tb <- tibble::tibble(`:=`(!!rlang::sym(ns_var_nm_1L_chr), 
        libraries_chr)) %>% dplyr::mutate(Type = !!rlang::sym(ns_var_nm_1L_chr) %>% 
        purrr::map_chr(~get_from_lup_obj(additions_tb, match_var_nm_1L_chr = "library_chr", 
            match_value_xx = .x, target_var_nm_1L_chr = "type_chr")), 
        Section = !!rlang::sym(ns_var_nm_1L_chr) %>% purrr::map_chr(~get_from_lup_obj(additions_tb, 
            match_var_nm_1L_chr = "library_chr", match_value_xx = .x, 
            target_var_nm_1L_chr = "category_chr")))
    if (include_1L_chr %in% c("framework", "Framework")) {
        libraries_tb <- libraries_tb %>% dplyr::arrange(dplyr::desc(.data$Type), 
            !!rlang::sym(ns_var_nm_1L_chr))
    }
    else {
        libraries_tb <- libraries_tb %>% dplyr::arrange(.data$Section, 
            .data$Type, !!rlang::sym(ns_var_nm_1L_chr))
    }
    libraries_tb <- libraries_tb %>% dplyr::mutate(Link = purrr::map_chr(!!rlang::sym(ns_var_nm_1L_chr), 
        ~paste0(url_stub_1L_chr, .x, "/index", ".html"))) %>% 
        dplyr::mutate(Library = kableExtra::cell_spec(!!rlang::sym(ns_var_nm_1L_chr), 
            "html", link = .data$Link))
    libraries_tb <- add_vignette_links(libraries_tb, ns_var_nm_1L_chr = ns_var_nm_1L_chr, 
        reference_var_nm_1L_chr = reference_var_nm_1L_chr, url_stub_1L_chr = url_stub_1L_chr, 
        vignette_var_nm_1L_chr = vignette_var_nm_1L_chr, vignette_url_var_nm_1L_chr = vignette_url_var_nm_1L_chr)
    libraries_tb <- libraries_tb %>% dplyr::mutate(Citation = paste0(url_stub_1L_chr, 
        !!rlang::sym(ns_var_nm_1L_chr), "/authors.html")) %>% 
        dplyr::mutate(manual_urls_ls = purrr::map2(!!rlang::sym(ns_var_nm_1L_chr), 
            .data$Link, ~get_manual_urls(.x, pkg_url_1L_chr = .y))) %>% 
        dplyr::mutate(code_urls_ls = purrr::map2(!!rlang::sym(ns_var_nm_1L_chr), 
            .data$Link, ~get_source_code_urls(.x, pkg_url_1L_chr = .y)))
    y_tb <- purrr::map_dfr(libraries_tb$Citation, ~{
        scraped_1L_chr <- rvest::read_html(.x) %>% rvest::html_elements("pre") %>% 
            rvest::html_text2()
        details_chr <- strsplit(scraped_1L_chr, split = "\n") %>% 
            purrr::flatten_chr() %>% purrr::map_chr(~{
            value_1L_chr <- trimws(.x)
            ifelse(startsWith(value_1L_chr, "title = ") | startsWith(value_1L_chr, 
                "author = ") | startsWith(value_1L_chr, "doi = "), 
                value_1L_chr, NA_character_)
        }) %>% purrr::discard(is.na)
        col_names_chr <- c("AUTHOR", "TITLE", "DOI")
        col_names_chr %>% purrr::map_dfc(~details_chr[which(startsWith(details_chr, 
            tolower(.x)))] %>% stringr::str_match("\\{(.*?)\\}") %>% 
            purrr::pluck(2)) %>% stats::setNames(col_names_chr)
    }) %>% dplyr::mutate(`:=`(!!rlang::sym(ns_var_nm_1L_chr), 
        libraries_tb %>% dplyr::pull(!!rlang::sym(ns_var_nm_1L_chr)))) %>% 
        dplyr::rename(DOI_chr = .data$DOI, Title = .data$TITLE, 
            Authors = .data$AUTHOR)
    libraries_tb <- dplyr::left_join(libraries_tb, y_tb, by = ns_var_nm_1L_chr)
    if (empty_1L_lgl) {
        libraries_tb <- libraries_tb %>% dplyr::filter(F)
    }
    return(libraries_tb)
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
#' Make a tabular summary of methods associated with ready model modules
#' @description make_ds_releases_tbl() scrapes metadata from Dataverse datasets for which a valid Digital Object Identifier (DOI) has been supplied to create a table summarising the entire release history of these datasets.
#' @param packages_tb Packages (a tibble), Default: NULL
#' @param exclude_mthds_for_chr Exclude methods for (a character vector), Default: 'NA'
#' @param framework_only_1L_lgl Framework only (a logical vector of length one), Default: T
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @param module_pkgs_chr Module packages (a character vector), Default: character(0)
#' @param ns_var_nm_1L_chr Namespace variable name (a character vector of length one), Default: 'pt_ns_chr'
#' @param path_1L_chr Path (a character vector of length one), Default: character(0)
#' @param return_1L_chr Return (a character vector of length one), Default: 'all'
#' @return Methods (a tibble)
#' @rdname make_methods_tb
#' @export 
#' @importFrom dplyr filter mutate
#' @importFrom rlang sym
#' @importFrom tibble tibble
#' @importFrom purrr map flatten_chr discard
#' @examplesIf interactive()
#'   # Likely to take more than one minute to execute.
#'   make_methods_tb(gh_repo_1L_chr = "ready4-dev/ready4")
make_methods_tb <- function (packages_tb = NULL, exclude_mthds_for_chr = NA_character_, 
    framework_only_1L_lgl = T, gh_repo_1L_chr = "ready4-dev/ready4", 
    gh_tag_1L_chr = "Documentation_0.0", module_pkgs_chr = character(0), 
    ns_var_nm_1L_chr = "pt_ns_chr", path_1L_chr = character(0), 
    return_1L_chr = "all") 
{
    if (is.null(packages_tb)) {
        packages_tb <- get_libraries_tb(gh_repo_1L_chr = gh_repo_1L_chr, 
            gh_tag_1L_chr = gh_tag_1L_chr)
    }
    if (!identical(module_pkgs_chr, character(0))) {
        packages_tb <- dplyr::filter(packages_tb, !!rlang::sym(ns_var_nm_1L_chr) %in% 
            module_pkgs_chr | .data$Section == "Framework")
    }
    methods_tb <- tibble::tibble(Method = get_generics(exclude_mthds_for_chr = exclude_mthds_for_chr, 
        framework_only_1L_lgl = framework_only_1L_lgl, return_1L_chr = return_1L_chr)) %>% 
        dplyr::mutate(Purpose = .data$Method %>% get_mthd_titles(path_1L_chr = path_1L_chr), 
            Examples = .data$Method %>% purrr::map(~get_examples(packages_tb$Vignettes_URLs %>% 
                purrr::flatten_chr() %>% unique() %>% purrr::discard(is.na), 
                term_1L_chr = .x)))
    return(methods_tb)
}
#' Make modules packages character vector
#' @description make_modules_pkgs_chr() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make modules packages character vector. The function returns Modules packages (a character vector).
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @param sort_1L_lgl Sort (a logical vector of length one), Default: F
#' @param what_chr What (a character vector), Default: 'all'
#' @return Modules packages (a character vector)
#' @rdname make_modules_pkgs_chr
#' @export 
#' @importFrom purrr flatten_chr
#' @keywords internal
make_modules_pkgs_chr <- function (gh_repo_1L_chr = "ready4-dev/ready4", gh_tag_1L_chr = "Documentation_0.0", 
    sort_1L_lgl = F, what_chr = "all") 
{
    libraries_ls <- get_libraries_ls(gh_repo_1L_chr = gh_repo_1L_chr, 
        gh_tag_1L_chr = gh_tag_1L_chr) %>% update_libraries_ls(keep_chr = what_chr)
    modules_pkgs_chr <- c(character(0), setdiff(libraries_ls %>% 
        purrr::flatten_chr(), libraries_ls$Framework))
    if (sort_1L_lgl) {
        modules_pkgs_chr <- modules_pkgs_chr %>% sort()
    }
    return(modules_pkgs_chr)
}
#' Make a tabular summary of ready4 model modules and sub-modules
#' @description make_methods_tb() scrapes the documentation websites of all libraries of ready4 modules in a specified GitHub organisation and then creates a tabular summary of vignette examples of ready4 module methods.
#' @param pkg_extensions_tb Package extensions (a tibble), Default: NULL
#' @param cls_extensions_tb Class extensions (a tibble), Default: NULL
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @param module_pkgs_chr Module packages (a character vector), Default: character(0)
#' @param include_1L_chr Include (a character vector of length one), Default: 'modules'
#' @param ns_var_nm_1L_chr Namespace variable name (a character vector of length one), Default: 'pt_ns_chr'
#' @param url_stub_1L_chr Url stub (a character vector of length one), Default: 'https://ready4-dev.github.io/'
#' @param what_chr What (a character vector), Default: 'all'
#' @return Modules (a tibble)
#' @rdname make_modules_tb
#' @export 
#' @importFrom dplyr filter inner_join arrange mutate case_when select
#' @importFrom rlang sym
#' @importFrom purrr map flatten_int flatten_chr discard map_int pluck map2_chr pmap map2
#' @importFrom stringr str_which regex str_locate str_sub str_remove_all str_match
#' @importFrom kableExtra cell_spec
#' @importFrom rvest read_html html_elements html_text2
#' @importFrom stringi stri_replace_last_regex
#' @examplesIf interactive()
#'   # Likely to take more than one minute to execute.
#'   make_modules_tb(gh_repo_1L_chr = "ready4-dev/ready4")
make_modules_tb <- function (pkg_extensions_tb = NULL, cls_extensions_tb = NULL, 
    gh_repo_1L_chr = "ready4-dev/ready4", gh_tag_1L_chr = "Documentation_0.0", 
    module_pkgs_chr = character(0), include_1L_chr = "modules", 
    ns_var_nm_1L_chr = "pt_ns_chr", url_stub_1L_chr = "https://ready4-dev.github.io/", 
    what_chr = "all") 
{
    if (is.null(pkg_extensions_tb)) {
        pkg_extensions_tb <- get_libraries_tb(gh_repo_1L_chr = gh_repo_1L_chr, 
            gh_tag_1L_chr = gh_tag_1L_chr)
    }
    if (include_1L_chr %in% c("framework", "Framework")) {
        pkg_extensions_tb <- dplyr::filter(pkg_extensions_tb, 
            .data$Section == "Framework")
    }
    if (include_1L_chr %in% c("modules", "Modules")) {
        pkg_extensions_tb <- dplyr::filter(pkg_extensions_tb, 
            .data$Section != "Framework")
    }
    if (!identical(module_pkgs_chr, character(0))) {
        pkg_extensions_tb <- dplyr::filter(pkg_extensions_tb, 
            !!rlang::sym(ns_var_nm_1L_chr) %in% module_pkgs_chr)
    }
    if (!what_chr %in% c("all", "All")) {
        libraries_ls <- make_libraries_ls(libraries_tb = pkg_extensions_tb, 
            ns_var_nm_1L_chr = ns_var_nm_1L_chr)
        include_int <- what_chr %>% purrr::map(~stringr::str_which(names(libraries_ls), 
            stringr::regex(.x, ignore_case = TRUE))) %>% purrr::flatten_int()
        if (identical(include_int, integer(0))) {
            pkg_extensions_tb <- dplyr::filter(pkg_extensions_tb, 
                F)
        }
        else {
            pkg_extensions_tb <- dplyr::filter(pkg_extensions_tb, 
                !!rlang::sym(ns_var_nm_1L_chr) %in% purrr::flatten_chr(libraries_ls[include_int]))
        }
    }
    if (is.null(cls_extensions_tb)) 
        cls_extensions_tb <- get_cls_extensions(pkg_extensions_tb, 
            gh_repo_1L_chr = gh_repo_1L_chr, gh_tag_1L_chr = gh_tag_1L_chr, 
            url_stub_1L_chr = url_stub_1L_chr, validate_1L_lgl = T)
    modules_tb <- dplyr::inner_join(cls_extensions_tb, pkg_extensions_tb, 
        by = ns_var_nm_1L_chr) %>% dplyr::arrange(.data$type_chr, 
        .data$old_class_lgl)
    order_int <- modules_tb$Reference %>% purrr::flatten_int() %>% 
        unique() %>% purrr::discard(is.na)
    modules_tb <- modules_tb %>% dplyr::mutate(Reference = dplyr::case_when(!is.na(Reference) ~ 
        purrr::map(Reference, ~{
            new_int <- which(.x == order_int)
            if (length(new_int) == 1) 
                new_int <- rep(new_int, 2)
            new_int
        }), T ~ Reference))
    order_int <- modules_tb$Vignettes_URLs %>% purrr::map(~{
        if (is.na(.x[[1]])) {
            NA_integer_
        }
        else {
            .x %>% purrr::map_int(~gsub(".*style=\"     \" >(.+)</a>.*", 
                "\\1", .x) %>% as.numeric())
        }
    }) %>% purrr::flatten_int() %>% purrr::discard(is.na) %>% 
        unique()
    modules_tb <- modules_tb %>% dplyr::mutate(Vignettes_URLs = dplyr::case_when(!is.na(Reference) ~ 
        purrr::map(.data$Vignettes_URLs, ~{
            if (is.na(.x[1])) {
                new_chr <- NA_character_
            }
            else {
                old_int <- .x %>% purrr::map_int(~{
                  start_1L_int <- 1 + (.x %>% stringr::str_locate("\"     \" >") %>% 
                    purrr::pluck(2))
                  stringr::str_sub(.x, start = start_1L_int) %>% 
                    stringr::str_remove_all("</a>") %>% as.numeric()
                })
                new_chr <- .x %>% purrr::map2_chr(old_int, ~{
                  end_1L_int <- (.x %>% stringr::str_locate("\"     \" >") %>% 
                    purrr::pluck(2))
                  paste0(stringr::str_sub(.x, end = end_1L_int), 
                    which(.y == order_int), "</a>")
                })
            }
            new_chr
        }), T ~ .data$Vignettes_URLs))
    modules_tb <- modules_tb %>% dplyr::mutate(Class = purrr::pmap(list(!!rlang::sym(ns_var_nm_1L_chr), 
        .data$type_chr, .data$old_class_lgl), ~{
        kableExtra::cell_spec(..2, "html", link = paste0(url_stub_1L_chr, 
            ..1, "/reference/", ifelse(..3, ..2, paste0(..2, 
                "-class")), ".html"))
    })) %>% dplyr::mutate(Examples = purrr::map2(.data$Vignettes_URLs, 
        .data$type_chr, ~get_examples(.x, term_1L_chr = .y)))
    modules_tb <- modules_tb %>% dplyr::mutate(Description = purrr::map2_chr(.data$Class, 
        .data$old_class_lgl, ~{
            rvest::read_html((.x %>% stringr::str_match("href=\"\\s*(.*?)\\s*\" style"))[, 
                2]) %>% rvest::html_elements(ifelse(.y, "h1", 
                "p")) %>% rvest::html_text2() %>% purrr::pluck(1)
        }) %>% stringi::stri_replace_last_regex("\\.", "")) %>% 
        dplyr::select("Class", "Description", "Library", "Examples", 
            "old_class_lgl")
    return(modules_tb)
}
#' Make a tabular summary of programs using ready4 model modules
#' @description make_modules_tb() scrapes the documentation websites of all libraries of ready4 modules in a specified GitHub organisation and then creates a tabular summary of the modules included in those libraries and vignette examples of their use.
#' @param what_1L_chr What (a character vector of length one), Default: c("Program", "Subroutine", "Program_and_Subroutine")
#' @param as_kbl_1L_lgl As kable (a logical vector of length one), Default: F
#' @param exclude_chr Exclude (a character vector), Default: character(0)
#' @param format_1L_chr Format (a character vector of length one), Default: '%d-%b-%Y'
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @param tidy_desc_1L_lgl Tidy description (a logical vector of length one), Default: T
#' @param url_stub_1L_chr Url stub (a character vector of length one), Default: 'https://ready4-dev.github.io/'
#' @param zenodo_1L_chr Zenodo (a character vector of length one), Default: 'ready4'
#' @param ... Additional arguments
#' @return Programs (an output object of multiple potential types)
#' @rdname make_programs_tbl
#' @export 
#' @importFrom dplyr group_by filter row_number arrange ungroup pull mutate select
#' @importFrom rlang sym
#' @seealso [zen4R::ZenodoManager()]
#' @importFrom purrr pluck map map_lgl map2_int map_chr map2_chr pmap
#' @importFrom stringr str_remove_all str_remove str_equal str_detect
#' @importFrom kableExtra cell_spec kable kable_styling
#' @examplesIf interactive()
#'   # Likely to take more than one minute to execute.
#'   if(requireNamespace("zen4R", quietly = TRUE)) {
#'     make_programs_tbl("Program",
#'                       gh_repo_1L_chr = "ready4-dev/ready4")
#'     make_programs_tbl("Subroutine",
#'                       gh_repo_1L_chr = "ready4-dev/ready4")
#'   }
make_programs_tbl <- function (what_1L_chr = c("Program", "Subroutine", "Program_and_Subroutine"), 
    as_kbl_1L_lgl = F, exclude_chr = character(0), format_1L_chr = "%d-%b-%Y", 
    gh_repo_1L_chr = "ready4-dev/ready4", gh_tag_1L_chr = "Documentation_0.0", 
    tidy_desc_1L_lgl = T, url_stub_1L_chr = "https://ready4-dev.github.io/", 
    zenodo_1L_chr = "ready4", ...) 
{
    if (!requireNamespace("zen4R", quietly = TRUE)) {
        stop("zen4R package is required - please install it and rerun the last command.")
    }
    what_1L_chr <- match.arg(what_1L_chr)
    programs_xx <- make_code_releases_tbl(what_1L_chr, as_kbl_1L_lgl = F, 
        exclude_chr = exclude_chr, gh_repo_1L_chr = gh_repo_1L_chr, 
        gh_tag_1L_chr = gh_tag_1L_chr, tidy_desc_1L_lgl = F, 
        url_stub_1L_chr = url_stub_1L_chr) %>% dplyr::group_by(!!rlang::sym(what_1L_chr)) %>% 
        dplyr::filter(dplyr::row_number() == 1) %>% dplyr::arrange(!!rlang::sym(what_1L_chr)) %>% 
        dplyr::ungroup()
    zenodo_xx <- zen4R::ZenodoManager$new()
    community_ls <- zenodo_xx$getCommunityById(zenodo_1L_chr)
    records_chr <- readLines(url(community_ls$links$records))
    records_chr <- records_chr %>% strsplit("\\{\"created\": ") %>% 
        purrr::pluck(1)
    records_chr <- records_chr[2:length(records_chr)]
    records_ls <- records_chr %>% purrr::map(~{
        individual_chr <- .x %>% strsplit(",") %>% purrr::pluck(1)
        individual_chr[individual_chr %>% purrr::map_lgl(~startsWith(.x, 
            " \"doi_url\"") | startsWith(.x, " \"metadata\"") | 
            startsWith(.x, " \"description\""))]
    })
    indices_int <- programs_xx$Description %>% purrr::map2_int(programs_xx %>% 
        dplyr::pull(1), ~{
        description_1L_chr <- .x
        title_1L_chr <- .y
        index_1L_int <- which(records_ls %>% purrr::map_lgl(~{
            any(.x %>% purrr::map_lgl(~{
                modified_1L_chr <- gsub("<.*?>", "", .x) %>% 
                  stringr::str_remove_all("\\\\n") %>% stringr::str_remove(" \"description\": \"") %>% 
                  stringr::str_remove("\"")
                stringr::str_equal(modified_1L_chr, description_1L_chr) | 
                  stringr::str_detect(modified_1L_chr, description_1L_chr)
            })) | .x[2] %>% stringr::str_remove(" \"metadata\": \\{\"title\": \"") %>% 
                startsWith(paste0(title_1L_chr, ":"))
        }))
        ifelse(identical(index_1L_int, integer(0)), NA_integer_, 
            index_1L_int)
    })
    programs_xx$DOI <- indices_int %>% purrr::map_chr(~records_ls[[.x]][1] %>% 
        strsplit("\"") %>% purrr::pluck(1) %>% purrr::pluck(4))
    programs_xx$GitHub <- gsub("/releases/.*", "", programs_xx$URL)
    if (tidy_desc_1L_lgl) 
        programs_xx <- programs_xx %>% dplyr::mutate(Description = .data$Description %>% 
            purrr::map2_chr(!!rlang::sym(what_1L_chr), ~stringr::str_remove(.x, 
                paste0(.y, ": "))))
    if (as_kbl_1L_lgl) {
        programs_xx <- programs_xx %>% dplyr::mutate(Release = .data$Release %>% 
            stringr::str_remove_all("Release ") %>% stringr::str_remove_all("v"), 
            Date = .data$Date %>% format.Date(format_1L_chr) %>% 
                as.character()) %>% dplyr::mutate(Source = purrr::pmap(list(.data$GitHub, 
            .data$DOI), ~{
            kableExtra::cell_spec(c("Dev", "Archive"), format = "html", 
                link = c(..1, ..2))
        })) %>% dplyr::select(!!rlang::sym(what_1L_chr), "Release", 
            "Date", "Description", "Source")
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
