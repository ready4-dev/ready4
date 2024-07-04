#' Print a table of ready4 model data collections
#' @description print_data() formats the output of either get_datasts_tb() or make_datasts_tb() as HTML. The type of output can be customised to display Dataverse data collections or Dataverse datasets. Similarly output can be restricted to real or toy datasets.
#' @param datasets_tb Datasets (a tibble)
#' @param by_dv_1L_lgl By dataverse (a logical vector of length one), Default: FALSE
#' @param filter_cdns_ls Filter conditions (a list), Default: NULL
#' @param root_1L_chr Root (a character vector of length one), Default: 'https://dataverse.harvard.edu/dataverse/'
#' @param scroll_height_1L_chr Scroll height (a character vector of length one), Default: character(0)
#' @param scroll_width_1L_chr Scroll width (a character vector of length one), Default: character(0)
#' @param toy_data_dv_1L_chr Toy data dataverse (a character vector of length one), Default: 'fakes'
#' @param what_1L_chr What (a character vector of length one), Default: 'all'
#' @param ... Additional arguments
#' @return Datasets (a kable)
#' @rdname print_data
#' @export
#' @example man/examples/print_data.R
print_data <- function (datasets_tb, by_dv_1L_lgl = FALSE, filter_cdns_ls = NULL,
    root_1L_chr = "https://dataverse.harvard.edu/dataverse/",
    scroll_height_1L_chr = character(0), scroll_width_1L_chr = character(0),
    toy_data_dv_1L_chr = "fakes", what_1L_chr = "all", ...)
{
    if (is.null(datasets_tb)) {
        message("datasets_tb is NULL")
        datasets_kbl <- NULL
    }
    else {
        if (by_dv_1L_lgl & ("Datasets_Meta" %in% names(datasets_tb))) {
            datasets_kbl <- print_dvs(datasets_tb, root_1L_chr = root_1L_chr,
                scroll_height_1L_chr = scroll_height_1L_chr,
                scroll_width_1L_chr = scroll_width_1L_chr, toy_data_dv_1L_chr = toy_data_dv_1L_chr,
                what_1L_chr = what_1L_chr, ...)
        }
        else {
            datasets_kbl <- print_dss(datasets_tb, filter_cdns_ls = filter_cdns_ls,
                scroll_height_1L_chr = scroll_height_1L_chr,
                scroll_width_1L_chr = scroll_width_1L_chr, toy_data_dv_1L_chr = toy_data_dv_1L_chr,
                what_1L_chr = what_1L_chr, ...)
        }
    }
    return(datasets_kbl)
}
#' Print datasets
#' @description print_dss() is a Print function that prints output to console. Specifically, this function implements an algorithm to print datasets. The function is called for its side effects and does not return a value.
#' @param datasets_tb Datasets (a tibble)
#' @param filter_cdns_ls Filter conditions (a list), Default: NULL
#' @param scroll_height_1L_chr Scroll height (a character vector of length one), Default: character(0)
#' @param scroll_width_1L_chr Scroll width (a character vector of length one), Default: character(0)
#' @param toy_data_dv_1L_chr Toy data dataverse (a character vector of length one), Default: 'fakes'
#' @param what_1L_chr What (a character vector of length one), Default: 'all'
#' @param ... Additional arguments
#' @return Datasets (a kable)
#' @rdname print_dss
#' @export
#' @importFrom kableExtra kable kable_styling
#' @keywords internal
print_dss <- function (datasets_tb, filter_cdns_ls = NULL, scroll_height_1L_chr = character(0),
    scroll_width_1L_chr = character(0), toy_data_dv_1L_chr = "fakes",
    what_1L_chr = "all", ...)
{
    if (is.null(datasets_tb)) {
        message("datasets_tb is NULL")
        dss_kbl <- NULL
    }
    else {
        datasets_tb <- make_dss_tb(datasets_tb, filter_cdns_ls = filter_cdns_ls,
            toy_data_dv_1L_chr = toy_data_dv_1L_chr, what_1L_chr = what_1L_chr)
        dss_kbl <- datasets_tb %>% kableExtra::kable("html",
            escape = FALSE) %>% kableExtra::kable_styling(bootstrap_options = c("hover",
            "condensed")) %>% add_scroll_box(scroll_height_1L_chr = scroll_height_1L_chr,
            scroll_width_1L_chr = scroll_width_1L_chr, ...)
    }
    return(dss_kbl)
}
#' Print dataverses
#' @description print_dvs() is a Print function that prints output to console. Specifically, this function implements an algorithm to print dataverses. The function is called for its side effects and does not return a value.
#' @param dvs_tb Dataverses (a tibble)
#' @param filter_cdns_ls Filter conditions (a list), Default: NULL
#' @param root_1L_chr Root (a character vector of length one), Default: 'https://dataverse.harvard.edu/dataverse/'
#' @param scroll_height_1L_chr Scroll height (a character vector of length one), Default: character(0)
#' @param scroll_width_1L_chr Scroll width (a character vector of length one), Default: character(0)
#' @param toy_data_dv_1L_chr Toy data dataverse (a character vector of length one), Default: 'fakes'
#' @param what_1L_chr What (a character vector of length one), Default: 'all'
#' @param ... Additional arguments
#' @return Dataverses (a kable)
#' @rdname print_dvs
#' @export
#' @importFrom dplyr select mutate filter
#' @importFrom purrr map
#' @importFrom kableExtra kable kable_styling column_spec
#' @keywords internal
print_dvs <- function (dvs_tb, filter_cdns_ls = NULL, root_1L_chr = "https://dataverse.harvard.edu/dataverse/",
    scroll_height_1L_chr = character(0), scroll_width_1L_chr = character(0),
    toy_data_dv_1L_chr = "fakes", what_1L_chr = "all", ...)
{
    if (is.null(dvs_tb)) {
        message("dvs_tb is NULL")
        dvs_kbl <- NULL
    }
    else {
        dvs_tb <- add_references(dvs_tb, data_var_nm_1L_chr = "Contents",
            data_url_var_nm_1L_chr = "Datasets") %>% dplyr::select("Dataverse",
            "Name", "Description", "Creator", "Datasets") %>%
            dplyr::mutate(Datasets = .data$Datasets %>% purrr::map(~if (identical(.x,
                NA_character_)) {
                ""
            }
            else {
                .x
            }))
        if (what_1L_chr == "real")
            dvs_tb <- dvs_tb %>% dplyr::filter(.data$Dataverse !=
                toy_data_dv_1L_chr)
        if (what_1L_chr == "fakes")
            dvs_tb <- dvs_tb %>% dplyr::filter(.data$Dataverse ==
                toy_data_dv_1L_chr)
        dvs_kbl <- dvs_tb %>% kableExtra::kable("html", escape = FALSE) %>%
            kableExtra::kable_styling(bootstrap_options = c("hover",
                "condensed")) %>% kableExtra::column_spec(which(names(dvs_tb) ==
            "Dataverse"), link = paste0(root_1L_chr, dvs_tb$Dataverse)) %>%
            add_scroll_box(scroll_height_1L_chr = scroll_height_1L_chr,
                scroll_width_1L_chr = scroll_width_1L_chr, ...)
    }
    return(dvs_kbl)
}
#' Print a table of methods associated with ready4 model modules
#' @description print_methods() formats the output of either get_methods_tb() or make_methods_tb() as HTML.
#' @param methods_tb Methods (a tibble), Default: NULL
#' @param exclude_mthds_for_chr Exclude methods for (a character vector), Default: 'NA'
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @param methods_chr Methods (a character vector), Default: NULL
#' @param module_pkgs_chr Module packages (a character vector), Default: character(0)
#' @param ns_var_nm_1L_chr Namespace variable name (a character vector of length one), Default: 'pt_ns_chr'
#' @param path_1L_chr Path (a character vector of length one), Default: character(0)
#' @param packages_tb Packages (a tibble), Default: NULL
#' @param return_1L_chr Return (a character vector of length one), Default: 'all'
#' @param scroll_height_1L_chr Scroll height (a character vector of length one), Default: character(0)
#' @param scroll_width_1L_chr Scroll width (a character vector of length one), Default: character(0)
#' @param ... Additional arguments
#' @return Methods (a kable)
#' @rdname print_methods
#' @export
#' @importFrom dplyr filter
#' @importFrom purrr map_chr
#' @importFrom kableExtra kable kable_styling column_spec
#' @example man/examples/print_methods.R
print_methods <- function (methods_tb = NULL, exclude_mthds_for_chr = NA_character_,
    gh_repo_1L_chr = "ready4-dev/ready4", gh_tag_1L_chr = "Documentation_0.0",
    methods_chr = NULL, module_pkgs_chr = character(0), ns_var_nm_1L_chr = "pt_ns_chr",
    path_1L_chr = character(0), packages_tb = NULL, return_1L_chr = "all",
    scroll_height_1L_chr = character(0), scroll_width_1L_chr = character(0),
    ...)
{
    if (is.null(methods_tb))
        methods_tb <- make_methods_tb(exclude_mthds_for_chr = exclude_mthds_for_chr,
            gh_repo_1L_chr = gh_repo_1L_chr, gh_tag_1L_chr = gh_tag_1L_chr,
            module_pkgs_chr = module_pkgs_chr, ns_var_nm_1L_chr = ns_var_nm_1L_chr,
            packages_tb = packages_tb, path_1L_chr = path_1L_chr,
            return_1L_chr = return_1L_chr)
    if (is.null(methods_chr))
        methods_chr <- get_generics(exclude_mthds_for_chr = exclude_mthds_for_chr,
            return_1L_chr = return_1L_chr)
    if (is.null(methods_tb)) {
        message("methods_tb is NULL")
        methods_kbl <- NULL
    }
    else {
        methods_tb <- methods_tb %>% dplyr::filter(.data$Method %in%
            methods_chr)
        links_chr <- methods_tb$Method %>% purrr::map_chr(~paste0("https://ready4-dev.github.io/ready4/reference/",
            .x, "-methods.html"))
        methods_kbl <- methods_tb %>% kableExtra::kable("html",
            escape = FALSE) %>% kableExtra::kable_styling(bootstrap_options = c("hover",
            "condensed")) %>% kableExtra::column_spec(which(names(methods_tb) ==
            "Method"), link = links_chr) %>% add_scroll_box(scroll_height_1L_chr = scroll_height_1L_chr,
            scroll_width_1L_chr = scroll_width_1L_chr, ...)
    }
    return(methods_kbl)
}
#' Print a table of ready4 model modules
#' @description print_modules() formats the output of either get_modules_tb() or make_modules_tb() as HTML.
#' @param modules_tb Modules (a tibble)
#' @param scroll_height_1L_chr Scroll height (a character vector of length one), Default: character(0)
#' @param scroll_width_1L_chr Scroll width (a character vector of length one), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'All'
#' @param ... Additional arguments
#' @return Modules (a kable)
#' @rdname print_modules
#' @export
#' @importFrom dplyr filter select
#' @importFrom kableExtra kable kable_styling
#' @example man/examples/print_modules.R
print_modules <- function (modules_tb, scroll_height_1L_chr = character(0), scroll_width_1L_chr = character(0),
    what_1L_chr = "All", ...)
{
    if (is.null(modules_tb)) {
        message("modules_tb is NULL")
        modules_kbl <- NULL
    }
    else {
        if (what_1L_chr == "S4") {
            modules_tb <- modules_tb %>% dplyr::filter(!.data$old_class_lgl)
        }
        if (what_1L_chr == "S3") {
            modules_tb <- modules_tb %>% dplyr::filter(.data$old_class_lgl)
        }
        modules_kbl <- modules_tb %>% dplyr::select(-"old_class_lgl",
            -"Library") %>% kableExtra::kable("html", escape = FALSE) %>%
            kableExtra::kable_styling(bootstrap_options = c("hover",
                "condensed")) %>% add_scroll_box(scroll_height_1L_chr = scroll_height_1L_chr,
            scroll_width_1L_chr = scroll_width_1L_chr, ...)
    }
    return(modules_kbl)
}
#' Print a table of ready4 libraries
#' @description print_packages() formats the output of get_libraries_tb() as HTML.
#' @param pkg_extensions_tb Package extensions (a tibble), Default: NULL
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @param include_1L_chr Include (a character vector of length one), Default: 'modules'
#' @param module_pkgs_chr Module packages (a character vector), Default: character(0)
#' @param ns_var_nm_1L_chr Namespace variable name (a character vector of length one), Default: 'pt_ns_chr'
#' @param project_badges_url_1L_chr Project badges url (a character vector of length one), Default: 'https://img.shields.io/badge/ready4'
#' @param reference_var_nm_1L_chr Reference variable name (a character vector of length one), Default: 'Reference'
#' @param scroll_height_1L_chr Scroll height (a character vector of length one), Default: character(0)
#' @param scroll_width_1L_chr Scroll width (a character vector of length one), Default: character(0)
#' @param sections_chr Sections (a character vector), Default: character(0)
#' @param url_stub_1L_chr Url stub (a character vector of length one), Default: 'https://ready4-dev.github.io/'
#' @param vignette_var_nm_1L_chr Vignette variable name (a character vector of length one), Default: 'Vignettes'
#' @param vignette_url_var_nm_1L_chr Vignette url variable name (a character vector of length one), Default: 'Vignettes_URLs'
#' @param what_chr What (a character vector), Default: 'all'
#' @param ... Additional arguments
#' @return Package extensions (a kable)
#' @rdname print_packages
#' @export
#' @importFrom dplyr filter mutate rename select
#' @importFrom purrr map map_chr map2_chr pmap
#' @importFrom stringr str_remove
#' @importFrom kableExtra cell_spec kable kable_styling column_spec spec_image
#' @examplesIf interactive()
#'   # Method 1
#'   libraries_tb <- get_libraries_tb(gh_repo_1L_chr = "ready4-dev/ready4")
#'   ## Print framework libraries
#'   update_libraries_tb(libraries_tb,
#'                       url_stub_1L_chr = "https://ready4-dev.github.io/",
#'                       include_1L_chr = "framework") %>%
#'     print_packages()
#'   ## Print module libraries
#'   update_libraries_tb(libraries_tb,
#'                       url_stub_1L_chr = "https://ready4-dev.github.io/",
#'                       include_1L_chr = "modules") %>%
#'     print_packages()
#'   # Method 2
#'   ## Print framework libraries
#'   print_packages(gh_repo_1L_chr = "ready4-dev/ready4",
#'                  include_1L_chr = "framework")
#'   ## Print module libraries
#'   print_packages(gh_repo_1L_chr = "ready4-dev/ready4",
#'                  include_1L_chr = "modules")
print_packages <- function (pkg_extensions_tb = NULL, gh_repo_1L_chr = "ready4-dev/ready4",
    gh_tag_1L_chr = "Documentation_0.0", include_1L_chr = "modules",
    module_pkgs_chr = character(0), ns_var_nm_1L_chr = "pt_ns_chr",
    project_badges_url_1L_chr = "https://img.shields.io/badge/ready4",
    reference_var_nm_1L_chr = "Reference", scroll_height_1L_chr = character(0),
    scroll_width_1L_chr = character(0), sections_chr = character(0),
    url_stub_1L_chr = "https://ready4-dev.github.io/", vignette_var_nm_1L_chr = "Vignettes",
    vignette_url_var_nm_1L_chr = "Vignettes_URLs", what_chr = "all",
    ...)
{
    if (is.null(pkg_extensions_tb))
        pkg_extensions_tb <- get_libraries_tb(gh_repo_1L_chr = gh_repo_1L_chr,
            gh_tag_1L_chr = gh_tag_1L_chr) %>% update_libraries_tb(include_1L_chr = include_1L_chr,
            module_pkgs_chr = module_pkgs_chr, ns_var_nm_1L_chr = ns_var_nm_1L_chr,
            reference_var_nm_1L_chr = reference_var_nm_1L_chr,
            url_stub_1L_chr = url_stub_1L_chr, vignette_var_nm_1L_chr = vignette_var_nm_1L_chr,
            vignette_url_var_nm_1L_chr = vignette_url_var_nm_1L_chr,
            what_chr = what_chr)
    if (is.null(pkg_extensions_tb)) {
        message("pkg_extensions_tb is NULL")
        pkg_extensions_kbl <- NULL
    }
    else {
        if (!identical(sections_chr, character(0))) {
            pkg_extensions_tb <- pkg_extensions_tb %>% dplyr::filter(.data$Section %in%
                sections_chr)
        }
        if (nrow(pkg_extensions_tb) == 1) {
            pkg_extensions_tb <- rbind(pkg_extensions_tb, pkg_extensions_tb)
            is_single_1L_lgl <- TRUE
        }
        else {
            is_single_1L_lgl <- FALSE
        }
        pkg_extensions_tb <- pkg_extensions_tb %>% dplyr::mutate(Badges = purrr::map(.data$pt_ns_chr,
            ~get_badge_urls(.x, project_badges_url_1L_chr = project_badges_url_1L_chr,
                url_stub_1L_chr = url_stub_1L_chr))) %>% dplyr::mutate(Type = "") %>%
            dplyr::mutate(DOI = "") %>% dplyr::mutate(Logo = "")
        ready4_badges_chr <- purrr::map_chr(pkg_extensions_tb$Badges,
            ~{
                badge_1L_chr <- .x$ready4_1L_chr
                ifelse(identical(badge_1L_chr, character(0)),
                  NA_character_, badge_1L_chr)
            })
        zenodo_badges_chr <- purrr::map_chr(pkg_extensions_tb$Badges,
            ~{
                badge_1L_chr <- .x$zenodo_1L_chr
                ifelse(identical(badge_1L_chr, character(0)),
                  NA_character_, badge_1L_chr)
            })
        logos_chr <- purrr::map_chr(pkg_extensions_tb$pt_ns_chr,
            ~paste0("https://ready4-dev.github.io/", .x, "/logo.png"))
        homepages_chr <- pkg_extensions_tb$Link
        pkg_extensions_tb <- pkg_extensions_tb %>% dplyr::mutate(Purpose = .data$Title %>%
            purrr::map2_chr(.data$pt_ns_chr, ~stringr::str_remove(.x,
                paste0(.y, ": ")))) %>% dplyr::rename(Package = .data$Logo,
            Website = .data$Link, Examples = .data$Vignettes_URLs)
        pkg_extensions_tb <- pkg_extensions_tb %>% dplyr::mutate(Examples = purrr::map(.data$Examples,
            ~if (is.na(.x[1])) {
                ""
            }
            else {
                .x
            })) %>% dplyr::mutate(Documentation = purrr::pmap(list(.data$manual_urls_ls,
            .data$Citation, .data$Website), ~{
            if (identical(..1, character(0)) | is.na(..1[1]) |
                length(..1) != 2) {
                manual_txt_chr <- character(0)
            }
            else {
                manual_txt_chr <- c("Manual - Short (PDF)", "Manual - Full (PDF)")
            }
            kableExtra::cell_spec(c("Citation", "Website", manual_txt_chr),
                "html", link = c(..2, ..3, ..1))
        })) %>% dplyr::mutate(Code = purrr::map(.data$code_urls_ls,
            ~{
                if (is.na(.x[1])) {
                  ""
                }
                else {
                  kableExtra::cell_spec(c("Dev", "Archive"),
                    "html", link = .x)
                }
            })) %>% dplyr::select("Type", "Package", "Purpose",
            "Documentation", "Code", "Examples")
        pkg_extensions_kbl <- pkg_extensions_tb %>% kableExtra::kable("html",
            escape = FALSE) %>% kableExtra::kable_styling(bootstrap_options = c("hover",
            "condensed")) %>% kableExtra::column_spec(which(names(pkg_extensions_tb) ==
            "Type"), image = ready4_badges_chr) %>% kableExtra::column_spec(which(names(pkg_extensions_tb) ==
            "DOI"), image = zenodo_badges_chr) %>% kableExtra::column_spec(which(names(pkg_extensions_tb) ==
            "Package"), image = kableExtra::spec_image(logos_chr,
            height = 160, width = 160)) %>% kableExtra::column_spec(which(names(pkg_extensions_tb) ==
            "Website"), link = homepages_chr)
        if (is_single_1L_lgl) {
            code_ls <- pkg_extensions_kbl[1] %>% strsplit(split = "<tr>")
            code_ls <- code_ls[[1]][-3]
            pkg_extensions_kbl[1] <- code_ls %>% paste0(collapse = "")
        }
        pkg_extensions_kbl <- pkg_extensions_kbl %>% add_scroll_box(scroll_height_1L_chr = scroll_height_1L_chr,
            scroll_width_1L_chr = scroll_width_1L_chr, ...)
    }
    return(pkg_extensions_kbl)
}
#' Print vignettes
#' @description print_vignettes() is a Print function that prints output to console. Specifically, this function implements an algorithm to print vignettes. The function is called for its side effects and does not return a value.
#' @param pkg_extensions_tb Package extensions (a tibble), Default: NULL
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @param include_1L_chr Include (a character vector of length one), Default: 'modules'
#' @param module_pkgs_chr Module packages (a character vector), Default: character(0)
#' @param ns_var_nm_1L_chr Namespace variable name (a character vector of length one), Default: 'pt_ns_chr'
#' @param reference_var_nm_1L_chr Reference variable name (a character vector of length one), Default: 'Reference'
#' @param scroll_height_1L_chr Scroll height (a character vector of length one), Default: character(0)
#' @param scroll_width_1L_chr Scroll width (a character vector of length one), Default: character(0)
#' @param url_stub_1L_chr Url stub (a character vector of length one), Default: 'https://ready4-dev.github.io/'
#' @param vignette_var_nm_1L_chr Vignette variable name (a character vector of length one), Default: 'Vignettes'
#' @param vignette_url_var_nm_1L_chr Vignette url variable name (a character vector of length one), Default: 'Vignettes_URLs'
#' @param what_chr What (a character vector), Default: 'all'
#' @param ... Additional arguments
#' @return Vignettes (a kable)
#' @rdname print_vignettes
#' @export
#' @importFrom purrr flatten_chr map_chr map2
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select
#' @importFrom rvest read_html html_elements html_text2 html_attr
#' @importFrom kableExtra cell_spec kable kable_styling
#' @keywords internal
print_vignettes <- function (pkg_extensions_tb = NULL, gh_repo_1L_chr = "ready4-dev/ready4",
    gh_tag_1L_chr = "Documentation_0.0", include_1L_chr = "modules",
    module_pkgs_chr = character(0), ns_var_nm_1L_chr = "pt_ns_chr",
    reference_var_nm_1L_chr = "Reference", scroll_height_1L_chr = character(0),
    scroll_width_1L_chr = character(0), url_stub_1L_chr = "https://ready4-dev.github.io/",
    vignette_var_nm_1L_chr = "Vignettes", vignette_url_var_nm_1L_chr = "Vignettes_URLs",
    what_chr = "all", ...)
{
    if (is.null(pkg_extensions_tb))
        pkg_extensions_tb <- get_libraries_tb(gh_repo_1L_chr = gh_repo_1L_chr,
            gh_tag_1L_chr = gh_tag_1L_chr) %>% update_libraries_tb(include_1L_chr = include_1L_chr,
            module_pkgs_chr = module_pkgs_chr, ns_var_nm_1L_chr = ns_var_nm_1L_chr,
            reference_var_nm_1L_chr = reference_var_nm_1L_chr,
            url_stub_1L_chr = url_stub_1L_chr, vignette_var_nm_1L_chr = vignette_var_nm_1L_chr,
            vignette_url_var_nm_1L_chr = vignette_url_var_nm_1L_chr,
            what_chr = what_chr)
    if (is.null(pkg_extensions_tb)) {
        message("pkg_extensions_tb is NULL")
        vignettes_kbl <- NULL
    }
    else {
        vignettes_chr <- pkg_extensions_tb$Vignettes %>% purrr::flatten_chr()
        keep_lgl <- !is.na(vignettes_chr)
        vignettes_tb <- tibble::tibble(HTML = vignettes_chr[keep_lgl]) %>%
            dplyr::mutate(Title = purrr::map_chr(.data$HTML,
                ~rvest::read_html(.x) %>% rvest::html_elements("h1") %>%
                  rvest::html_text2()), RMD = purrr::map_chr(.data$HTML,
                ~rvest::read_html(.x) %>% rvest::html_elements(".dont-index") %>%
                  rvest::html_elements("a") %>% rvest::html_attr("href"))) %>%
            dplyr::mutate(Program = purrr::map2(.data$HTML, .data$RMD,
                ~kableExtra::cell_spec(c("HTML", "RMD"), "html",
                  link = c(.x, .y))))
        vignettes_kbl <- vignettes_tb %>% dplyr::select(.data$Title,
            .data$Program) %>% kableExtra::kable("html", escape = FALSE) %>%
            kableExtra::kable_styling(bootstrap_options = c("hover",
                "condensed")) %>% add_scroll_box(scroll_height_1L_chr = scroll_height_1L_chr,
            scroll_width_1L_chr = scroll_width_1L_chr, ...)
    }
    return(vignettes_kbl)
}
