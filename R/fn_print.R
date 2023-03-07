#' Print data
#' @description print_data() is a Print function that prints output to console Specifically, this function implements an algorithm to print data. The function is called for its side effects and does not return a value.
#' @param datasets_tb Datasets (a tibble)
#' @param by_dv_1L_lgl By dataverse (a logical vector of length one), Default: F
#' @param root_1L_chr Root (a character vector of length one), Default: 'https://dataverse.harvard.edu/dataverse/'
#' @param scroll_height_1L_chr Scroll height (a character vector of length one), Default: character(0)
#' @param scroll_width_1L_chr Scroll width (a character vector of length one), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'all'
#' @param ... Additional arguments
#' @return Datasets (a kable)
#' @rdname print_data
#' @export 
#' @keywords internal
print_data <- function (datasets_tb, by_dv_1L_lgl = F, root_1L_chr = "https://dataverse.harvard.edu/dataverse/", 
    scroll_height_1L_chr = character(0), scroll_width_1L_chr = character(0), 
    what_1L_chr = "all", ...) 
{
    if (by_dv_1L_lgl) {
        datasets_kbl <- print_dvs(datasets_tb, root_1L_chr = root_1L_chr, 
            scroll_height_1L_chr = scroll_height_1L_chr, scroll_width_1L_chr = scroll_width_1L_chr, 
            what_1L_chr = what_1L_chr, ...)
    }
    else {
        datasets_kbl <- print_dss(datasets_tb, scroll_height_1L_chr = scroll_height_1L_chr, 
            scroll_width_1L_chr = scroll_width_1L_chr, what_1L_chr = what_1L_chr, 
            ...)
    }
    return(datasets_kbl)
}
#' Print datasets
#' @description print_dss() is a Print function that prints output to console Specifically, this function implements an algorithm to print datasets. The function is called for its side effects and does not return a value.
#' @param dvs_tb Dataverses (a tibble)
#' @param scroll_height_1L_chr Scroll height (a character vector of length one), Default: character(0)
#' @param scroll_width_1L_chr Scroll width (a character vector of length one), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'all'
#' @param ... Additional arguments
#' @return Datasets (a kable)
#' @rdname print_dss
#' @export 
#' @importFrom dplyr filter select mutate
#' @importFrom purrr pmap_dfr map_dfr
#' @importFrom tibble tibble
#' @importFrom kableExtra kable kable_styling
print_dss <- function (dvs_tb, scroll_height_1L_chr = character(0), scroll_width_1L_chr = character(0), 
    what_1L_chr = "all", ...) 
{
    dss_tb <- dvs_tb %>% dplyr::filter(!is.na(Contents)) %>% 
        dplyr::select(Contents, Datasets_Meta, Dataverse) %>% 
        purrr::pmap_dfr(~{
            ..2 %>% purrr::map_dfr(~{
                fields_ls <- .x$fields
                tibble::tibble(Title = fields_ls$value[which(fields_ls$typeName == 
                  "title")][[1]], Description = fields_ls$value[which(fields_ls$typeName == 
                  "dsDescription")][[1]][[1]][[4]])
            }) %>% dplyr::mutate(Dataverse = ..3, DOI = ..1)
        })
    if (what_1L_chr == "real") 
        dss_tb <- dss_tb %>% dplyr::filter(Dataverse != "fakes")
    if (what_1L_chr == "fakes") 
        dss_tb <- dss_tb %>% dplyr::filter(Dataverse == "fakes")
    dss_kbl <- dss_tb %>% kableExtra::kable("html", escape = FALSE) %>% 
        kableExtra::kable_styling(bootstrap_options = c("hover", 
            "condensed")) %>% add_scroll_box(scroll_height_1L_chr = scroll_height_1L_chr, 
        scroll_width_1L_chr = scroll_width_1L_chr, ...)
    return(dss_kbl)
}
#' Print dataverses
#' @description print_dvs() is a Print function that prints output to console Specifically, this function implements an algorithm to print dataverses. The function is called for its side effects and does not return a value.
#' @param dvs_tb Dataverses (a tibble)
#' @param root_1L_chr Root (a character vector of length one), Default: 'https://dataverse.harvard.edu/dataverse/'
#' @param scroll_height_1L_chr Scroll height (a character vector of length one), Default: character(0)
#' @param scroll_width_1L_chr Scroll width (a character vector of length one), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'all'
#' @param ... Additional arguments
#' @return Dataverses (a kable)
#' @rdname print_dvs
#' @export 
#' @importFrom dplyr select mutate filter
#' @importFrom purrr map
#' @importFrom kableExtra kable kable_styling column_spec
print_dvs <- function (dvs_tb, root_1L_chr = "https://dataverse.harvard.edu/dataverse/", 
    scroll_height_1L_chr = character(0), scroll_width_1L_chr = character(0), 
    what_1L_chr = "all", ...) 
{
    dvs_tb <- add_references(dvs_tb, data_var_nm_1L_chr = "Contents", 
        data_url_var_nm_1L_chr = "Datasets") %>% dplyr::select(Dataverse, 
        Name, Description, Creator, Datasets) %>% dplyr::mutate(Datasets = Datasets %>% 
        purrr::map(~if (identical(.x, NA_character_)) {
            ""
        }
        else {
            .x
        }))
    if (what_1L_chr == "real") 
        dvs_tb <- dvs_tb %>% dplyr::filter(Dataverse != "fakes")
    if (what_1L_chr == "fakes") 
        dvs_tb <- dvs_tb %>% dplyr::filter(Dataverse == "fakes")
    dvs_kbl <- dvs_tb %>% kableExtra::kable("html", escape = FALSE) %>% 
        kableExtra::kable_styling(bootstrap_options = c("hover", 
            "condensed")) %>% kableExtra::column_spec(which(names(dvs_tb) == 
        "Dataverse"), link = paste0(root_1L_chr, dvs_tb$Dataverse)) %>% 
        add_scroll_box(scroll_height_1L_chr = scroll_height_1L_chr, 
            scroll_width_1L_chr = scroll_width_1L_chr, ...)
    return(dvs_kbl)
}
#' Print methods
#' @description print_methods() is a Print function that prints output to console Specifically, this function implements an algorithm to print methods. The function is called for its side effects and does not return a value.
#' @param methods_tb Methods (a tibble), Default: NULL
#' @param exclude_mthds_for_chr Exclude methods for (a character vector), Default: 'NA'
#' @param methods_chr Methods (a character vector), Default: NULL
#' @param path_1L_chr Path (a character vector of length one), Default: character(0)
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
print_methods <- function (methods_tb = NULL, exclude_mthds_for_chr = NA_character_, 
    methods_chr = NULL, path_1L_chr = character(0), return_1L_chr = "all", 
    scroll_height_1L_chr = character(0), scroll_width_1L_chr = character(0), 
    ...) 
{
    if (is.null(methods_tb)) 
        methods_tb <- make_methods_tb(path_1L_chr = path_1L_chr)
    if (is.null(methods_chr)) 
        methods_chr <- get_generics(exclude_mthds_for_chr = exclude_mthds_for_chr, 
            return_1L_chr = return_1L_chr)
    methods_tb <- methods_tb %>% dplyr::filter(Method %in% methods_chr)
    links_chr <- methods_tb$Method %>% purrr::map_chr(~paste0("https://ready4-dev.github.io/ready4/reference/", 
        .x, "-methods.html"))
    methods_kbl <- methods_tb %>% kableExtra::kable("html", escape = FALSE) %>% 
        kableExtra::kable_styling(bootstrap_options = c("hover", 
            "condensed")) %>% kableExtra::column_spec(which(names(methods_tb) == 
        "Method"), link = links_chr) %>% add_scroll_box(scroll_height_1L_chr = scroll_height_1L_chr, 
        scroll_width_1L_chr = scroll_width_1L_chr, ...)
    return(methods_kbl)
}
#' Print modules
#' @description print_modules() is a Print function that prints output to console Specifically, this function implements an algorithm to print modules. The function is called for its side effects and does not return a value.
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
print_modules <- function (modules_tb, scroll_height_1L_chr = character(0), scroll_width_1L_chr = character(0), 
    what_1L_chr = "All", ...) 
{
    if (what_1L_chr == "S4") {
        modules_tb <- modules_tb %>% dplyr::filter(!old_class_lgl)
    }
    if (what_1L_chr == "S3") {
        modules_tb <- modules_tb %>% dplyr::filter(old_class_lgl)
    }
    modules_kbl <- modules_tb %>% dplyr::select(-old_class_lgl, 
        -Library) %>% kableExtra::kable("html", escape = FALSE) %>% 
        kableExtra::kable_styling(bootstrap_options = c("hover", 
            "condensed")) %>% add_scroll_box(scroll_height_1L_chr = scroll_height_1L_chr, 
        scroll_width_1L_chr = scroll_width_1L_chr, ...)
    return(modules_kbl)
}
#' Print packages
#' @description print_packages() is a Print function that prints output to console Specifically, this function implements an algorithm to print packages. The function is called for its side effects and does not return a value.
#' @param pkg_extensions_tb Package extensions (a tibble), Default: NULL
#' @param include_1L_chr Include (a character vector of length one), Default: 'modules'
#' @param scroll_height_1L_chr Scroll height (a character vector of length one), Default: character(0)
#' @param scroll_width_1L_chr Scroll width (a character vector of length one), Default: character(0)
#' @param ... Additional arguments
#' @return Package extensions (a kable)
#' @rdname print_packages
#' @export 
#' @importFrom dplyr mutate rename select
#' @importFrom purrr map map_chr map2_chr pmap
#' @importFrom stringr str_remove
#' @importFrom kableExtra cell_spec kable kable_styling column_spec spec_image
print_packages <- function (pkg_extensions_tb = NULL, include_1L_chr = "modules", 
    scroll_height_1L_chr = character(0), scroll_width_1L_chr = character(0), 
    ...) 
{
    if (is.null(pkg_extensions_tb)) 
        pkg_extensions_tb <- make_libraries_tb(include_1L_chr = include_1L_chr)
    if (nrow(pkg_extensions_tb) == 1) {
        pkg_extensions_tb <- rbind(pkg_extensions_tb, pkg_extensions_tb)
        is_single_1L_lgl <- T
    }
    else {
        is_single_1L_lgl <- F
    }
    pkg_extensions_tb <- pkg_extensions_tb %>% dplyr::mutate(Badges = purrr::map(pt_ns_chr, 
        ~get_badge_urls(.x))) %>% dplyr::mutate(Type = "") %>% 
        dplyr::mutate(DOI = "") %>% dplyr::mutate(Logo = "")
    ready4_badges_chr <- purrr::map_chr(pkg_extensions_tb$Badges, 
        ~.x$ready4_1L_chr)
    zenodo_badges_chr <- purrr::map_chr(pkg_extensions_tb$Badges, 
        ~{
            badge_1L_chr <- .x$zenodo_1L_chr
            ifelse(identical(badge_1L_chr, character(0)), "https://zenodo.org/badge/DOI/10.5281/zenodo.5646668.svg", 
                badge_1L_chr)
        })
    logos_chr <- purrr::map_chr(pkg_extensions_tb$pt_ns_chr, 
        ~paste0("https://ready4-dev.github.io/", .x, "/logo.png"))
    homepages_chr <- pkg_extensions_tb$Link
    pkg_extensions_tb <- pkg_extensions_tb %>% dplyr::mutate(Purpose = Title %>% 
        purrr::map2_chr(pt_ns_chr, ~stringr::str_remove(.x, paste0(.y, 
            ": ")))) %>% dplyr::rename(Package = Logo, Website = Link, 
        Examples = Vignettes_URLs)
    pkg_extensions_tb <- pkg_extensions_tb %>% dplyr::mutate(Examples = purrr::map(Examples, 
        ~if (is.na(.x[1])) {
            ""
        }
        else {
            .x
        })) %>% dplyr::mutate(Documentation = purrr::pmap(list(manual_urls_ls, 
        Citation, Website), ~{
        if (identical(..1, character(0)) | is.na(..1[1]) | length(..1) != 
            2) {
            manual_txt_chr <- character(0)
        }
        else {
            manual_txt_chr <- c("Manual - Short (PDF)", "Manual - Full (PDF)")
        }
        kableExtra::cell_spec(c("Citation", "Website", manual_txt_chr), 
            "html", link = c(..2, ..3, ..1))
    })) %>% dplyr::mutate(Code = purrr::map(code_urls_ls, ~{
        if (is.na(.x[1])) {
            ""
        }
        else {
            kableExtra::cell_spec(c("Dev", "Archive"), "html", 
                link = .x)
        }
    })) %>% dplyr::select(Type, Package, Purpose, Documentation, 
        Code, Examples)
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
    return(pkg_extensions_kbl)
}
#' Print vignettes
#' @description print_vignettes() is a Print function that prints output to console Specifically, this function implements an algorithm to print vignettes. The function is called for its side effects and does not return a value.
#' @param pkg_extensions_tb Package extensions (a tibble), Default: NULL
#' @param include_1L_chr Include (a character vector of length one), Default: 'modules'
#' @param scroll_height_1L_chr Scroll height (a character vector of length one), Default: character(0)
#' @param scroll_width_1L_chr Scroll width (a character vector of length one), Default: character(0)
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
print_vignettes <- function (pkg_extensions_tb = NULL, include_1L_chr = "modules", 
    scroll_height_1L_chr = character(0), scroll_width_1L_chr = character(0), 
    ...) 
{
    if (is.null(pkg_extensions_tb)) 
        pkg_extensions_tb <- make_libraries_tb(include_1L_chr = include_1L_chr)
    vignettes_chr <- pkg_extensions_tb$Vignettes %>% purrr::flatten_chr()
    keep_lgl <- !is.na(vignettes_chr)
    vignettes_tb <- tibble::tibble(HTML = vignettes_chr[keep_lgl]) %>% 
        dplyr::mutate(Title = purrr::map_chr(HTML, ~rvest::read_html(.x) %>% 
            rvest::html_elements("h1") %>% rvest::html_text2()), 
            RMD = purrr::map_chr(HTML, ~rvest::read_html(.x) %>% 
                rvest::html_elements(".dont-index") %>% rvest::html_elements("a") %>% 
                rvest::html_attr("href"))) %>% dplyr::mutate(Program = purrr::map2(HTML, 
        RMD, ~kableExtra::cell_spec(c("HTML", "RMD"), "html", 
            link = c(.x, .y))))
    vignettes_kbl <- vignettes_tb %>% dplyr::select(Title, Program) %>% 
        kableExtra::kable("html", escape = FALSE) %>% kableExtra::kable_styling(bootstrap_options = c("hover", 
        "condensed")) %>% add_scroll_box(scroll_height_1L_chr = scroll_height_1L_chr, 
        scroll_width_1L_chr = scroll_width_1L_chr, ...)
    return(vignettes_kbl)
}
