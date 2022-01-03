#' Print modules
#' @description print_modules() is a Print function that prints output to console Specifically, this function implements an algorithm to print modules. The function is called for its side effects and does not return a value.
#' @param modules_tb Modules (a tibble)
#' @param what_1L_chr What (a character vector of length one), Default: 'All'
#' @return modules_kbl (An object)
#' @rdname print_modules
#' @export 
#' @importFrom dplyr filter select
#' @importFrom kableExtra kable kable_styling
#' @keywords internal
print_modules <- function (modules_tb, what_1L_chr = "All") 
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
            "condensed"))
    return(modules_kbl)
}
#' Print package extensions
#' @description print_pkg_extensions() is a Print function that prints output to console Specifically, this function implements an algorithm to print package extensions. The function is called for its side effects and does not return a value.
#' @param pkg_extensions_tb Package extensions (a tibble), Default: NULL
#' @return pkg_extensions_kbl (An object)
#' @rdname print_pkg_extensions
#' @export 
#' @importFrom dplyr mutate select
#' @importFrom purrr map map_chr
#' @importFrom kableExtra kbl kable_paper column_spec spec_image
#' @keywords internal
print_pkg_extensions <- function (pkg_extensions_tb = NULL) 
{
    if (is.null(pkg_extensions_tb)) 
        pkg_extensions_tb <- make_pkg_extensions_tb()
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
    pkg_extensions_tb <- pkg_extensions_tb %>% dplyr::select(Type, 
        Logo, Package, Authors, DOI)
    pkg_extensions_kbl <- pkg_extensions_tb %>% kableExtra::kbl(booktabs = T) %>% 
        kableExtra::kable_paper(full_width = F) %>% kableExtra::column_spec(which(names(pkg_extensions_tb) == 
        "Type"), image = ready4_badges_chr) %>% kableExtra::column_spec(which(names(pkg_extensions_tb) == 
        "DOI"), image = zenodo_badges_chr) %>% kableExtra::column_spec(which(names(pkg_extensions_tb) == 
        "Logo"), image = kableExtra::spec_image(logos_chr, height = 100, 
        width = 100)) %>% kableExtra::column_spec(which(names(pkg_extensions_tb) == 
        "Package"), link = homepages_chr)
    return(pkg_extensions_kbl)
}
