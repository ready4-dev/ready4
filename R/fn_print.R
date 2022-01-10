#' Print methods
#' @description print_methods() is a Print function that prints output to console Specifically, this function implements an algorithm to print methods. The function is called for its side effects and does not return a value.
#' @param methods_tb Methods (a tibble), Default: NULL
#' @param exclude_mthds_for_chr Exclude methods for (a character vector), Default: 'NA'
#' @param methods_chr Methods (a character vector), Default: NULL
#' @param return_1L_chr Return (a character vector of length one), Default: 'all'
#' @return methods_kbl (An object)
#' @rdname print_methods
#' @export 
#' @importFrom dplyr filter
#' @importFrom purrr map_chr
#' @importFrom kableExtra kable kable_styling column_spec
print_methods <- function (methods_tb = NULL, exclude_mthds_for_chr = NA_character_, 
    methods_chr = NULL, return_1L_chr = "all") 
{
    if (is.null(methods_tb)) 
        methods_tb <- make_methods_tb()
    if (is.null(methods_chr)) 
        methods_chr <- get_generics(exclude_mthds_for_chr = exclude_mthds_for_chr, 
            return_1L_lgl = return_1L_chr)
    methods_tb <- methods_tb %>% dplyr::filter(Method %in% methods_chr)
    links_chr <- methods_tb$Method %>% purrr::map_chr(~paste0("https://ready4-dev.github.io/ready4/reference/", 
        .x, "-methods.html"))
    methods_kbl <- methods_tb %>% kableExtra::kable("html", escape = FALSE) %>% 
        kableExtra::kable_styling(bootstrap_options = c("hover", 
            "condensed")) %>% kableExtra::column_spec(which(names(methods_tb) == 
        "Method"), link = links_chr)
    return(methods_kbl)
}
#' Print modules
#' @description print_modules() is a Print function that prints output to console Specifically, this function implements an algorithm to print modules. The function is called for its side effects and does not return a value.
#' @param modules_tb Modules (a tibble)
#' @param what_1L_chr What (a character vector of length one), Default: 'All'
#' @return modules_kbl (An object)
#' @rdname print_modules
#' @export 
#' @importFrom dplyr filter select
#' @importFrom kableExtra kable kable_styling
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
#' @importFrom dplyr mutate rename select
#' @importFrom purrr map map_chr map2_chr
#' @importFrom stringr str_remove
#' @importFrom kableExtra cell_spec kable kable_styling column_spec spec_image
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
    pkg_extensions_tb <- pkg_extensions_tb %>% dplyr::mutate(Purpose = Title %>% 
        purrr::map2_chr(pt_ns_chr, ~stringr::str_remove(.x, paste0(.y, 
            ": ")))) %>% dplyr::rename(Package = Logo, Website = pt_ns_chr, 
        Examples = Vignettes_URLs)
    pkg_extensions_tb <- pkg_extensions_tb %>% dplyr::mutate(`:=`(Manuals, 
        purrr::map(manual_urls_ls, ~{
            if (identical(.x, character(0)) | is.na(.x[1]) | 
                length(.x) != 2) {
                NA_character_
            }
            else {
                kableExtra::cell_spec(c("Modeller (PDF)", "Developer (PDF)"), 
                  "html", link = .x)
            }
        }))) %>% dplyr::mutate(`:=`(`Source Code`, purrr::map(code_urls_ls, 
        ~{
            if (is.na(.x[1])) {
                NA_character_
            }
            else {
                kableExtra::cell_spec(c("Development", "Archived"), 
                  "html", link = .x)
            }
        }))) %>% dplyr::select(Type, Package, Purpose, Authors, 
        DOI, Website, Manuals, `Source Code`, Examples)
    pkg_extensions_kbl <- pkg_extensions_tb %>% kableExtra::kable("html", 
        escape = FALSE) %>% kableExtra::kable_styling(bootstrap_options = c("hover", 
        "condensed")) %>% kableExtra::column_spec(which(names(pkg_extensions_tb) == 
        "Type"), image = ready4_badges_chr) %>% kableExtra::column_spec(which(names(pkg_extensions_tb) == 
        "DOI"), image = zenodo_badges_chr) %>% kableExtra::column_spec(which(names(pkg_extensions_tb) == 
        "Package"), image = kableExtra::spec_image(logos_chr, 
        height = 160, width = 160)) %>% kableExtra::column_spec(which(names(pkg_extensions_tb) == 
        "Website"), link = homepages_chr)
    return(pkg_extensions_kbl)
}
