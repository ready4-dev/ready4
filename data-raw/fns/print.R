print_data <- function(datasets_tb,
                       by_dv_1L_lgl = FALSE,
                       filter_cdns_ls = NULL,
                       root_1L_chr = "https://dataverse.harvard.edu/dataverse/",
                       scroll_height_1L_chr = character(0),
                       scroll_width_1L_chr = character(0),
                       toy_data_dv_1L_chr = "fakes",
                       what_1L_chr = "all",
                       ...){
  if(is.null(datasets_tb)){
    message("datasets_tb is NULL")
    datasets_kbl <- NULL
  }else{
    if(by_dv_1L_lgl & ("Datasets_Meta" %in% names(datasets_tb))){
      datasets_kbl <- print_dvs(datasets_tb, root_1L_chr = root_1L_chr, scroll_height_1L_chr = scroll_height_1L_chr,
                                scroll_width_1L_chr = scroll_width_1L_chr, toy_data_dv_1L_chr = toy_data_dv_1L_chr, what_1L_chr = what_1L_chr, ...)
    }else{
      datasets_kbl <- print_dss(datasets_tb, filter_cdns_ls = filter_cdns_ls, scroll_height_1L_chr = scroll_height_1L_chr,
                                scroll_width_1L_chr = scroll_width_1L_chr, toy_data_dv_1L_chr = toy_data_dv_1L_chr, what_1L_chr = what_1L_chr, ...)
    }
  }
  return(datasets_kbl)
}
print_dss <- function(datasets_tb,
                      filter_cdns_ls = NULL,
                      scroll_height_1L_chr = character(0),
                      scroll_width_1L_chr = character(0),
                      toy_data_dv_1L_chr = "fakes",
                      what_1L_chr = "all",
                      ...){
  if(is.null(datasets_tb)){
    message("datasets_tb is NULL")
    dss_kbl <- NULL
  }else{
  datasets_tb <- make_dss_tb(datasets_tb, filter_cdns_ls = filter_cdns_ls, toy_data_dv_1L_chr = toy_data_dv_1L_chr, what_1L_chr = what_1L_chr)
  dss_kbl <- datasets_tb %>%
    kableExtra::kable("html", escape = FALSE) %>%
    kableExtra::kable_styling(bootstrap_options = c("hover", "condensed")) %>%
    add_scroll_box(scroll_height_1L_chr = scroll_height_1L_chr,
                   scroll_width_1L_chr = scroll_width_1L_chr,
                   ...)
  }
return(dss_kbl)
}
print_dvs <- function(dvs_tb,
                      filter_cdns_ls = NULL,
                      root_1L_chr = "https://dataverse.harvard.edu/dataverse/",
                      scroll_height_1L_chr = character(0),
                      scroll_width_1L_chr = character(0),
                      toy_data_dv_1L_chr = "fakes",
                      what_1L_chr = "all",
                      ...){
  if(is.null(dvs_tb)){
    message("dvs_tb is NULL")
    dvs_kbl <- NULL
  }else{
  dvs_tb <- add_references(dvs_tb, data_var_nm_1L_chr = "Contents", data_url_var_nm_1L_chr = "Datasets") %>%
    dplyr::select("Dataverse", "Name", "Description", "Creator", "Datasets") %>%
    dplyr::mutate(Datasets = .data$Datasets %>% purrr::map(~
                                                       if(identical(.x,NA_character_)){
                                                         ""}else{
                                                           .x
                                                         }))
  if(what_1L_chr == "real")
    dvs_tb <- dvs_tb %>%
      dplyr::filter(.data$Dataverse != toy_data_dv_1L_chr)
  if(what_1L_chr == "fakes")
    dvs_tb <- dvs_tb %>%
      dplyr::filter(.data$Dataverse == toy_data_dv_1L_chr)
  dvs_kbl <- dvs_tb %>%
    kableExtra::kable("html", escape = FALSE) %>%
    kableExtra::kable_styling(bootstrap_options = c("hover", "condensed")) %>%
    kableExtra::column_spec(which(names(dvs_tb)=="Dataverse"),
                            link = paste0(root_1L_chr,
                                          dvs_tb$Dataverse)) %>%
    add_scroll_box(scroll_height_1L_chr = scroll_height_1L_chr,
                   scroll_width_1L_chr = scroll_width_1L_chr,
                   ...)
  }
  return(dvs_kbl)
}
print_methods <- function(methods_tb = NULL,
                          exclude_mthds_for_chr = NA_character_,
                          gh_repo_1L_chr = "ready4-dev/ready4",
                          gh_tag_1L_chr = "Documentation_0.0",
                          methods_chr = NULL,
                          module_pkgs_chr = character(0), ##
                          ns_var_nm_1L_chr = "pt_ns_chr", ##
                          path_1L_chr = character(0),
                          packages_tb = NULL,
                          return_1L_chr = "all",
                          scroll_height_1L_chr = character(0),
                          scroll_width_1L_chr = character(0),
                          ...){
  if(is.null(methods_tb))
    methods_tb <- make_methods_tb(exclude_mthds_for_chr = exclude_mthds_for_chr,
                                  gh_repo_1L_chr = gh_repo_1L_chr,
                                  gh_tag_1L_chr = gh_tag_1L_chr,
                                  module_pkgs_chr = module_pkgs_chr,
                                  ns_var_nm_1L_chr = ns_var_nm_1L_chr,
                                  packages_tb = packages_tb,
                                  path_1L_chr = path_1L_chr,
                                  return_1L_chr = return_1L_chr)
  if(is.null(methods_chr))
    methods_chr <- get_generics(exclude_mthds_for_chr = exclude_mthds_for_chr,
                                return_1L_chr = return_1L_chr)
  if(is.null(methods_tb)){
    message("methods_tb is NULL")
    methods_kbl <- NULL
  }else{
  methods_tb <- methods_tb %>%
    dplyr::filter(.data$Method %in% methods_chr)
  links_chr <- methods_tb$Method %>%
    purrr::map_chr(~paste0("https://ready4-dev.github.io/ready4/reference/",.x,"-methods.html"))
  methods_kbl <- methods_tb %>%
    kableExtra::kable("html", escape = FALSE) %>%
    kableExtra::kable_styling(bootstrap_options = c("hover", "condensed")) %>%
    kableExtra::column_spec(which(names(methods_tb)=="Method"),
                            link = links_chr) %>%
    add_scroll_box(scroll_height_1L_chr = scroll_height_1L_chr,
                   scroll_width_1L_chr = scroll_width_1L_chr,
                   ...)
  }
  return(methods_kbl)
}
print_modules <- function(modules_tb,
                          scroll_height_1L_chr = character(0),
                          scroll_width_1L_chr = character(0),
                          what_1L_chr = "All",
                          ...
                          ){
  if(is.null(modules_tb)){
    message("modules_tb is NULL")
    modules_kbl <- NULL
  }else{
  if(what_1L_chr == "S4"){
    modules_tb <- modules_tb %>%
      dplyr::filter(!.data$old_class_lgl)
  }
  if(what_1L_chr == "S3"){
    modules_tb <- modules_tb %>%
      dplyr::filter(.data$old_class_lgl)
  }
  modules_kbl <- modules_tb %>%
    dplyr::select(-"old_class_lgl",
                  -"Library")  %>%
    kableExtra::kable("html", escape = FALSE) %>%
    kableExtra::kable_styling(bootstrap_options = c("hover", "condensed")) %>%
    add_scroll_box(scroll_height_1L_chr = scroll_height_1L_chr,
                   scroll_width_1L_chr = scroll_width_1L_chr,
                   ...)
  }
  return(modules_kbl)
}
print_packages <- function (pkg_extensions_tb = NULL,
                            gh_repo_1L_chr = "ready4-dev/ready4",
                            gh_tag_1L_chr = "Documentation_0.0",
                            include_1L_chr = "modules",
                            module_pkgs_chr = character(0), ##
                            ns_var_nm_1L_chr = "pt_ns_chr", #
                            project_badges_url_1L_chr = "https://img.shields.io/badge/ready4", ##
                            reference_var_nm_1L_chr = "Reference",
                            scroll_height_1L_chr = character(0), scroll_width_1L_chr = character(0),
                            url_stub_1L_chr = "https://ready4-dev.github.io/",
                            vignette_var_nm_1L_chr = "Vignettes",
                            vignette_url_var_nm_1L_chr = "Vignettes_URLs",
                            what_chr = "all", #
                            ...)
{
  if (is.null(pkg_extensions_tb))
    pkg_extensions_tb <- get_libraries_tb(gh_repo_1L_chr = gh_repo_1L_chr,
                                          gh_tag_1L_chr = gh_tag_1L_chr) %>%
      update_libraries_tb(include_1L_chr = include_1L_chr,
                          module_pkgs_chr = module_pkgs_chr,
                          ns_var_nm_1L_chr = ns_var_nm_1L_chr,
                          reference_var_nm_1L_chr = reference_var_nm_1L_chr,
                          url_stub_1L_chr = url_stub_1L_chr,
                          vignette_var_nm_1L_chr = vignette_var_nm_1L_chr,
                          vignette_url_var_nm_1L_chr = vignette_url_var_nm_1L_chr,
                          what_chr = what_chr )
  if(is.null(pkg_extensions_tb)){
    message("pkg_extensions_tb is NULL")
    pkg_extensions_kbl <- NULL
  }else{
  if(nrow(pkg_extensions_tb) == 1){
    pkg_extensions_tb <- rbind(pkg_extensions_tb,pkg_extensions_tb)
    is_single_1L_lgl <- TRUE
  }else{
    is_single_1L_lgl <- FALSE
  }
  pkg_extensions_tb <- pkg_extensions_tb %>%
    dplyr::mutate(Badges = purrr::map(.data$pt_ns_chr,
                                      ~ get_badge_urls(.x,
                                                      project_badges_url_1L_chr = project_badges_url_1L_chr,
                                                      url_stub_1L_chr = url_stub_1L_chr))) %>% dplyr::mutate(Type = "") %>%
    dplyr::mutate(DOI = "") %>% dplyr::mutate(Logo = "")
  ready4_badges_chr <- purrr::map_chr(pkg_extensions_tb$Badges,
                                      ~{
                                        badge_1L_chr <- .x$ready4_1L_chr
                                        ifelse(identical(badge_1L_chr, character(0)),
                                               NA_character_,
                                               # paste0("https://img.shields.io/badge/ready4-authoring-maroon?style=flat&labelColor=black&logo=data:",
                                               # "image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAFzUkdCAK7OHOkAAAAEZ0FNQQAAsY8L/",
                                               # "GEFAAAACXBIWXMAABYlAAAWJQFJUiTwAAAAIXRFWHRDcmVhdGlvbiBUaW1lADIwMjI6MDM6MDcgMTY6MTM6NTPZeG5UAAABa0lEQVQ4T4WT607CQBCFpyUi3qIR0eAfNfCi/",
                                               # "vENfEgENIAIlcJ6vr1oLaZOerJzdst0zpklc49nznqHZs6ZfWwDem1xM1sqXwtXkb8rL4SuOLEoLXPPXWfD01Dg9dPsrTQbngQ+EZ+LDyIfiy/FHyIfFZbbTslWKOOqxx/",
                                               # "uWBPSfp07FahGlqlNfWGqL9HNfBO+CAfwdO55WS8g4MFML834sfJVA9e7vwsg50aGohncdmRojV9XeL+jArRNmZxVSJ4Acj3NHqARdyeFJqC2KJiCfKE9zsfxnNYTl5TcCtmNMcwY/",
                                               # "ZXf+3wdzzVza2vj4iCaq3d1R/bvwVSH6IPjNIUHx0FSNZA7WquDqOVb35+eiO8h7Oe+vRfp0a3yGtFMDuiAIg2R20YaVwJ3Hj+4kehO/J/I7VJ/",
                                               # "jHtpvBP6mrHnR4EzdyQ0xI8HhM8jUiChxVpDK3iVuadzx43yRdI4E2d0gNtX74TCs419AR8YEST/cHPBAAAAAElFTkSuQmCC"),
                                               badge_1L_chr)
                                        #.x$ready4_1L_chr
                                        })
  zenodo_badges_chr <- purrr::map_chr(pkg_extensions_tb$Badges,
                                      ~{
                                        badge_1L_chr <- .x$zenodo_1L_chr
                                        ifelse(identical(badge_1L_chr, character(0)),
                                               NA_character_,#"https://zenodo.org/badge/DOI/10.5281/zenodo.5646668.svg",
                                               badge_1L_chr)
                                      })
  logos_chr <- purrr::map_chr(pkg_extensions_tb$pt_ns_chr,
                              ~paste0("https://ready4-dev.github.io/", .x, "/logo.png"))
  homepages_chr <- pkg_extensions_tb$Link
  pkg_extensions_tb <- pkg_extensions_tb %>% dplyr::mutate(Purpose = .data$Title %>%
                                                             purrr::map2_chr(.data$pt_ns_chr,
                                                                             ~stringr::str_remove(.x, paste0(.y,
                                                                                                             ": ")))) %>%
    dplyr::rename(Package = .data$Logo,
                  Website = .data$Link,
                  Examples = .data$Vignettes_URLs)
  pkg_extensions_tb <- pkg_extensions_tb %>%
    dplyr::mutate(Examples = purrr::map(.data$Examples,
                                        ~if (is.na(.x[1])) {
                                          ""
                                          } else {
                                            .x
                                            })) %>%
    dplyr::mutate(Documentation = purrr::pmap(list(.data$manual_urls_ls,
                                                   .data$Citation, .data$Website),
                                              ~{
                                                if (identical(..1, character(0)) | is.na(..1[1]) | length(..1) != 2) {
                                                  manual_txt_chr <- character(0)
                                                  } else {
                                                    manual_txt_chr <- c("Manual - Short (PDF)", "Manual - Full (PDF)")
                                                    }
                                                kableExtra::cell_spec(c("Citation", "Website", manual_txt_chr),
                                                                      "html", link = c(..2, ..3, ..1))
                                                })) %>%
    dplyr::mutate(Code = purrr::map(.data$code_urls_ls,
                                    ~{
                                      if (is.na(.x[1])) {
                                        ""
                                        } else {
                                          kableExtra::cell_spec(c("Dev", "Archive"), "html",
                                                                link = .x)
                                          }
                                      })) %>%
    dplyr::select("Type", "Package", "Purpose", "Documentation", "Code", "Examples")
  pkg_extensions_kbl <- pkg_extensions_tb %>%
    kableExtra::kable("html",
                      escape = FALSE) %>%
    kableExtra::kable_styling(bootstrap_options = c("hover",
                                                    "condensed")) %>%
    kableExtra::column_spec(which(names(pkg_extensions_tb) == "Type"),
                            image = ready4_badges_chr) %>%
    kableExtra::column_spec(which(names(pkg_extensions_tb) == "DOI"),
                            image = zenodo_badges_chr) %>%
    kableExtra::column_spec(which(names(pkg_extensions_tb) == "Package"),
                            image = kableExtra::spec_image(logos_chr,
                                                           height = 160, width = 160)) %>%
    kableExtra::column_spec(which(names(pkg_extensions_tb) == "Website"),
                            link = homepages_chr)
  if(is_single_1L_lgl){
    code_ls <- pkg_extensions_kbl[1] %>% strsplit(split = "<tr>")
    code_ls <- code_ls[[1]][-3]
    pkg_extensions_kbl[1] <- code_ls %>% paste0(collapse = "")
  }
  pkg_extensions_kbl <- pkg_extensions_kbl %>%
    add_scroll_box(scroll_height_1L_chr = scroll_height_1L_chr,
                   scroll_width_1L_chr = scroll_width_1L_chr, ...)
  }
  return(pkg_extensions_kbl)
}
print_vignettes <- function(pkg_extensions_tb = NULL,
                            gh_repo_1L_chr = "ready4-dev/ready4",
                            gh_tag_1L_chr = "Documentation_0.0",
                            include_1L_chr = "modules",
                            module_pkgs_chr = character(0), ##
                            ns_var_nm_1L_chr = "pt_ns_chr", #
                            reference_var_nm_1L_chr = "Reference",
                            scroll_height_1L_chr = character(0),
                            scroll_width_1L_chr = character(0),
                            url_stub_1L_chr = "https://ready4-dev.github.io/",
                            vignette_var_nm_1L_chr = "Vignettes",
                            vignette_url_var_nm_1L_chr = "Vignettes_URLs",
                            what_chr = "all",
                            ...){
  if(is.null(pkg_extensions_tb))
    pkg_extensions_tb <- get_libraries_tb(gh_repo_1L_chr = gh_repo_1L_chr,
                                          gh_tag_1L_chr = gh_tag_1L_chr) %>%
      update_libraries_tb(include_1L_chr = include_1L_chr,
                          module_pkgs_chr = module_pkgs_chr,
                          ns_var_nm_1L_chr = ns_var_nm_1L_chr,
                          reference_var_nm_1L_chr = reference_var_nm_1L_chr,
                          url_stub_1L_chr = url_stub_1L_chr,
                          vignette_var_nm_1L_chr = vignette_var_nm_1L_chr,
                          vignette_url_var_nm_1L_chr = vignette_url_var_nm_1L_chr,
                          what_chr = what_chr)
  if(is.null(pkg_extensions_tb)){
    message("pkg_extensions_tb is NULL")
    vignettes_kbl <- NULL
  }else{
  vignettes_chr <- pkg_extensions_tb$Vignettes %>% purrr::flatten_chr()
  keep_lgl <- !is.na(vignettes_chr)
  vignettes_tb <- tibble::tibble(HTML = vignettes_chr[keep_lgl]#,
                                 # HTML = pkg_extensions_tb$Examples %>%
                                 #   purrr::flatten_chr() %>%
                                 #   purrr::keep(keep_lgl)
                                 ) %>%
    dplyr::mutate(Title = purrr::map_chr(.data$HTML,
                                         ~ rvest::read_html(.x) %>%
                    rvest::html_elements("h1")  %>%
                    rvest::html_text2()),
                  RMD = purrr::map_chr(.data$HTML,
                                       ~ rvest::read_html(.x) %>%
                    rvest::html_elements(".dont-index")  %>%
                      rvest::html_elements("a") %>%
                      rvest::html_attr("href"))) %>%
    dplyr::mutate(Program = purrr::map2(.data$HTML, .data$RMD,
                                        ~kableExtra::cell_spec(c("HTML",
                                                                 "RMD"),
                                                               "html",
                                                               link = c(.x,.y))))
  vignettes_kbl <- vignettes_tb %>%
    dplyr::select(.data$Title, .data$Program) %>%
    kableExtra::kable("html", escape = FALSE) %>%
    kableExtra::kable_styling(bootstrap_options = c("hover", "condensed")) %>%
    add_scroll_box(scroll_height_1L_chr = scroll_height_1L_chr,
                   scroll_width_1L_chr = scroll_width_1L_chr,
                   ...)
}
  return(vignettes_kbl)
}

