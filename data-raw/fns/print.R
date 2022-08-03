print_data <- function(datasets_tb,
                       by_dv_1L_lgl = F,
                       root_1L_chr = "https://dataverse.harvard.edu/dataverse/",
                       what_1L_chr = "all"){
  if(by_dv_1L_lgl){
    datasets_kbl <- print_dvs(datasets_tb,
              root_1L_chr = root_1L_chr,
              what_1L_chr = what_1L_chr)
  }else{
    datasets_kbl <- print_dss(datasets_tb,
                              what_1L_chr = what_1L_chr)
  }
  return(datasets_kbl)
}
print_dss <- function(dvs_tb,
                      what_1L_chr = "all"){
dss_tb <- dvs_tb %>%
    dplyr::filter(!is.na(Contents)) %>%
  dplyr::select(Contents,
                Datasets_Meta,
                Dataverse) %>%
  purrr::pmap_dfr(~ {
    ..2 %>%
                    purrr::map_dfr(~{
                      fields_ls <- .x$fields
                      tibble::tibble(Title = fields_ls$value[which(fields_ls$typeName == "title")][[1]],
                             Description = fields_ls$value[which(fields_ls$typeName == "dsDescription")][[1]][[1]][[4]])
                      }) %>%
      dplyr::mutate(Dataverse = ..3,
                    DOI = ..1)
    })
if(what_1L_chr == "real")
  dss_tb <- dss_tb %>%
    dplyr::filter(Dataverse != "fakes")
if(what_1L_chr == "fakes")
  dss_tb <- dss_tb %>%
    dplyr::filter(Dataverse == "fakes")
dss_kbl <- dss_tb %>%
  kableExtra::kable("html", escape = FALSE) %>%
  kableExtra::kable_styling(bootstrap_options = c("hover", "condensed"))
return(dss_kbl)
}
print_dvs <- function(dvs_tb,
                      root_1L_chr = "https://dataverse.harvard.edu/dataverse/",
                      what_1L_chr = "all"){
  dvs_tb <- add_references(dvs_tb,
                            data_var_nm_1L_chr = "Contents",
                            data_url_var_nm_1L_chr = "Datasets") %>%
    dplyr::select(Dataverse, Name, Description, Creator, Datasets) %>%
    dplyr::mutate(Datasets = Datasets %>% purrr::map(~
                                                       if(identical(.x,NA_character_)){
                                                         ""}else{
                                                           .x
                                                         }))
  if(what_1L_chr == "real")
    dvs_tb <- dvs_tb %>%
      dplyr::filter(Dataverse != "fakes")
  if(what_1L_chr == "fakes")
    dvs_tb <- dvs_tb %>%
      dplyr::filter(Dataverse == "fakes")
  dvs_kbl <- dvs_tb %>%
    kableExtra::kable("html", escape = FALSE) %>%
    kableExtra::kable_styling(bootstrap_options = c("hover", "condensed")) %>%
    kableExtra::column_spec(which(names(dvs_tb)=="Dataverse"),
                            link = paste0(root_1L_chr,
                                          dvs_tb$Dataverse))
  return(dvs_kbl)
}
print_methods <- function(methods_tb = NULL,
                          exclude_mthds_for_chr = NA_character_,
                          methods_chr = NULL,
                          path_1L_chr = character(0),
                          return_1L_chr = "all"){
  if(is.null(methods_tb))
    methods_tb <- make_methods_tb(path_1L_chr = path_1L_chr)
  if(is.null(methods_chr))
    methods_chr <- get_generics(exclude_mthds_for_chr = exclude_mthds_for_chr,
                                return_1L_chr = return_1L_chr)
  methods_tb <- methods_tb %>%
    dplyr::filter(Method %in% methods_chr)
  links_chr <- methods_tb$Method %>%
    purrr::map_chr(~paste0("https://ready4-dev.github.io/ready4/reference/",.x,"-methods.html"))
  methods_kbl <- methods_tb %>%
    # kableExtra::kbl(booktabs = T) %>%
    # kableExtra::kable_paper(full_width = F) %>%
    kableExtra::kable("html", escape = FALSE) %>%
    kableExtra::kable_styling(bootstrap_options = c("hover", "condensed")) %>%
    kableExtra::column_spec(which(names(methods_tb)=="Method"),
                            link = links_chr)
  return(methods_kbl)
}
print_modules <- function(modules_tb,
                          what_1L_chr = "All"){
  if(what_1L_chr == "S4"){
    modules_tb <- modules_tb %>%
      dplyr::filter(!old_class_lgl)
  }
  if(what_1L_chr == "S3"){
    modules_tb <- modules_tb %>%
      dplyr::filter(old_class_lgl)
  }
  modules_kbl <- modules_tb %>%
    dplyr::select(-old_class_lgl,
                  -Library)  %>%
    kableExtra::kable("html", escape = FALSE) %>%
    kableExtra::kable_styling(bootstrap_options = c("hover", "condensed"))
  return(modules_kbl)
}
print_packages <- function(pkg_extensions_tb = NULL,
                           include_1L_chr = "modules"){
  if(is.null(pkg_extensions_tb))
    pkg_extensions_tb <- make_libraries_tb(include_1L_chr = include_1L_chr)
  pkg_extensions_tb <- pkg_extensions_tb %>%
    dplyr::mutate(Badges = purrr::map(pt_ns_chr,
                                      ~ get_badge_urls(.x))) %>%
    dplyr::mutate(Type = "") %>%
    dplyr::mutate(DOI = "") %>%
    dplyr::mutate(Logo = "")
  ready4_badges_chr <- purrr::map_chr(pkg_extensions_tb$Badges,
                                      ~ .x$ready4_1L_chr)
  zenodo_badges_chr <- purrr::map_chr(pkg_extensions_tb$Badges,
                                      ~ {
                                        badge_1L_chr <-.x$zenodo_1L_chr
                                        ifelse(identical(badge_1L_chr, character(0)),
                                               "https://zenodo.org/badge/DOI/10.5281/zenodo.5646668.svg",
                                               badge_1L_chr)}
  )
  logos_chr <- purrr::map_chr(pkg_extensions_tb$pt_ns_chr,
                              ~paste0("https://ready4-dev.github.io/",.x,"/logo.png"))
  homepages_chr <- pkg_extensions_tb$Link
  pkg_extensions_tb <- pkg_extensions_tb %>%
    dplyr::mutate(Purpose = Title %>%
                    purrr::map2_chr(pt_ns_chr,
                                    ~ stringr::str_remove(.x,paste0(.y,": ")))) %>%
    dplyr::rename(Package = Logo,
                  Website = Link,
                  Examples = Vignettes_URLs)
  pkg_extensions_tb <- pkg_extensions_tb %>%
    dplyr::mutate(Examples = purrr::map(Examples,
                                        ~ if(is.na(.x[1])){
                                            ""
                                          }else{
                                            .x
                                          }
                                        )) %>%
    dplyr::mutate(Documentation = purrr::pmap(list(manual_urls_ls,
                                              Citation, Website),
                                             ~ {
                                               if(identical(..1,
                                                            character(0)) | is.na(..1[1]) | length(..1) != 2){
                                                 manual_txt_chr <- character(0)
                                               }else{
                                                 manual_txt_chr <- c("Manual - Short (PDF)",
                                                                     "Manual - Full (PDF)")

                                               }
                                               kableExtra::cell_spec(c("Citation",
                                                                       "Website",
                                                                       manual_txt_chr),
                                                                     "html",
                                                                     link = c(..2,..3,..1))

                                             })) %>%
    # dplyr::mutate(Manuals = purrr::map(manual_urls_ls,
    #                                     ~ {
    #                                          if(identical(.x,
    #                                                       character(0)) | is.na(.x[1]) | length(.x) != 2){
    #                                            ""
    #                                            }else{
    #                                              kableExtra::cell_spec(c("Modeller (PDF)",
    #                                                                      "Developer (PDF)"),
    #                                                                    "html",
    #                                                                    link = .x)
    #                                                                         }
    #
    #                                                                       })) %>%
    dplyr::mutate(Code = purrr::map(code_urls_ls,
                                           ~ {
                                             if(is.na(.x[1])){
                                               ""
                                             }else{
                                               kableExtra::cell_spec(c("Dev", "Archive"),
                                                                     "html",
                                                                     link = .x)
                                             }

                                           })) %>%
    dplyr::select(Type, Package, Purpose, Documentation,
                  #Authors, DOI, Website, Manuals,
                  Code, Examples)
  pkg_extensions_kbl <- pkg_extensions_tb %>%
    # kableExtra::kbl(booktabs = T) %>%
    # kableExtra::kable_paper(full_width = F) %>%
    kableExtra::kable("html", escape = FALSE) %>%
    kableExtra::kable_styling(bootstrap_options = c("hover", "condensed")) %>%
    kableExtra::column_spec(which(names(pkg_extensions_tb)=="Type"),
                            image = ready4_badges_chr) %>%
    kableExtra::column_spec(which(names(pkg_extensions_tb)=="DOI"),
                            image = zenodo_badges_chr) %>%
    kableExtra::column_spec(which(names(pkg_extensions_tb)=="Package"),
                            image = kableExtra::spec_image(logos_chr,
                                                           height = 160,
                                                           width = 160)) %>%
    kableExtra::column_spec(which(names(pkg_extensions_tb)=="Website"),
                            link = homepages_chr)
  return(pkg_extensions_kbl)
}
print_vignettes <- function(pkg_extensions_tb = NULL,
                            include_1L_chr = "modules"){
  if(is.null(pkg_extensions_tb))
    pkg_extensions_tb <- make_libraries_tb(include_1L_chr = include_1L_chr)
  vignettes_chr <- pkg_extensions_tb$Vignettes %>% purrr::flatten_chr()
  keep_lgl <- !is.na(vignettes_chr)
  vignettes_tb <- tibble::tibble(HTML = vignettes_chr[keep_lgl]#,
                                 # HTML = pkg_extensions_tb$Examples %>%
                                 #   purrr::flatten_chr() %>%
                                 #   purrr::keep(keep_lgl)
                                 ) %>%
    dplyr::mutate(Title = purrr::map_chr(HTML,
                                         ~ rvest::read_html(.x) %>%
                    rvest::html_elements("h1")  %>%
                    rvest::html_text2()),
                  RMD = purrr::map_chr(HTML,
                                       ~ rvest::read_html(.x) %>%
                    rvest::html_elements(".dont-index")  %>%
                      rvest::html_elements("a") %>%
                      rvest::html_attr("href"))) %>%
    dplyr::mutate(Program = purrr::map2(HTML, RMD,
                                        ~kableExtra::cell_spec(c("HTML",
                                                                 "RMD"),
                                                               "html",
                                                               link = c(.x,.y))))
  vignettes_kbl <- vignettes_tb %>%
    dplyr::select(Title, Program) %>%
    kableExtra::kable("html", escape = FALSE) %>%
    kableExtra::kable_styling(bootstrap_options = c("hover", "condensed"))
  return(vignettes_kbl)
}

