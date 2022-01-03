## -----------------------------------------------------------------------------
library("ready4") 

## ----echo = F-----------------------------------------------------------------
# get_badges_lup <- function(gh_repo_1L_chr = "ready4-dev/ready4",
#                                gh_tag_1L_chr = "Documentation_0.0"){
#     dmt_urls_chr <- piggyback::pb_download_url(repo = gh_repo_1L_chr,
#                                              tag = gh_tag_1L_chr,
#                                              .token = "")
# ready4_badges_lup <- readRDS(url(dmt_urls_chr[dmt_urls_chr %>% 
#                                 endsWith("ready4_badges_lup.RDS")])) 
# return(ready4_badges_lup)
# }
# get_badge_urls <- function(pkg_nm_1L_chr){
#   images_chr <- rvest::read_html(paste0("https://ready4-dev.github.io/",pkg_nm_1L_chr,"/index.html")) %>%
# rvest::html_elements("img")  %>% 
# rvest::html_attr("src")
# badge_urls_ls <- list(ready4_1L_chr = images_chr[images_chr %>% startsWith("https://img.shields.io/badge/ready4")],
#                   zenodo_1L_chr = images_chr[images_chr %>% startsWith("https://zenodo.org/badge/DOI/")])
# return(badge_urls_ls)
# }
# get_cls_extensions <- function(pkg_extensions_tb,
#                                gh_repo_1L_chr = "ready4-dev/ready4",
#                                gh_tag_1L_chr = "Documentation_0.0"){
#   dmt_urls_chr <- piggyback::pb_download_url(repo = gh_repo_1L_chr,
#                                              tag = gh_tag_1L_chr,
#                                              .token = "")
# cls_extensions_tb <- readRDS(url(dmt_urls_chr[dmt_urls_chr %>% 
#                                 endsWith("prototype_lup.RDS")])) %>%
#   tibble::as_tibble() %>%
#   dplyr::arrange(pt_ns_chr) %>%
#   dplyr::filter(pt_ns_chr %in% pkg_extensions_tb$pt_ns_chr) %>%
#   dplyr::arrange(pt_ns_chr) %>%
#     dplyr::select(type_chr, pt_ns_chr, old_class_lgl)
# return(cls_extensions_tb)
# }
# get_examples <- function(vignettes_chr,
#                         term_1L_chr){
#   if(is.na(vignettes_chr[1])){
#   examples_chr <- ""
# }else{
#   examples_chr <- vignettes_chr %>%
#     purrr::map_chr(~{
#      code_chr <- rvest::read_html((.x %>% 
#                                      stringr::str_match("href=\"\\s*(.*?)\\s*\" style"))[,2]) %>%
#        rvest::html_elements(".r") %>% 
#        rvest::html_text2()
#      ifelse(stringr::str_detect(paste0(code_chr, collapse = "/n"),
#                                 paste0(term_1L_chr,"\\(")),
#             .x,
#             NA_character_)
#      
#     }) %>%
#     purrr::discard(is.na)
#   if(identical(character(0),examples_chr))
#     examples_chr <- ""
# }
#   return(examples_chr)
# }
# make_modules_tb <- function(pkg_extensions_tb = NULL,
#                             cls_extensions_tb = NULL,
#                             gh_repo_1L_chr = "ready4-dev/ready4",
#                             gh_tag_1L_chr = "Documentation_0.0"){
#   if(is.null(pkg_extensions_tb))
#     pkg_extensions_tb <- make_pkg_extensions_tb()
#   if(is.null(cls_extensions_tb))
#     cls_extensions_tb <- get_cls_extensions(pkg_extensions_tb,
#                                             gh_repo_1L_chr = gh_repo_1L_chr,
#                                             gh_tag_1L_chr = gh_tag_1L_chr)
#   modules_tb <- dplyr::inner_join(cls_extensions_tb,
#                         pkg_extensions_tb) %>%
#     dplyr::mutate(Class = purrr::pmap(list(pt_ns_chr,
#                                            type_chr,
#                                            old_class_lgl),
#                                             
#                                       ~ {
#                                                                                   kableExtra::cell_spec(..2, 
#                                                               "html", 
#                                                               link = paste0("https://ready4-dev.github.io/",
#                                                                             ..1,
#                                                                             "/reference/",
#                                                                             ifelse(..3,
#                                                                                    ..2,
#                                                                                    paste0(..2,"-class")),
#                                                                             ".html"
#                                                                             ))
#                                         
# 
#                                         }
#                                       )) %>%
#   dplyr::mutate(Examples = purrr::map2(Vignette_URLs,
#                                    type_chr,
#                                   ~ get_examples(.x,
#                         term_1L_chr = .y))) %>%
#   dplyr::mutate(Description = purrr::map2_chr(Class,
#                                               old_class_lgl,
#                                               ~{
#                                                 rvest::read_html((.x %>% 
#                                                                   stringr::str_match("href=\"\\s*(.*?)\\s*\" style"))[,2]) %>%
#        rvest::html_elements(ifelse(.y,"h1","p")) %>% 
#        rvest::html_text2() %>%
#          purrr::pluck(1)
# 
#     }) %>% stringi::stri_replace_last_regex("\\.","")) %>%
#   # dplyr::rename(Class = type_chr) %>%
#   dplyr::select(Class,Description,Library,
#                 Examples,old_class_lgl)
#   return(modules_tb)
#                             }
# make_pkg_extensions_tb <- function(){
#   pkg_extensions_tb <- tibble::tibble(pt_ns_chr = c("scorz","specific","TTU", "youthvars","ready4show","ready4use","ready4fun","ready4class","ready4pack","youthu")) %>%
#   dplyr::mutate(Purpose = dplyr::case_when(pt_ns_chr == "ready4class" ~ "Authoring (code - classes)",
#                                            pt_ns_chr == "ready4fun" ~ "Authoring (code - functions)",
#                                            pt_ns_chr == "ready4pack" ~ "Authoring (code - libraries)",
#                                            pt_ns_chr == "ready4show" ~ "Authoring (code - programs)",
#                                            pt_ns_chr == "ready4use" ~ "Authoring (datasets)",
#                                            pt_ns_chr == "youthvars" ~ "Description (datasets)",
#                                            pt_ns_chr == "scorz" ~ "Description (variable scoring)",
#                                            pt_ns_chr == "specific" ~ "Modelling (inverse problems)",
#                                            pt_ns_chr == "heterodox" ~ "Modelling (heterogeneity)",
#                                            pt_ns_chr == "TTU" ~ "Modelling (health utility)",
#                                            pt_ns_chr == "youthu" ~ "Prediction (health utility)",
#                                            T ~ "")) %>%
#     dplyr::arrange(Purpose) %>%
#   dplyr::mutate(Link = purrr::map_chr(pt_ns_chr,
#                                       ~ paste0("https://ready4-dev.github.io/",.x,
#                                                "/index",#"/articles/",.x,
#                                                ".html"))) %>%
#   dplyr::mutate(Library = kableExtra::cell_spec(pt_ns_chr, "html", link = Link)) %>%
#   dplyr::mutate(Vignettes = purrr::map(pt_ns_chr,~as.vector(browseVignettes(.x)[[.x]][,7]) %>% purrr::keep(~startsWith(.x,"V_")) %>% sort()))
# examples_1L_int <- pkg_extensions_tb$Vignettes %>% 
#   purrr::compact() %>% 
#   purrr::flatten_chr() %>% 
#   length()
# pkg_extensions_tb <- pkg_extensions_tb %>%
#   dplyr::mutate(Reference = Vignettes %>% 
#   purrr::reduce(.init = list(ref_int = c(0,0),
#                              content_ls = NULL),
#                 ~{
#                   if(is.null(.y)|identical(.y, character(0))){
#                     .x$content_ls <- append(.x$content_ls,list(NA_integer_))
#                     }else{
#                       .x$ref_int <- .x$ref_int + 1:length(.y)
#                       .x$content_ls <- append(.x$content_ls,
#                                               list((1:examples_1L_int)[.x$ref_int]))
#                       }
#                   .x
#                   }) %>%
#     purrr::pluck(2)) %>%
#   dplyr::mutate(Vignette_URLs = purrr::pmap(list(pt_ns_chr,
#                                                  Vignettes,
#                                                  Reference),
#                                             
#                                       ~ {
#                                         if(is.na(..3[1])){
#                                           NA_character_
#                                         }else{
#                                                                                   kableExtra::cell_spec(..3, 
#                                                               "html", 
#                                                               link = paste0("https://ready4-dev.github.io/",..1,"/articles/",..2
#                                                                             ))
#                                         }
# 
#                                         }
#                                       ))
# y_tb <- purrr::map_dfr(pkg_extensions_tb$pt_ns_chr,
#                ~ {
#                  if(!.x %in% c("TTU","youthu")){
#                                     f <- tempfile(fileext = ".bib")
#                  sink(f)
#                  writeLines(rvest::read_html(paste0("https://ready4-dev.github.io/",.x,"/authors.html")) %>%
#                    rvest::html_elements("pre") %>%
#                    rvest::html_text2())
#                  sink(NULL)
#                  bib2df::bib2df(f) %>%
#                    dplyr::select(AUTHOR, TITLE, DOI)
#                  }else{
#                    if(.x == "TTU"){
#                                         tibble::tibble(AUTHOR = list(c("Caroline Gao","Matthew Hamilton")),
#                               TITLE = "TTU: Specify, Report and Share Transfer to Utility Mapping Algorithms",
#                               DOI = "10.5281/zenodo.5646593")
#                    }else{
#                                                              tibble::tibble(AUTHOR = list(c("Matthew Hamilton","Caroline Gao")),
#                               TITLE = "youthu: Map Youth Outcomes to Health Utility",
#                               DOI = "10.5281/zenodo.5646668")
#                    }
# 
#                  }
#                }) %>% dplyr::mutate(pt_ns_chr = pkg_extensions_tb$pt_ns_chr) %>%
#   dplyr::rename(DOI_chr = DOI,
#                 Package = TITLE,
#                 Authors = AUTHOR)
# pkg_extensions_tb <- dplyr::left_join(pkg_extensions_tb,y_tb) 
# # %>%
# #     dplyr::mutate(TITLE = purrr::map2_chr(TITLE,
# #                                         pt_ns_chr,
# #                                         ~stringr::str_replace(.x,
# #                                                               paste0(.y,": "),
# #                                                               ""))) 
#   return(pkg_extensions_tb)
# }
# print_modules <- function(modules_tb,
#                           what_1L_chr = "All"){
#   if(what_1L_chr == "S4"){
#     modules_tb <- modules_tb %>%
#   dplyr::filter(!old_class_lgl)  
#   }
#     if(what_1L_chr == "S3"){
#     modules_tb <- modules_tb %>%
#   dplyr::filter(old_class_lgl) 
#     }
#   
#  modules_kbl <- modules_tb %>%
#   dplyr::select(-old_class_lgl,
#                 -Library)  %>%
#   kableExtra::kable("html", escape = FALSE) %>%
#   kableExtra::kable_styling(bootstrap_options = c("hover", "condensed"))
#  return(modules_kbl)
#                           }
# print_pkg_extensions <- function(pkg_extensions_tb = NULL){
#   if(is.null(pkg_extensions_tb))
#     pkg_extensions_tb <- make_pkg_extensions_tb()
#   pkg_extensions_tb <- pkg_extensions_tb %>%
#   dplyr::mutate(Badges = purrr::map(pt_ns_chr,
#                                     ~ get_badge_urls(.x))) %>%
#     dplyr::mutate(Type = "") %>%
#   dplyr::mutate(DOI = "") %>%
#   dplyr::mutate(Logo = "") 
# ready4_badges_chr <- purrr::map_chr(pkg_extensions_tb$Badges,
#                                     ~ .x$ready4_1L_chr)
# zenodo_badges_chr <- purrr::map_chr(pkg_extensions_tb$Badges,
#                                     ~ {
#                                       badge_1L_chr <-.x$zenodo_1L_chr
#                                       ifelse(identical(badge_1L_chr, character(0)),
#                                              "https://zenodo.org/badge/DOI/10.5281/zenodo.5646668.svg",
#                                              badge_1L_chr)}
#                                     )
# logos_chr <- purrr::map_chr(pkg_extensions_tb$pt_ns_chr,
#                             ~paste0("https://ready4-dev.github.io/",.x,"/logo.png"))
# homepages_chr <- pkg_extensions_tb$Link
# pkg_extensions_tb <- pkg_extensions_tb %>%
#     dplyr::select(Type, Logo, Package, Authors, DOI) 
# pkg_extensions_kbl <- pkg_extensions_tb %>%
#   kableExtra::kbl(booktabs = T) %>%
#   kableExtra::kable_paper(full_width = F) %>%
#   kableExtra::column_spec(which(names(pkg_extensions_tb)=="Type"),
#                           image = ready4_badges_chr) %>%
#   kableExtra::column_spec(which(names(pkg_extensions_tb)=="DOI"),
#                           image = zenodo_badges_chr) %>%
#   kableExtra::column_spec(which(names(pkg_extensions_tb)=="Logo"),
#                           image = kableExtra::spec_image(logos_chr,
#                                                          height = 100,
#                                                          width = 100)) %>%
#     kableExtra::column_spec(which(names(pkg_extensions_tb)=="Package"),
#                           link = homepages_chr)
# return(pkg_extensions_kbl)
# }

## ----warning=FALSE, message=FALSE---------------------------------------------
x <- make_pkg_extensions_tb()

## ----warning=FALSE, message=FALSE---------------------------------------------
print_pkg_extensions(x)

