get_badges_lup <- function(gh_repo_1L_chr = "ready4-dev/ready4",
                           gh_tag_1L_chr = "Documentation_0.0"){
  dmt_urls_chr <- piggyback::pb_download_url(repo = gh_repo_1L_chr,
                                             tag = gh_tag_1L_chr,
                                             .token = "")
  ready4_badges_lup <- readRDS(url(dmt_urls_chr[dmt_urls_chr %>%
                                                  endsWith("ready4_badges_lup.RDS")]))
  return(ready4_badges_lup)
}
get_badge_urls <- function(pkg_nm_1L_chr){
  images_chr <- rvest::read_html(paste0("https://ready4-dev.github.io/",pkg_nm_1L_chr,"/index.html")) %>%
    rvest::html_elements("img")  %>%
    rvest::html_attr("src")
  badge_urls_ls <- list(ready4_1L_chr = images_chr[images_chr %>% startsWith("https://img.shields.io/badge/ready4")],
                        zenodo_1L_chr = images_chr[images_chr %>% startsWith("https://zenodo.org/badge/DOI/")])
  return(badge_urls_ls)
}
get_cls_extensions <- function(pkg_extensions_tb,
                               gh_repo_1L_chr = "ready4-dev/ready4",
                               gh_tag_1L_chr = "Documentation_0.0"){
  dmt_urls_chr <- piggyback::pb_download_url(repo = gh_repo_1L_chr,
                                             tag = gh_tag_1L_chr,
                                             .token = "")
  cls_extensions_tb <- readRDS(url(dmt_urls_chr[dmt_urls_chr %>%
                                                  endsWith("classes_lup.RDS")])) %>%
    tibble::as_tibble() %>%
    dplyr::arrange(pt_ns_chr) %>%
    dplyr::filter(pt_ns_chr %in% pkg_extensions_tb$pt_ns_chr) %>%
    dplyr::arrange(pt_ns_chr) %>%
    dplyr::select(type_chr, pt_ns_chr, old_class_lgl)
  return(cls_extensions_tb)
}
get_dv_fls_urls <- function(file_nms_chr,
                            dv_ds_nm_1L_chr,
                            dv_url_pfx_1L_chr = character(0),
                            server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
                            key_1L_chr = NULL){
  if(identical(dv_url_pfx_1L_chr, character(0)))
    dv_url_pfx_1L_chr <- paste0("https://",
                                server_1L_chr,
                                "/api/access/datafile/")
  ds_ls <- dataverse::dataset_files(dv_ds_nm_1L_chr,
                                    server = server_1L_chr,
                                    key = key_1L_chr)
  all_items_chr <- purrr::map_chr(ds_ls,~.x$label)
  urls_chr <- file_nms_chr %>%
    purrr::map_chr(~{
      idx_1L_int <- which(all_items_chr == .x)
      if(identical(idx_1L_int, integer(0))){
        NA_character_
      }else{
        paste0(dv_url_pfx_1L_chr,ds_ls[[idx_1L_int]]$dataFile$id)
      }
    })
  return(urls_chr)
}
get_dvs <- function(dv_nm_1L_chr = "ready4",
                    key_1L_chr = NULL,
                    server_1L_chr = "dataverse.harvard.edu"){
  contents_ls <- dataverse::dataverse_contents(dv_nm_1L_chr,
                                         key = key_1L_chr,
                                         server = server_1L_chr)
  dv_ls <- contents_ls[contents_ls %>% purrr::map_lgl(~.x$type == "dataverse")]
  ds_ls <- contents_ls[contents_ls %>% purrr::map_lgl(~.x$type == "dataset")]
  if(identical(ds_ls,list())){
    ds_ls <- NULL
  }else{
    extra_dv_ls <- dataverse::get_dataverse(dv_nm_1L_chr,
                                            key = key_1L_chr,
                                            server = server_1L_chr)
    dv_ls <- append(extra_dv_ls,
                    dv_ls)
  }
  dvs_tb <- dv_ls %>%
    purrr::map_dfr(~{
      dv_ls <- dataverse::get_dataverse(.x,
                                        key = key_1L_chr,
                                        server = server_1L_chr)
      tb <- tibble::tibble(Dataverse = dv_ls$alias,
                     Name = dv_ls$name,
                     Description = dv_ls$description,
                     Creator = dv_ls$affiliation)
      tb %>%
        dplyr::mutate(Contents =  purrr::map(Dataverse,
                                             ~{
                                               dv_all_ls <- dataverse::dataverse_contents(.x,
                                                                                            key = key_1L_chr,
                                                                                            server = server_1L_chr)
                                               #dv_ls <- dv_all_ls[dv_all_ls %>% purrr::map_lgl(~.x$type == "dataverse")]
                                               dv_all_ls[dv_all_ls %>% purrr::map_lgl(~.x$type == "dataset")] %>%
                                               purrr::map_chr(~if("persistentUrl" %in% names(.x)){
                                                 .x$persistentUrl
                                               }else{
                                                 NA_character_
                                               })
                                               }))
    })
  dvs_tb <- dvs_tb %>%
    dplyr::mutate(Datasets_Meta = Contents %>%
                    purrr::map(~.x %>%
                                 purrr::map(~ .x %>%
                                              dataverse::dataset_metadata(key = key_1L_chr,
                                                                          server = server_1L_chr) %>%
                                              tryCatch(error = function(e) "ERROR")))) %>%
    dplyr::mutate(Contents = Contents %>%
                    purrr::map2(Datasets_Meta,
                                ~ {
                                  entry_ls <- .x %>%
                                    purrr::map2(.y,
                                                ~ if(identical(.y, "ERROR")){
                                                  NA_character_
                                                  }else{
                                                    .x
                                                    }
                                                ) %>%
                                  purrr::discard(is.na)
                                if(identical(entry_ls, list())){
                                  NA_character_
                                }else{
                                  entry_ls  %>%
                                    purrr::flatten_chr()
                                }
                                }
                                ))
  dvs_tb <- dvs_tb %>%
    dplyr::mutate(Datasets_Meta = Datasets_Meta %>%
                    purrr::map(~ {
                                  entry_ls <- .x %>%
                                    purrr::map(~ if(identical(.x, "ERROR")){
                                                  NULL
                                                }else{
                                                  .x
                                                }
                                    ) %>%
                                    purrr::compact()
                                  if(identical(entry_ls, list())){
                                    NULL
                                  }else{
                                    entry_ls
                                  }
                                }
                    )) %>%
    dplyr::arrange(Dataverse)
  return(dvs_tb)
}
get_examples <- function(vignettes_chr,
                         term_1L_chr){
  if(is.na(vignettes_chr[1])){
    examples_chr <- ""
  }else{
    examples_chr <- vignettes_chr %>%
      purrr::map_chr(~{
        code_chr <- rvest::read_html((.x %>%
                                        stringr::str_match("href=\"\\s*(.*?)\\s*\" style"))[,2]) %>%
          rvest::html_elements(".r") %>%
          rvest::html_text2()
        ifelse(stringr::str_detect(paste0(code_chr, collapse = "/n"),
                                   paste0(term_1L_chr,"\\(")),
               .x,
               NA_character_)

      }) %>%
      purrr::discard(is.na)
    if(identical(character(0),examples_chr))
      examples_chr <- ""
  }
  return(examples_chr)
}
get_fl_id_from_dv_ls <-  function (ds_ls, fl_nm_1L_chr, nms_chr = NA_character_)
{
  if (is.na(nms_chr[1])) {
    nms_chr <- purrr::map_chr(ds_ls$files$filename, ~ifelse(is.na(.x), .y, .x))
  }
  if (fl_nm_1L_chr %in% nms_chr) {
    id_1L_chr <- get_from_lup_obj(ds_ls$files[, names(ds_ls$files) %>% unique()] %>%
                                            tibble::as_tibble(),
                                          match_var_nm_1L_chr = "filename",
                                          match_value_xx = fl_nm_1L_chr,
                                          target_var_nm_1L_chr = "id",
                                          evaluate_1L_lgl = F)
  }
  else {
    id_1L_chr <- NA_character_
  }
  return(id_1L_chr)
}
# get_fn_descs <- function(fn_nms_chr = NULL,
#                          functions_tb = NULL,
#                          gh_repo_1L_chr = "ready4-dev/ready4",
#                          gh_tag_1L_chr = "Documentation_0.0",
#                          return_1L_chr = "all"){
#   if(is.null(functions_tb))
#     functions_tb <- get_functions_tb(gh_repo_1L_chr = gh_repo_1L_chr,
#                              gh_tag_1L_chr = gh_tag_1L_chr,
#                              return_1L_chr = return_1L_chr)
#   if(is.null(fn_nms_chr)){
#         fn_descs_chr <- functions_tb$fn_type_desc_chr
#   }else{
#       fn_descs_chr <- purrr::map_chr(fn_nms_chr,
#                                  ~get_from_lup_obj(functions_tb,
#                                                    match_value_xx = .x,
#                                                    match_var_nm_1L_chr = "fn_type_nm_chr",
#                                                    target_var_nm_1L_chr = "fn_type_desc_chr"))
#   }
#   return(fn_descs_chr)
# }
get_from_lup_obj <- function(data_lookup_tb,
                             match_value_xx,
                             match_var_nm_1L_chr,
                             target_var_nm_1L_chr,
                             evaluate_1L_lgl = FALSE){
  return_object_ref <- data_lookup_tb %>%
    dplyr::filter(!!rlang::sym(match_var_nm_1L_chr)==match_value_xx) %>%
    dplyr::select(!!target_var_nm_1L_chr) %>%
    dplyr::pull()
  if(evaluate_1L_lgl){
    if(stringr::str_detect(return_object_ref,"::")){
      colon_positions <- stringr::str_locate(return_object_ref,
                                             "::")
      namespace_ref <- stringr::str_sub(return_object_ref,
                                        start=1,
                                        end=colon_positions[1,"start"]-1)
      object_ref <- stringr::str_sub(return_object_ref,
                                     start=colon_positions[1,"end"]+1)

      if(sum(stringr::str_detect(search(),paste0("package:",
                                                 namespace_ref))) == 0){
        namespace_ref_sym <- rlang::sym(namespace_ref)
        attachNamespace(namespace_ref)
        return_object_xx <- get(x = object_ref,
                                envir = as.environment(paste0("package:",
                                                              namespace_ref)))
        detach(paste0("package:",
                      namespace_ref),
               character.only = TRUE)
      }else{
        return_object_xx <- get(x = object_ref,
                                envir = as.environment(paste0("package:",
                                                              namespace_ref)))
      }
    }else{
      return_object_xx <- get(x = return_object_ref)
    }
  }else{
    return_object_xx <- return_object_ref
  }
  return(return_object_xx)
}
get_functions_tb <- function(gh_repo_1L_chr = "ready4-dev/ready4",
                             gh_tag_1L_chr = "Documentation_0.0",
                             return_1L_chr = "all"){
  dmt_urls_chr <- piggyback::pb_download_url(repo = gh_repo_1L_chr,
                                             tag = gh_tag_1L_chr,
                                             .token = "")
  functions_tb <- readRDS(url(dmt_urls_chr[dmt_urls_chr %>%
                                             endsWith("fn_types_lup.RDS")]))
  if(return_1L_chr == "methods"){
    functions_tb <- functions_tb %>%
      dplyr::filter(is_method_lgl)
  }
  if(return_1L_chr == "types"){
    functions_tb <- functions_tb %>%
      dplyr::filter(!is_method_lgl)
  }
  functions_tb <- functions_tb %>%
    dplyr::select(fn_type_nm_chr,fn_type_desc_chr)
  return(functions_tb)
}
get_generics <- function(pkg_nm_1L_chr = "ready4",
                         return_1L_lgl = "all",
                         exclude_mthds_for_chr = NA_character_
){
  generics_chr <- methods::getGenerics(paste0("package:",pkg_nm_1L_chr))@.Data
  generics_chr <- generics_chr[generics_chr %>%
                                 purrr::map_lgl(~{
                                   generic_1L_chr <- .x
                                   any(letters %>% purrr::map_lgl(~startsWith(generic_1L_chr,.x)))
                                 })]
  if(!is.na(exclude_mthds_for_chr[1])){
    generics_chr <- setdiff(generics_chr,
                            purrr::map(exclude_mthds_for_chr,
                                       ~ get_methods(pkg_nm_1L_chr = pkg_nm_1L_chr,
                                                     cls_nm_1L_chr = .x)) %>%
                              purrr::flatten_chr() %>%
                              unique())
  }
  if(return_1L_lgl == "core"){
    generics_chr <- generics_chr[tolower(generics_chr)==generics_chr]
  }
  if(return_1L_lgl == "extended")
    generics_chr <- generics_chr[tolower(generics_chr)!=generics_chr]
  if(return_1L_lgl == "slot"){
    generics_chr <- generics_chr[endsWith(generics_chr,"Slot")]
  }
  return(generics_chr)
}
get_manual_urls <- function(pkg_nm_1L_chr = "ready4",
                            pkg_url_1L_chr = "https://ready4-dev.github.io/ready4/index.html"){
  urls_chr <- rvest::read_html(pkg_url_1L_chr) %>%
    rvest::html_elements(".external-link") %>%
    rvest::html_attr("href")
  idcs_int <- urls_chr %>%
    purrr::map_lgl(~endsWith(.x,
                             paste0(pkg_nm_1L_chr,"_User.pdf"))|endsWith(.x,
                                                                         paste0(pkg_nm_1L_chr,"_Developer.pdf"))) %>%
    which()
  urls_chr <- sort(urls_chr[idcs_int], decreasing = T)
  return(urls_chr)
}
get_methods <- function(pkg_nm_1L_chr = "ready4",
                        cls_nm_1L_chr =  "Ready4Module"){
  methods_chr <- showMethods(class="Ready4Module", printTo =FALSE )
  methods_chr <- methods_chr[which(methods_chr %>% stringr::str_detect(cls_nm_1L_chr))-1]
  methods_chr <- methods_chr[which(methods_chr%>% stringr::str_detect(paste0("package ",pkg_nm_1L_chr)))] %>%
    stringr::str_remove_all("Function: ") %>%
    stringr::str_remove_all(paste0(" \\(package ",pkg_nm_1L_chr,"\\)"))
  return(methods_chr)
}
get_mthd_titles <- function(mthd_nms_chr, #NEED TO DEPRECATE IN READY4FUN
                            pkg_nm_1L_chr = "ready4"){
  mthd_titles_chr <-  mthd_nms_chr %>%
    purrr::map_chr(~{
      mthd_nm_1L_chr <- .x
      df <- mthd_nm_1L_chr %>% stringr::str_locate("\\.")
      if(!is.na(df[[1,1]])){
        mthd_nm_1L_chr <- stringr::str_sub(mthd_nm_1L_chr,
                                           end = (df[[1,1]]-1))
      }
      gnrc_dmt_ls <- tools::Rd_db(pkg_nm_1L_chr) %>%
        purrr::pluck(paste0(mthd_nm_1L_chr,"-methods.Rd"))
      ifelse(!is.null(gnrc_dmt_ls),
             gnrc_dmt_ls %>%
               purrr::pluck(1) %>%
               purrr::pluck(1) %>%
               as.vector(),
             mthd_nm_1L_chr)
    })
  return(mthd_titles_chr)
}
get_r4_obj_slots <- function(fn_name_1L_chr,
                             package_1L_chr = ""){
  slots_ls <- methods::className(fn_name_1L_chr,
                                 ifelse(package_1L_chr=="",".GlobalEnv",package_1L_chr)) %>%
    methods::getSlots()
  slots_chr <- purrr::map_chr(slots_ls, ~ .x)
  return(slots_chr)
}
get_rds_from_dv <- function(file_nm_1L_chr,
                            dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9",
                            dv_url_pfx_1L_chr = character(0),
                            key_1L_chr = NULL,
                            server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  if(identical(dv_url_pfx_1L_chr, character(0)))
    dv_url_pfx_1L_chr <- paste0("https://",
                                server_1L_chr,
                                "/api/access/datafile/")
  ds_ls <- dataverse::dataset_files(dv_ds_nm_1L_chr,
                                    server = server_1L_chr,
                                    key = key_1L_chr)
  all_items_chr <- purrr::map_chr(ds_ls,~.x$label) %>%
    stringi::stri_replace_last_regex("\\.RDS","") %>%
    stringi::stri_replace_last_regex("\\.Rds","") %>%
    stringi::stri_replace_last_regex("\\.rds","")
  idx_1L_int <- which(all_items_chr == file_nm_1L_chr) #paste0(file_nm_1L_chr,".RDS")
  if(identical(idx_1L_int, integer(0))){
    r_object_xx <- NULL
  }else{
    r_object_xx <- readRDS(url(paste0(dv_url_pfx_1L_chr,
                                      ds_ls[[idx_1L_int]]$dataFile$id)))
  }
  return(r_object_xx)
}
get_source_code_urls <- function(pkg_nm_1L_chr = "ready4",
                                 pkg_url_1L_chr = "https://ready4-dev.github.io/ready4/index.html"){
  urls_chr <- rvest::read_html(pkg_url_1L_chr) %>%
    rvest::html_elements(".external-link") %>%
    rvest::html_attr("href")
  idcs_int <- c(which((rvest::read_html(pkg_url_1L_chr) %>%
                         rvest::html_elements(".external-link") %>%
                         rvest::html_text()) == "Browse source code"),
                which(startsWith(urls_chr,
                                 "https://doi.org/10.5281/zenodo.")))
  urls_chr <- urls_chr[idcs_int]
  return(urls_chr)
}
