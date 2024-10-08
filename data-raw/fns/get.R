get_badge_urls <- function(pkg_nm_1L_chr,
                           project_badges_url_1L_chr = "https://img.shields.io/badge/ready4",
                           url_stub_1L_chr = "https://ready4-dev.github.io/"){
  images_xx <- get_gracefully(paste0(url_stub_1L_chr, pkg_nm_1L_chr, "/index.html"), fn=rvest::read_html)
  if(!is.null(images_xx)){
    images_chr <- images_xx %>%
      rvest::html_elements("img") %>%
      rvest::html_attr("src")
    badge_urls_ls <- list(ready4_1L_chr = images_chr[images_chr %>% startsWith(project_badges_url_1L_chr)],
                          zenodo_1L_chr = images_chr[images_chr %>% startsWith("https://zenodo.org/badge/DOI/")])
    if(identical(badge_urls_ls$ready4_1L_chr, character(0))){
      badge_urls_ls <- get_badge_urls(paste0(pkg_nm_1L_chr, "/dev"), project_badges_url_1L_chr = project_badges_url_1L_chr, url_stub_1L_chr = url_stub_1L_chr)
    }
  }else{
    badge_urls_ls <- NULL
  }
  return(badge_urls_ls)
}
get_badges_lup <- function(ends_with_1L_chr = "ready4_badges_lup.RDS",
                           gh_repo_1L_chr = "ready4-dev/ready4",
                           gh_tag_1L_chr = "Documentation_0.0"){
  ready4_badges_lup <- NULL
  dmt_urls_xx <- get_gracefully(NULL, fn = piggyback::pb_download_url,
                                args_ls = list(repo = gh_repo_1L_chr, tag = gh_tag_1L_chr, .token = ""))
  if(!is.null(dmt_urls_xx)){
    dmt_urls_chr <- dmt_urls_xx
    ready4_badges_lup <- get_gracefully(dmt_urls_chr[dmt_urls_chr %>%
                                                       endsWith(ends_with_1L_chr)])
  }
  return(ready4_badges_lup)
}
get_cls_extensions <- function(pkg_extensions_tb,
                               gh_repo_1L_chr = "ready4-dev/ready4",
                               gh_tag_1L_chr = "Documentation_0.0",
                               url_stub_1L_chr = "https://ready4-dev.github.io/",
                               validate_1L_lgl = FALSE){
  cls_extensions_tb <- NULL
  dmt_urls_xx <- get_gracefully(NULL, fn = piggyback::pb_download_url,
                                args_ls = list(repo = gh_repo_1L_chr, tag = gh_tag_1L_chr, .token = ""))
  if(!is.null(dmt_urls_xx)){
    dmt_urls_chr <- dmt_urls_xx
    cls_extensions_tb <- get_gracefully(dmt_urls_chr[dmt_urls_chr %>%
                                                       endsWith("classes_lup.RDS")])
  }
  if(!is.null(cls_extensions_tb)){
    cls_extensions_tb <- cls_extensions_tb %>%
      tibble::as_tibble() %>%
      dplyr::arrange(.data$pt_ns_chr) %>%
      dplyr::filter(.data$pt_ns_chr %in% pkg_extensions_tb$pt_ns_chr) %>%
      dplyr::arrange(.data$pt_ns_chr) %>%
      dplyr::select("type_chr", "pt_ns_chr", "old_class_lgl")
    if(validate_1L_lgl){
      cls_extensions_tb <- cls_extensions_tb$pt_ns_chr %>% unique() %>%
        purrr::map_dfr(~{
          url_1L_chr <- paste0(url_stub_1L_chr,
                               .x,
                               "/reference/index",
                               ".html")
          allowable_xx <- get_gracefully(url_1L_chr, fn=rvest::read_html)
          if(!is.null(allowable_xx)){
            allowable_chr <- allowable_xx %>%
              rvest::html_elements("a") %>%
              rvest::html_text2()
            allowable_chr <- allowable_chr[(allowable_chr %>% endsWith("-class") | allowable_chr %>% startsWith(.x)) & allowable_chr != .x] %>%
              stringi::stri_replace_last_fixed("-class","") %>%
              stringi::stri_replace_last_fixed("()","")
            cls_extensions_tb %>%
              dplyr::filter(.data$pt_ns_chr == .x) %>%
              dplyr::filter(.data$type_chr %in% allowable_chr)
          }else{ # FORCE NULL if nrow()==0 ????
            NULL
          }
        })
    }
    if(nrow(cls_extensions_tb)==0){
      cls_extensions_tb <- NULL
    }
  }
  return(cls_extensions_tb)
}
get_datasets_tb <- function(gh_repo_1L_chr = "ready4-dev/ready4",
                            gh_tag_1L_chr = "Documentation_0.0",
                            rds_fl_name_1L_chr = "datasets_tb"){
  datasets_tb <- NULL
  dmt_urls_xx <- get_gracefully(NULL, fn = piggyback::pb_download_url,
                                args_ls = list(repo = gh_repo_1L_chr, tag = gh_tag_1L_chr, .token = ""))
  if(!is.null(dmt_urls_xx)){
  dmt_urls_chr <- dmt_urls_xx
  datasets_tb <- get_gracefully(dmt_urls_chr[dmt_urls_chr %>% endsWith(paste0(rds_fl_name_1L_chr, ".RDS"))])
  }
  return(datasets_tb)
}
get_digits_from_text <- function(text_1L_chr){
  fn_attribution_1L_chr <- "This function is based on: http://stla.github.io/stlapblog/posts/Numextract.html"
  digits_chr <- unlist(regmatches(text_1L_chr, gregexpr("[[:digit:]]+\\.*[[:digit:]]*", text_1L_chr)))
  return(digits_chr)
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
  ds_ls <- get_gracefully(dv_ds_nm_1L_chr, fn = dataverse::dataset_files,
                          args_ls = list(key = key_1L_chr, server = server_1L_chr), not_chr_1L_lgl = TRUE)
  if(!is.null(ds_ls)){
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
  }else{
    urls_chr <- NULL
  }
  return(urls_chr)
}
get_examples <- function(vignettes_chr,
                         term_1L_chr){
  if(is.na(vignettes_chr[1])){
    examples_chr <- ""
  }else{
    examples_chr <- vignettes_chr %>%
      purrr::map_chr(~{
        code_xx <- get_gracefully((.x %>%
                                     stringr::str_match("href=\"\\s*(.*?)\\s*\" style"))[,2], fn=rvest::read_html)
        if(!is.null(code_xx)){
          code_chr <- code_xx %>%
            rvest::html_elements(".r") %>%
            rvest::html_text2()
          ifelse(stringr::str_detect(paste0(code_chr, collapse = "/n"),
                                     paste0(term_1L_chr,"\\(")),
                 .x,
                 NA_character_)
        }
      }) %>%
      purrr::discard(is.na)
    if(identical(character(0),examples_chr))
      examples_chr <- ""
  }
  return(examples_chr)
}
get_excluded_repos <- function(gh_repo_1L_chr = "ready4-dev/ready4",
                                 gh_tag_1L_chr = "Documentation_0.0"){
  exclude_chr <- NULL
  dmt_urls_xx <- get_gracefully(NULL, fn = piggyback::pb_download_url,
                                args_ls = list(repo = gh_repo_1L_chr, tag = gh_tag_1L_chr, .token = ""))
  if(!is.null(dmt_urls_xx)){
  dmt_urls_chr <- dmt_urls_xx
  if(any(dmt_urls_chr %>% endsWith("exclude_chr.RDS"))){
    exclude_chr <- get_gracefully(dmt_urls_chr[dmt_urls_chr %>% endsWith("exclude_chr.RDS")])
  }else{
    exclude_chr <- character(0)
  }
  }
  return(exclude_chr)
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
                                          evaluate_1L_lgl = FALSE)
  }
  else {
    id_1L_chr <- NA_character_
  }
  return(id_1L_chr)
}
get_fl_nm_from_path <- function (path_1L_chr) {
  acknowledgement_1L_chr <- "This function is a minor rephrasing of fs:path_file"
  missing_1L_lgl <- is.na(path_1L_chr)
  path_1L_chr[!missing_1L_lgl] <- basename(path_1L_chr[!missing_1L_lgl])
  fl_nm_1L_chr <- as.character(path_1L_chr)
  return(fl_nm_1L_chr)
}
get_fl_extension <- function (path_1L_chr) {
  acknowledgement_1L_chr <- "This function is a minor rephrasing of tools::file_ext"
  index_1L_int <- regexpr("\\.([[:alnum:]]+)$", path_1L_chr)
  extension_1L_chr<- ifelse(index_1L_int > -1L, substring(path_1L_chr, index_1L_int + 1L), "")
  return(extension_1L_chr)
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
        cell_value_xx <- get(x = object_ref,
                                envir = as.environment(paste0("package:",
                                                              namespace_ref)))
        detach(paste0("package:",
                      namespace_ref),
               character.only = TRUE)
      }else{
        cell_value_xx <- get(x = object_ref,
                                envir = as.environment(paste0("package:",
                                                              namespace_ref)))
      }
    }else{
      cell_value_xx <- get(x = return_object_ref)
    }
  }else{
    cell_value_xx <- return_object_ref
  }
  return(cell_value_xx)
}
get_functions_tb <- function(gh_repo_1L_chr = "ready4-dev/ready4",
                             gh_tag_1L_chr = "Documentation_0.0",
                             return_1L_chr = "all"){
    functions_tb <- NULL
   dmt_urls_xx <- get_gracefully(NULL, fn = piggyback::pb_download_url,
                                args_ls = list(repo = gh_repo_1L_chr, tag = gh_tag_1L_chr, .token = ""))
  if(!is.null(dmt_urls_xx)){
  dmt_urls_chr <- dmt_urls_xx
  functions_tb <- get_gracefully(dmt_urls_chr[dmt_urls_chr %>%
                                             endsWith("fn_types_lup.RDS")])
  if(!is.null(functions_tb)){
    if(return_1L_chr == "methods"){
      functions_tb <- functions_tb %>%
        dplyr::filter(.data$is_method_lgl)
    }
    if(return_1L_chr == "types"){
      functions_tb <- functions_tb %>%
        dplyr::filter(!.data$is_method_lgl)
    }
    functions_tb <- functions_tb %>%
      dplyr::select("fn_type_nm_chr", "fn_type_desc_chr")
  }
  }
  return(functions_tb)
}
get_generics <- function(pkg_nm_1L_chr = "ready4",
                         return_1L_chr = "all",
                         exclude_mthds_for_chr = NA_character_,
                         framework_only_1L_lgl = TRUE){
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
  if(framework_only_1L_lgl){
    generics_chr <- setdiff(generics_chr,
                            c("addNextMethod", "body<-", "cbind2", "coerce", "coerce<-", "initialize",
                              "kronecker", "loadMethod", "rbind2", "show", "slotsFromS3" ))

  }
  if(return_1L_chr == "core"){
    generics_chr <- generics_chr[tolower(generics_chr)==generics_chr]
  }
  if(return_1L_chr == "extended")
    generics_chr <- generics_chr[tolower(generics_chr)!=generics_chr]
  if(return_1L_chr == "slot"){
    generics_chr <- generics_chr[endsWith(generics_chr,"Slot")]
  }
  return(generics_chr)
}
get_gh_repos <- function (org_1L_chr) {
  if (!requireNamespace("gh", quietly = TRUE)) {
    stop("gh package is required - please install it and rerun the last command.")
  }
  acknowledgement_1L_chr <- "This function is a minor rephrasing of natmanager::list_repo"
  repositories_ls <- get_gracefully(paste0("/orgs/", org_1L_chr, "/repos"), fn = gh::gh, args_ls=list(type = "public"), not_chr_1L_lgl = TRUE)
  if(!is.null(repositories_ls)){
    repositories_chr <- vapply(repositories_ls, "[[", "", "name")
  }else{
    repositories_chr <- NULL
  }
  return(repositories_chr)
}
get_gracefully <- function(url_1L_chr,
                           args_ls = NULL,
                           fn = readRDS,
                           not_chr_1L_lgl = F,
                           tests_chr = character(0)) {
  if(identical(tests_chr, character(0))){
    tests_chr <- c("cannot open the connection to ",
                   "unknown input format",
                   "Attempt to get feed was unsuccessful",
                   "Not Found \\(HTTP 404\\)",
                   "GitHub API error \\(404\\)",
                   "Bad Request \\(HTTP 400\\)",
                   "Cannot access release data for repo",
                   "HTTP error 404",
                   "Could not resolve host",
                   "Could not parse",
                   "does not exist in current working directory",
                   "Could not retrieve Dataset ID from persistent identifier",
                   "Unknown HTTP verb")
  }
  if(identical(fn, readRDS)){
    url_xx <- NULL
    if(grepl("www.|http:|https:", url_1L_chr))
      url_xx <- url(url_1L_chr)
  }else{
    url_xx <- url_1L_chr
  }
  if (!curl::has_internet()) {
    message("No internet connection.")
    object_xx <- invisible(NULL)
  }else{
    if(!is.null(url_xx) | is.null(url_1L_chr) ){
      object_xx <- suppressWarnings(tryCatch(tryCatch(rlang::exec(fn, url_xx, !!!args_ls)), error = function(e) conditionMessage(e)))
      if(is.character(object_xx)){
        if(any(tests_chr %>% purrr::map_lgl(~stringr::str_detect(object_xx[1],.x))) | not_chr_1L_lgl){
          message(object_xx)
          object_xx <- invisible(NULL)
        }else{
          message("A character string is being returned. If this is not what you expected, it is likely to be the error message produced when the requested online resource has not been found. Set the not_chr_1L_lgl argument to TRUE to force a NULL return value when an internet resource is not found.")
        }
      }
    }else{
      object_xx <- NULL
      message("Valid URL must be supplied if using ReadRDS method to source internet file")
    }
  }
  return(object_xx)
}
get_libraries_ls <- function(gh_repo_1L_chr = "ready4-dev/ready4",
                             gh_tag_1L_chr = "Documentation_0.0"){
  libraries_ls <- NULL
  dmt_urls_xx <- get_gracefully(NULL, fn = piggyback::pb_download_url,
                                args_ls = list(repo = gh_repo_1L_chr, tag = gh_tag_1L_chr, .token = ""))
  if(!is.null(dmt_urls_xx)){
    dmt_urls_chr <- dmt_urls_xx
  if(any(dmt_urls_chr %>% endsWith("libraries_ls.RDS"))){
    libraries_ls <- get_gracefully(dmt_urls_chr[dmt_urls_chr %>% endsWith("libraries_ls.RDS")], not_chr_1L_lgl = TRUE)
  }
  }
  return(libraries_ls)
}
get_libraries_tb <- function(gh_repo_1L_chr = "ready4-dev/ready4",
                             gh_tag_1L_chr = "Documentation_0.0"){
  libraries_tb <- NULL
  dmt_urls_xx <- get_gracefully(NULL, fn = piggyback::pb_download_url,
                                args_ls = list(repo = gh_repo_1L_chr, tag = gh_tag_1L_chr, .token = ""))
  if(!is.null(dmt_urls_xx)){
    dmt_urls_chr <- dmt_urls_xx
    if(any(dmt_urls_chr %>% endsWith("libraries_tb.RDS"))){
      libraries_tb <- get_gracefully(dmt_urls_chr[dmt_urls_chr %>% endsWith("libraries_tb.RDS")])
    }
  }
  return(libraries_tb)
}
get_manual_urls <- function(pkg_nm_1L_chr = "ready4",
                            pkg_url_1L_chr = "https://ready4-dev.github.io/ready4/index.html"){
  urls_xx <- get_gracefully(pkg_url_1L_chr, fn=rvest::read_html)
  if(!is.null(urls_xx)){
    urls_chr <- urls_xx %>%
      rvest::html_elements(".external-link") %>%
      rvest::html_attr("href")
    indcs_int <- urls_chr %>%
      purrr::map_lgl(~endsWith(.x,
                               paste0(pkg_nm_1L_chr,"_User.pdf"))|endsWith(.x,
                                                                           paste0(pkg_nm_1L_chr,"_Developer.pdf"))) %>%
      which()
    urls_chr <- sort(urls_chr[indcs_int], decreasing = TRUE)
  }else{
    urls_chr <- NULL
  }
  return(urls_chr)
}
get_methods <- function(pkg_nm_1L_chr = "ready4",
                        cls_nm_1L_chr =  "Ready4Module"){
  methods_chr <- showMethods(classes = "Ready4Module", printTo =FALSE )
  methods_chr <- methods_chr[which(methods_chr %>% stringr::str_detect(cls_nm_1L_chr))-1]
  methods_chr <- methods_chr[which(methods_chr%>% stringr::str_detect(paste0("package ",pkg_nm_1L_chr)))] %>%
    stringr::str_remove_all("Function: ") %>%
    stringr::str_remove_all(paste0(" \\(package ",pkg_nm_1L_chr,"\\)"))
  return(methods_chr)
}
get_methods_tb <- function(gh_repo_1L_chr = "ready4-dev/ready4",
                           gh_tag_1L_chr = "Documentation_0.0"){
  methods_tb <- NULL
  dmt_urls_xx <- get_gracefully(NULL, fn = piggyback::pb_download_url,
                                args_ls = list(repo = gh_repo_1L_chr, tag = gh_tag_1L_chr, .token = ""))
  if(!is.null(dmt_urls_xx)){
    dmt_urls_chr <- dmt_urls_xx
  methods_tb <- get_gracefully(dmt_urls_chr[dmt_urls_chr %>% endsWith("methods_tb.RDS")])
  }
  return(methods_tb)
}
get_modules_tb <- function(gh_repo_1L_chr = "ready4-dev/ready4",
                           gh_tag_1L_chr = "Documentation_0.0"){
  modules_tb <- NULL
  dmt_urls_xx <- get_gracefully(NULL, fn = piggyback::pb_download_url,
                                args_ls = list(repo = gh_repo_1L_chr, tag = gh_tag_1L_chr, .token = ""))
  if(!is.null(dmt_urls_xx)){
    dmt_urls_chr <- dmt_urls_xx
    modules_tb <- get_gracefully(dmt_urls_chr[dmt_urls_chr %>% endsWith("modules_tb.RDS")])
  }
  return(modules_tb)
}
get_mthd_titles <- function(mthd_nms_chr,
                            pkg_nm_1L_chr = "ready4",
                            path_1L_chr = character(0)){
  mthd_titles_chr <-  mthd_nms_chr %>%
    purrr::map_chr(~{
      mthd_nm_1L_chr <- .x
      df <- mthd_nm_1L_chr %>% stringr::str_locate("\\.")
      if(!is.na(df[[1,1]])){
        mthd_nm_1L_chr <- stringr::str_sub(mthd_nm_1L_chr,
                                           end = (df[[1,1]]-1))
      }
      if(!identical(path_1L_chr, character(0))){
        dmtn_tb <- tools::Rd_db(dir = path_1L_chr)
      }else{
        dmtn_tb <- tools::Rd_db(pkg_nm_1L_chr)
      }
      gnrc_dmt_ls <- dmtn_tb %>%
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
                            dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/RIQTKK",
                            dv_url_pfx_1L_chr = character(0),
                            key_1L_chr = NULL,
                            server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  if(identical(dv_url_pfx_1L_chr, character(0)))
    dv_url_pfx_1L_chr <- paste0("https://",
                                server_1L_chr,
                                "/api/access/datafile/")
  ds_ls <- get_gracefully(dv_ds_nm_1L_chr, fn = dataverse::dataset_files,
                 args_ls = list(server = server_1L_chr, key = key_1L_chr), not_chr_1L_lgl = TRUE)
  if(!is.null(ds_ls)){
    all_items_chr <- purrr::map_chr(ds_ls,~.x$label) %>%
      stringi::stri_replace_last_regex("\\.RDS","") %>%
      stringi::stri_replace_last_regex("\\.Rds","") %>%
      stringi::stri_replace_last_regex("\\.rds","")
    idx_1L_int <- which(all_items_chr == file_nm_1L_chr) #paste0(file_nm_1L_chr,".RDS")
  }else{
    idx_1L_int <- integer(0)
  }
  if(identical(idx_1L_int, integer(0))){
    r_object_xx <- NULL
  }else{
    r_object_xx <- get_gracefully(paste0(dv_url_pfx_1L_chr,
                                      ds_ls[[idx_1L_int]]$dataFile$id))
  }
  return(r_object_xx)
}

get_source_code_urls <- function(pkg_nm_1L_chr = "ready4",
                                 pkg_url_1L_chr = "https://ready4-dev.github.io/ready4/index.html"){
  urls_xx <- get_gracefully(pkg_url_1L_chr, fn=rvest::read_html)
  if(!is.null(urls_xx)){
    urls_chr <- urls_xx %>%
      rvest::html_elements(".external-link") %>%
      rvest::html_attr("href")
    indcs_int <- c(which((urls_xx %>%
                            rvest::html_elements(".external-link") %>%
                            rvest::html_text()) == "Browse source code"),
                   which(startsWith(urls_chr,
                                    "https://doi.org/10.5281/zenodo.")))
    urls_chr <- urls_chr[indcs_int]
  }else{
    urls_chr <- NULL
  }
  return(urls_chr)
}
get_subroutine_repos <- function(gh_repo_1L_chr = "ready4-dev/ready4",
                                 gh_tag_1L_chr = "Documentation_0.0"){
  subroutine_repos_chr <- NULL
  dmt_urls_xx <- get_gracefully(NULL, fn = piggyback::pb_download_url,
                                args_ls = list(repo = gh_repo_1L_chr, tag = gh_tag_1L_chr, .token = ""))
  if(!is.null(dmt_urls_xx)){
    dmt_urls_chr <- dmt_urls_xx
    if(any(dmt_urls_chr %>% endsWith("subroutine_repos_chr.RDS"))){
      subroutine_repos_chr <- get_gracefully(dmt_urls_chr[dmt_urls_chr %>% endsWith("subroutine_repos_chr.RDS")])
    }else{
      subroutine_repos_chr <- character(0)
    }
    }
  return(subroutine_repos_chr)
}
get_table_from_loc_file <- function(path_1L_chr,
                                    force_numeric_1L_lgl = FALSE,
                                    force_tb_1L_lgl = FALSE,
                                    heading_rows_1L_int = 1L){
  file_type_1L_chr <-  get_fl_extension(path_1L_chr)
  pkg_1L_chr <- switch(file_type_1L_chr,
                       csv = "readr", # read.csv,
                       xls = "readxl",
                       xlsx = "readxl", #readxl::read_excel,
                       RDS = NULL)
  if(!is.null(pkg_1L_chr)){
    if(!requireNamespace(pkg_1L_chr, quietly = TRUE)) {
      stop(paste0(pkg_1L_chr," package is required - please install it and rerun the last command."))
    }
  }
  fn <- switch(file_type_1L_chr,
               csv = readr::read_csv, # read.csv,
               xls = readxl::read_excel,
               xlsx = readxl::read_xlsx, #readxl::read_excel,
               RDS = readRDS)
  table_xx <- rlang::exec(fn,path_1L_chr)
  if(force_tb_1L_lgl)
    table_xx <- tibble::as_tibble(table_xx)
  if(heading_rows_1L_int>1L)
    table_xx <- table_xx %>%
    dplyr::slice(heading_rows_1L_int:dplyr::n())
  if(force_numeric_1L_lgl){
    table_xx <- table_xx %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.character), ## added ns
                                  ~transform_chr_to_num(.x))) #_if(is.character, transform_chr_to_num) %>%
  }
  return(table_xx)
}
