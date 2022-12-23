## This initial section is ugly and bespoke to this package. Its a hacky solution to the problem that the packages
## that format code and documentation using the ready4 house-style are dependencies of the ready4 package.
## The following lines enables these dependencies to be deployed in authoring this parent package.
## Functions adapted from ready4fun
write_all_fn_dmt <- function(pkg_setup_ls,
                             fns_env_ls,
                             document_unexp_lgl = F,
                             fns_dmt_tb = deprecated()){
  if(lifecycle::is_present(fns_dmt_tb)) {
    lifecycle::deprecate_warn("0.0.0.9421",
                              "ready4fun::write_all_fn_dmt(fns_dmt_tb)",
                              details = "Please use `ready4fun::write_all_fn_dmt(pkg_desc_ls)` to pass the fns_dmt_tb object to this function.")
  }
  pkg_setup_ls$subsequent_ls$fns_dmt_tb <- pkg_setup_ls$subsequent_ls$fns_dmt_tb %>%
    dplyr::filter(!is.na(file_nm_chr))
  pkg_setup_ls$subsequent_ls$fns_dmt_tb$file_nm_chr <- pkg_setup_ls$subsequent_ls$fns_dmt_tb$file_nm_chr %>%
    stringr::str_replace_all(paste0(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr,"/"),"")
  if(file.exists(paste0(paste0(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr,"/R"),
                        "/grp_generics.R"))){
    pkg_setup_ls$subsequent_ls$fns_dmt_tb <- pkg_setup_ls$subsequent_ls$fns_dmt_tb %>%
      dplyr::filter(file_nm_chr != "generics.R")
  }
  paths_chr <- paste0(paste0(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr,"/R"),
                      "/",
                      pkg_setup_ls$subsequent_ls$fns_dmt_tb$file_pfx_chr,
                      pkg_setup_ls$subsequent_ls$fns_dmt_tb$file_nm_chr %>%
                        stringr::str_replace_all(paste0(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr,"/"),
                                                 "")) %>%
    unique()
  fns_env_ls$fns_env$write_new_files(paths_chr = paths_chr,
                                     custom_write_ls = list(fn = write_fn_fl,
                                                            args_ls = list(fns_env_ls = fns_env_ls,
                                                                           pkg_setup_ls = pkg_setup_ls,
                                                                           document_unexp_lgl = document_unexp_lgl)))
  devtools::document()
  devtools::load_all()
  if(length(pkg_setup_ls$subsequent_ls$s4_fns_ls)>0){
    s4_mthds_ls <- rlang::exec(pkg_setup_ls$subsequent_ls$s4_fns_ls$fn,
                               !!!pkg_setup_ls$subsequent_ls$s4_fns_ls$args_ls)
    devtools::document()
    devtools::load_all()
  }else{
    s4_mthds_ls <- NULL
  }
  return(s4_mthds_ls)
}
write_fn_fl <- function(fns_env_ls,
                        pkg_setup_ls,
                        document_unexp_lgl = T,
                        consent_1L_chr = NULL,
                        fns_dmt_tb = deprecated(),
                        r_dir_1L_chr = deprecated()){
  if(lifecycle::is_present(fns_dmt_tb)) {
    lifecycle::deprecate_warn("0.0.0.9421",
                              "ready4fun::write_fn_fl(fns_dmt_tb)",
                              details = "Please use `ready4fun::write_fn_fl(pkg_desc_ls)` to pass the fns_dmt_tb object to this function.")
  }
  if (lifecycle::is_present(r_dir_1L_chr)) {
    lifecycle::deprecate_warn("0.0.0.9199",
                              "ready4fun::write_fn_fl(r_dir_1L_chr)",
                              details = "Please use `ready4fun::write_fn_fl(pkg_setup_ls)` to pass the R directory path to this function.")
  }
  r_dir_1L_chr <- paste0(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr,"/R")
  pkg_setup_ls$subsequent_ls$fns_dmt_tb$file_nm_chr <- pkg_setup_ls$subsequent_ls$fns_dmt_tb$file_nm_chr %>%
    stringr::str_replace_all(paste0(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr,"/"),"")
  file_nms_chr <- pkg_setup_ls$subsequent_ls$fns_dmt_tb$file_nm_chr %>% unique()
  if(is.null(consent_1L_chr)){
    consent_1L_chr <- make_prompt(prompt_1L_chr=paste0("Do you confirm ('Y') that you want to write the files ",
                                                       file_nms_chr %>%
                                                         paste0(collapse = ", ") %>%
                                                         stringi::stri_replace_last(fixed = ",", " and"),
                                                       " to the ",
                                                       r_dir_1L_chr,
                                                       " directory?"),
                                  options_chr = c("Y", "N"),
                                  force_from_opts_1L_chr = T)
  }
  if(consent_1L_chr == "Y"){
    file_nms_chr %>%
      purrr::walk(~
                    {
                      tb <- pkg_setup_ls$subsequent_ls$fns_dmt_tb %>%
                        dplyr::filter(file_nm_chr == .x)
                      first_lgl_vec <- c(T,rep(F,nrow(tb)-1))
                      dest_path_1L_chr <- paste0(r_dir_1L_chr,"/",tb$file_pfx_chr[1],.x)
                      purrr::walk(1:nrow(tb),
                                  ~
                                    {
                                      #if(!exists(tb[[.x,1]])){
                                      fn <- fns_env_ls$fns_env[[tb[[.x,1]]]]
                                      #}else{
                                      #  fn <- eval(parse(text=tb[[.x,1]]))
                                      #}
                                      # fn <- eval(parse(text=tb[[.x,1]]))
                                      fn_chr <- deparse(fn)
                                      fn_and_cls_chr <- tb[[.x,1]] %>% strsplit("\\.") %>% purrr::pluck(1)
                                      sink(dest_path_1L_chr, append =  !first_lgl_vec[.x])
                                      ready4fun::make_lines_for_fn_dmt(fn_name_1L_chr = tb[[.x,1]],
                                                                       fn_type_1L_chr = ifelse(tb$file_pfx_chr[1]=="mthd_",
                                                                                               "meth_std_s3_mthd",
                                                                                               ifelse(tb$file_pfx_chr[1]=="grp_",
                                                                                                      "gen_std_s3_mthd",
                                                                                                      "fn")),
                                                                       fn = fn,
                                                                       fn_desc_1L_chr = tb[[.x,3]],
                                                                       fn_out_type_1L_chr = tb[[.x,6]],
                                                                       fn_title_1L_chr = ifelse(tb$file_pfx_chr[1] %in% c("mthd_","grp_"),
                                                                                                tb[[.x,2]],
                                                                                                Hmisc::capitalize(tb[[.x,2]])),
                                                                       example_1L_lgl = tb[[.x,7]],
                                                                       export_1L_lgl = T,
                                                                       class_name_1L_chr = "",
                                                                       details_1L_chr = tb[[.x,4]],
                                                                       args_ls = tb$args_ls[[.x]] %>% as.list(),
                                                                       import_chr = NA_character_,
                                                                       import_from_chr = pkg_setup_ls$subsequent_ls$import_from_chr,
                                                                       doc_in_class_1L_lgl = F,
                                                                       abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup,
                                                                       fn_types_lup = pkg_setup_ls$subsequent_ls$fn_types_lup,
                                                                       object_type_lup = pkg_setup_ls$subsequent_ls$object_type_lup)
                                      if(tb[[.x,5]] + document_unexp_lgl == 0){
                                        writeLines(paste0("#' @keywords internal"))
                                      }
                                      writeLines(paste0(tb[[.x,1]]," <- ",fn_chr[1]))
                                      writeLines(fn_chr[2:length(fn_chr)])
                                      if(tb$file_pfx_chr[1]=="grp_"){
                                        writeLines(paste0("methods::setGeneric(\"",
                                                          tb[[.x,1]],
                                                          "\")"))
                                      }
                                      if(tb$file_pfx_chr[1]=="mthd_"){
                                        writeLines(paste0("#' @rdname ",fn_and_cls_chr[1],"-methods"))
                                        writeLines(paste0("#' @aliases ",fn_and_cls_chr[1],",",fn_and_cls_chr[2],"-method"))
                                        {
                                          if(fn_and_cls_chr[1] %in% names(pkg_setup_ls$subsequent_ls$import_from_chr)){
                                            writeLines(paste0("#' @importFrom ",
                                                              unname(pkg_setup_ls$subsequent_ls$import_from_chr)[names(pkg_setup_ls$subsequent_ls$import_from_chr) == fn_and_cls_chr[1]],
                                                              " ",
                                                              fn_and_cls_chr[1]))
                                          }
                                        }
                                        writeLines(paste0('methods::setMethod(\"',
                                                          fn_and_cls_chr[1],
                                                          '\"',
                                                          ', ',
                                                          "methods::className(",
                                                          paste0('\"',
                                                                 fn_and_cls_chr[2],
                                                                 '\"',
                                                                 ', package = \"',
                                                                 pkg_setup_ls$initial_ls$pkg_desc_ls$Package,
                                                                 '\"'),
                                                          ")",
                                                          ', ',
                                                          tb[[.x,1]],
                                                          ')'))
                                      }
                                      ready4fun::close_open_sinks()
                                    })
                    }
      )
  }
}
write_new_generic_descs <- function(x){
  generics_txt_chr <- readLines("R/grp_generics.R")
  title_indcs_int <- (generics_txt_chr %>% startsWith("#' @rdname") %>% which())-1
  titles_chr <- generics_txt_chr[title_indcs_int]
  descriptions_indcs_int <- generics_txt_chr %>% startsWith("#' @description") %>% which()
  descriptions_chr <- generics_txt_chr[descriptions_indcs_int]
  end_indcs_int <- (descriptions_chr %>% stringi::stri_locate_first_fixed("()") %>% `[`(,1)) - 1
  replacements_chr <- purrr::map2_chr(descriptions_chr,
                                      end_indcs_int,
                                      ~ {
                                        original_1L_chr <- generic_1L_chr <- stringr::str_sub(.x, start = 17, end = .y)
                                        #substr(generic_1L_chr, 1, 1) <- toupper(substr(generic_1L_chr, 1, 1))
                                        mthd_desc_1L_chr <- get_from_lup_obj(x$subsequent_ls$fn_types_lup,
                                                                             match_value_xx = generic_1L_chr,
                                                                             match_var_nm_1L_chr = "fn_type_nm_chr",
                                                                             target_var_nm_1L_chr = "fn_type_desc_chr")
                                        substr(mthd_desc_1L_chr, 1, 1) <- tolower(substr(mthd_desc_1L_chr, 1, 1))
                                        paste0("#' @description ",
                                               original_1L_chr,
                                               "() is a method that ",
                                               mthd_desc_1L_chr)
                                      }
  )
  new_titles_ls <- replacements_chr %>% stringr::str_replace_all("@description ","") %>%
    stringi::stri_replace_last_regex(".","") %>%
    stringr::str_replace_all("\\(\\) is a method that", " -") %>% strsplit("- ")
  new_titles_chr <- new_titles_ls %>% purrr::map_chr(~{
    first_1L_chr <- .x[2] %>% stringr::word()
    new_first_1L_chr <- stringr::str_sub(first_1L_chr, end = -2) %>% Hmisc::capitalize()
    remainder_1L_chr <- stringr::str_remove(.x[2],first_1L_chr)
    if(startsWith(.x[2],paste0(first_1L_chr," and "))){
      second_1L_chr <- stringr::str_remove(.x[2],paste0(first_1L_chr," and ")) %>% stringr::word()
      new_second_1L_chr <- stringr::str_sub(second_1L_chr, end = -2)
      remainder_1L_chr <- stringr::str_remove(.x[2],paste0(first_1L_chr," and ", second_1L_chr))
      new_first_1L_chr <- paste0(new_first_1L_chr, " and ", new_second_1L_chr)
    }
    paste0("#' ",new_first_1L_chr, remainder_1L_chr)
  })
  # %>% stringr::str_replace_all("Searche ","Search ") %>%
  #   stringr::str_replace_all("Processe ","Process ") %>%
  #   stringr::str_replace("and uploads to an online repository","and upload to an online repository")

  generics_txt_chr <- purrr::reduce(1:length(new_titles_chr),
                                    .init = generics_txt_chr,
                                    ~ {
                                      .x[title_indcs_int[.y]] <- new_titles_chr[.y]
                                      .x
                                    })
  generics_txt_chr <- purrr::reduce(1:length(replacements_chr),
                                    .init = generics_txt_chr,
                                    ~ {
                                      .x[descriptions_indcs_int[.y]] <- replacements_chr[.y]
                                      .x
                                    })
  generics_txt_chr %>%
    writeLines("R/grp_generics.R")
  devtools::load_all()
}
write_to_copy_s4_mthds <- function(fns_dir_1L_chr,
                                   fn_types_lup,
                                   import_from_chr,
                                   output_dir_1L_chr,
                                   pkg_nm_1L_chr){
  file_nms_chr <- list.files(fns_dir_1L_chr)
  mthd_nms_chr <- file_nms_chr %>% stringi::stri_replace_last_regex("\\.R","")
  purrr::walk(file_nms_chr,
              ~   write_new_files(output_dir_1L_chr,
                                  source_paths_ls = list(paste0(fns_dir_1L_chr,"/",.x)),
                                  fl_nm_1L_chr = paste0("mthd_",.x)))
  s4_mthds_ls <- list(mthds_ls = mthd_nms_chr %>% purrr::map(~c(Ready4Module = .x)) %>%
                        stats::setNames(mthd_nms_chr))
  return(s4_mthds_ls)
}
write_to_copy_s4_cls_fls <- function(x,#ready4fun_manifest or pkg_setup_ls
                                     class_pt_lup,
                                     dev_pkg_nm_1L_chr,
                                     path_to_cls_fls_1L_chr = "data-raw/clss"){
  cls_fls_chr <- list.files(path_to_cls_fls_1L_chr)
  if(!identical(character(0),cls_fls_chr)){
    cls_fls_chr %>%
      purrr::walk(~file.copy(paste0(path_to_cls_fls_1L_chr,"/",.x),
                             "R"))
    devtools::document()
    clss_chr <- cls_fls_chr %>% stringr::str_sub(start=4,end=-3)

    prototype_lup <- class_pt_lup %>%
      dplyr::filter(pt_ns_chr != dev_pkg_nm_1L_chr) %>%
      tibble::add_case(type_chr = clss_chr,
                       val_chr = paste0(clss_chr,"()"),
                       pt_ns_chr = dev_pkg_nm_1L_chr,
                       fn_to_call_chr = clss_chr,
                       old_class_lgl = F)
    x$subsequent_ls$prototype_lup <- prototype_lup
    if(!identical(class_pt_lup,
                  prototype_lup) & !is.null(class_pt_lup)){
      write_env_objs_to_dv(list(prototype_lup = prototype_lup),
                           descriptions_chr = "Class prototype lookup table",
                           ds_url_1L_chr = x$subsequent_ls$pkg_dmt_dv_dss_chr[2],
                           piggyback_to_1L_chr = x$subsequent_ls$piggyback_to_1L_chr,
                           publish_dv_1L_lgl = F)
    }
  }
  return(x)
}
## Single use custom function
write_self_srvc_pkg <- function(x){
  x <- ready4fun::ratify.ready4fun_manifest(x)
  if(!is.null(x$problems_ls)){
    message("Execution halted - fix issues with manifest before making a new call to author.")
  }else{
    message("Manifest has been validated. Proceeding to package set-up.")
    dir.create('safety')
    file.copy("R", "safety", recursive=TRUE, overwrite = T)
    fn_fls_chr <- list.files("data-raw/fns")
    fn_fls_chr %>%
      purrr::walk(~file.copy(paste0("data-raw/fns/",.x),
                             "R"))
    ready4fun::author.ready4fun_metadata_a(x$initial_ls)
    # important: ANSWER "N" TO DELETE GENERICS AND FN FILES
    # ready4fun::write_to_delete_fls(c(#"R/imp_fns.R",
    #                                  "R/imp_mthds.R"))
    # devtools::document()
    x <- ready4fun::authorData.ready4fun_manifest(x)
    x <- write_to_copy_s4_cls_fls(x,
                                  class_pt_lup = ready4fun::get_rds_from_pkg_dmt(x,
                                                                                 fl_nm_1L_chr = "prototype_lup"),
                                  dev_pkg_nm_1L_chr = "ready4")
    x <- ready4fun::renew.ready4fun_manifest(x,
                                             type_1L_chr = "fns_dmt")
    fns_dmt_tb <- x$subsequent_ls$fns_dmt_tb
    x$subsequent_ls$fns_dmt_tb <- x$subsequent_ls$fns_dmt_tb %>%
      dplyr::filter(!file_nm_chr %>% endsWith("generics.R"))
    pkg_setup_ls <- x
    ready4fun::add_build_ignore(pkg_setup_ls$subsequent_ls$build_ignore_ls)
    ready4fun::add_addl_pkgs(pkg_setup_ls$subsequent_ls$addl_pkgs_ls)
    dev_pkgs_chr <- pkg_setup_ls$subsequent_ls$dev_pkgs_chr
    r_dir_1L_chr <- paste0(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr,"/R")
    fns_env_ls <- ready4fun::read_fns()
    fns_env_ls$fns_env$write_new_dirs(c(pkg_setup_ls$subsequent_ls$path_to_dmt_dir_1L_chr,
                                        paste0(pkg_setup_ls$subsequent_ls$path_to_dmt_dir_1L_chr,"/Developer"),
                                        paste0(pkg_setup_ls$subsequent_ls$path_to_dmt_dir_1L_chr,"/User")))
    fns_env_ls <- ready4fun::read_fns(ready4fun::make_undmtd_fns_dir_chr(paste0(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr,
                                                                                "/data-raw"),
                                                                         drop_empty_1L_lgl = T))
    pkg_setup_ls$subsequent_ls$s4_fns_ls$fn <- write_to_copy_s4_mthds#ready4class::write_r4_mthds
    pkg_setup_ls$subsequent_ls$s4_fns_ls$args_ls <- list(fns_dir_1L_chr = paste0(x$initial_ls$path_to_pkg_rt_1L_chr,"/data-raw/s4_fns"),
                                                         fn_types_lup = x$subsequent_ls$fn_types_lup,
                                                         import_from_chr = x$subsequent_ls$import_from_chr,
                                                         output_dir_1L_chr = paste0(x$initial_ls$path_to_pkg_rt_1L_chr,"/R"),
                                                         pkg_nm_1L_chr = x$initial_ls$pkg_desc_ls$Package)

    s4_mthds_ls_ls <- purrr::map2(list(paste0(pkg_setup_ls$subsequent_ls$path_to_dmt_dir_1L_chr,"/Developer"),
                                       paste0(pkg_setup_ls$subsequent_ls$path_to_dmt_dir_1L_chr,"/User")),
                                  c(T,F),
                                  ~ {
                                    s4_mthds_ls <- write_all_fn_dmt(pkg_setup_ls,
                                                                    fns_env_ls = fns_env_ls,
                                                                    document_unexp_lgl = .y)
                                    ready4fun::write_ns_imps_to_desc(dev_pkgs_chr = dev_pkgs_chr,
                                                                     incr_ver_1L_lgl = .y)
                                    devtools::load_all()
                                    if(T)
                                      devtools::build_manual(path = .x)
                                    s4_mthds_ls
                                  })
    pkg_setup_ls$subsequent_ls$fns_dmt_tb <- fns_dmt_tb
    #write_new_generic_descs(pkg_setup_ls)
    if(T){
      datasets_chr <- utils::data(package = ready4fun::get_dev_pkg_nm(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr),
                                  envir = environment())$results[,3]
      list_generics_1L_lgl <- T

      writeLines(c("development:",
                   "  mode: auto",
                   "reference:",
                   {
                     if(length(datasets_chr)>0){
                       c(paste0("- title: \"",
                                "Datasets",
                                "\""),
                         "- contents:",
                         paste0("  - ", datasets_chr))
                     }
                   },
                   {
                     if(!is.null(pkg_setup_ls$subsequent_ls$prototype_lup)){
                       classes_tb <- pkg_setup_ls$subsequent_ls$prototype_lup %>% dplyr::filter(pt_ns_chr == pkg_setup_ls$initial_ls$pkg_desc_ls$Package)
                       purrr::map(c("S3","S4"),
                                  ~ {
                                    fns_chr <- classes_tb %>% dplyr::filter(old_class_lgl == (.x=="S3")) %>%
                                      dplyr::pull(fn_to_call_chr)
                                    if(length(fns_chr)>0){
                                      txt_chr <- c(paste0("- title: \"",
                                                          paste0(.x," Classes"),
                                                          "\""),
                                                   "- contents:",
                                                   paste0("  - ", fns_chr))
                                    }else{
                                      txt_chr <- ""
                                    }
                                    txt_chr
                                  }) %>%
                         purrr::flatten_chr() %>%
                         purrr::discard(~.x=="")

                     }
                   },
                   purrr::map2(c("fn_",
                                 ifelse(!list_generics_1L_lgl,NA_character_,"grp_"),
                                 "mthd_") %>% purrr::discard(is.na),
                               c("Functions",
                                 ifelse(!list_generics_1L_lgl,NA_character_,"Generics"),
                                 "Methods")  %>% purrr::discard(is.na),
                               ~{
                                 is_gnrc_1L_lgl <- (.x == "grp_")
                                 fns_chr <- dplyr::filter(pkg_setup_ls$subsequent_ls$fns_dmt_tb,
                                                          inc_for_main_user_lgl %>% purrr::map_lgl(~ifelse(is_gnrc_1L_lgl,T,.x)) & file_pfx_chr == .x) %>%
                                   dplyr::pull(fns_chr)
                                 if(.x=="mthd_" & !is.null(s4_mthds_ls_ls)){
                                   fns_chr <- c(fns_chr,
                                                s4_mthds_ls_ls[[2]]$mthds_ls %>%
                                                  purrr::map2(names(s4_mthds_ls_ls[[2]]$mthds_ls),
                                                              ~{
                                                                mthd_nm_1L_chr <- .y
                                                                s4_cls_nms_chr <- names(.x)
                                                                paste0(mthd_nm_1L_chr,"-",s4_cls_nms_chr)
                                                              }) %>% purrr::flatten_chr()) %>% sort()
                                 }
                                 if(length(fns_chr)>0){
                                   txt_chr  <- c( paste0("- title: \"",.y,"\""),
                                                  "- contents:",
                                                  paste0("  - ",
                                                         fns_chr))
                                 }else{
                                   txt_chr  <- ""
                                 }
                               }) %>% purrr::flatten_chr() %>% purrr::discard(~.x=="")),
                 con = paste0(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr,"/_pkgdown.yml"))
    }
    fn_fls_chr %>%
      purrr::walk(~unlink(paste0("R/",.x)))
    devtools::document()
    ready4fun::authorReport.ready4fun_manifest(pkg_setup_ls,
                                               key_1L_chr = Sys.getenv("DATAVERSE_KEY"))
  }
  fns_env_ls$fns_env$write_to_delete_dirs("safety")
  write_prototypes()
  fns_env_ls$fns_env$write_citation_cff(packageDescription("ready4"),
                     citation_chr = readLines("inst/CITATION"))
  ready4fun::write_fns_dmt_tb(x)
  return(x)
}
write_prototypes <- function(gh_repo_1L_chr = "ready4-dev/ready4",
                             gh_tag_1L_chr = "Documentation_0.0"){
  a <- ready4use::Ready4useRepos(gh_repo_1L_chr = gh_repo_1L_chr,
                                 gh_tag_1L_chr = gh_tag_1L_chr)
  dmt_urls_chr <- piggyback::pb_download_url(repo = a@gh_repo_1L_chr,
                                             tag = a@gh_tag_1L_chr,
                                             .token = "")
  b <- readRDS(url(dmt_urls_chr[dmt_urls_chr %>% endsWith("prototype_lup.RDS")]))
  b <- b %>% dplyr::mutate(default_val_chr = dplyr::case_when(pt_ns_chr == "ready4" ~ "",
                                                              T ~ default_val_chr)) %>%
    dplyr::arrange(pt_ns_chr)
  c <- tibble::as_tibble(b)
  write_env_objs_to_dv(env_objects_ls = list(prototype_lup = b,
                                             classes_lup = c),
                       descriptions_chr = NULL,
                       ds_url_1L_chr = character(0),
                       piggyback_desc_1L_chr = "Supplementary Files",
                       piggyback_tag_1L_chr =  a@gh_tag_1L_chr,
                       piggyback_to_1L_chr = a@gh_repo_1L_chr,
                       prerelease_1L_lgl = T)
}
write_badges <- function(gh_repo_1L_chr = "ready4-dev/ready4",
                         gh_tag_1L_chr = "Documentation_0.0"){
  data("badges_lup", package = "ready4fun")
  badges_lup <- badges_lup[,c(2,4)]
  write_env_objs_to_dv(env_objects_ls = list(ready4_badges_lup = badges_lup),
                       descriptions_chr = NULL,
                       ds_url_1L_chr = character(0),
                       piggyback_desc_1L_chr = "Supplementary Files",
                       piggyback_tag_1L_chr =  gh_tag_1L_chr,
                       piggyback_to_1L_chr = gh_repo_1L_chr,
                       prerelease_1L_lgl = T)
}
write_extensions <- function(gh_repo_1L_chr = "ready4-dev/ready4",
                             gh_tag_1L_chr = "Documentation_0.0",
                             include_1L_chr = "modules",
                             path_1L_chr = getwd()){
  libraries_tb <- make_libraries_tb(include_1L_chr = include_1L_chr)
  env_objects_ls <- list(datasets_tb = make_datasets_tb(),
                         libraries_tb = libraries_tb,
                         methods_tb = make_methods_tb(packages_tb = libraries_tb,
                                                      path_1L_chr = path_1L_chr),
                         modules_tb = make_modules_tb(pkg_extensions_tb = libraries_tb))
  write_env_objs_to_dv(env_objects_ls,
                       descriptions_chr = NULL,
                       ds_url_1L_chr = character(0),
                       piggyback_desc_1L_chr = "Supplementary Files",
                       piggyback_tag_1L_chr =  gh_tag_1L_chr,
                       piggyback_to_1L_chr = gh_repo_1L_chr,
                       prerelease_1L_lgl = T)
}
write_housestyle_fls <- function(){ # Use sparingly [Max 4 times a year]
  X <- ready4use::Ready4useRepos(dv_nm_1L_chr = "ready4fw",
                                 dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/RIQTKK",
                                 dv_server_1L_chr = "dataverse.harvard.edu",
                                 gh_repo_1L_chr = "ready4-dev/ready4",
                                 gh_tag_1L_chr = "Documentation_0.0")
  X <- ingest(X)
  X <- ready4::share(X@a_Ready4usePointer@b_Ready4useRepos,
                     obj_to_share_xx = X@b_Ready4useIngest@objects_ls,
                     fl_nm_1L_chr = "framework_metadata_ls",
                     description_1L_chr = "R objects useful for implementation of standardised ready4 framework workflows")
  return(X)
}
