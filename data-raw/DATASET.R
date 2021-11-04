library(magrittr)
library(lifecycle)
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
                                                                       fn_title_1L_chr = tb[[.x,2]],
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
    # x <- ready4fun::authorClasses.ready4fun_manifest(x,
    #                                                  ##
    #                                                  self_serve_1L_lgl = T)
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
    pkg_setup_ls$subsequent_ls$fns_dmt_tb <-  fns_dmt_tb
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
    # file.copy("data-raw/generics/grp_generics.R",
    #           "R/grp_generics.R")
    # devtools::document()
    #x$subsequent_ls$fns_dmt_tb <-  fns_dmt_tb
    ready4fun::report.ready4fun_manifest(pkg_setup_ls,
                                         key_1L_chr = Sys.getenv("DATAVERSE_KEY"))
  }
  # usethis::use_badge(badge_name = "DOI",
  #                    src = "https://zenodo.org/badge/DOI/10.5281/zenodo.5606250.svg",
  #                    href = "https://doi.org/10.5281/zenodo.5606250")
  ready4fun::write_to_delete_dirs("safety")

}
ready4fun::write_fn_type_dirs()
x <- ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "A Framework for Open and Modular Mental Health Systems Models" %>% tools::toTitleCase(),
                                                    pkg_desc_1L_chr = "ready4 provides a set of generic functions that are designed for use across the ready4 suite of packages.
  This development version of the ready4 package has been made available as part of the process of testing and documenting the package. If you have any questions, please contact the authors (matthew.hamilton@orygen.org.au).",
                                                    authors_prsn = c(utils::person(
                                                      given = "Matthew",family = "Hamilton", email =
                                                        "matthew.hamilton@orygen.org.au",role = c("aut",
                                                                                                  "cre"),comment = c(ORCID = "0000-0001-7407-9194")
                                                    ),
                                                    utils::person("Orygen", role = c("cph", "fnd")),
                                                    utils::person("VicHealth",role = c("fnd")),
                                                    utils::person("Victoria University", role =c("fnd"))
                                                    ),
                                                    urls_chr = c("https://ready4-dev.github.io/ready4/",
                                                                 "https://github.com/ready4-dev/ready4",
                                                                 "https://ready4-dev.github.io/ready4/")) %>%
  ready4fun::make_manifest(addl_pkgs_ls = ready4fun::make_addl_pkgs_ls(depends_chr = "generics",
                                                                       suggests_chr = "rmarkdown"),
                           build_ignore_ls = ready4fun::make_build_ignore_ls(file_nms_chr = c("initial_setup.R")),
                           check_type_1L_chr = "ready4",
                           cls_fn_ls = list(),
                           custom_dmt_ls = ready4fun::make_custom_dmt_ls(user_manual_fns_chr = c("get_dv_fls_urls",
                                                                                                 "get_from_lup_obj",
                                                                                                 "get_rds_from_dv",
                                                                                                 "write_ws")),
                           copyright_holders_chr = "Orygen",
                           import_from_chr = NA_character_,
                           lifecycle_stage_1L_chr = "experimental",
                           path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/ready4-logo/default.png",
                           piggyback_to_1L_chr = "ready4-dev/ready4",
                           # pkg_dmt_dv_dss_chr = c("https://doi.org/10.7910/DVN/HLLXZN",
                           #                        "https://doi.org/10.7910/DVN/2Y9VF9"),
                           ready4_type_1L_chr = "foundation",
                           zenodo_badge_1L_chr = "[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5606250.svg)](https://doi.org/10.5281/zenodo.5606250)")
# x$subsequent_ls$s4_fns_ls$fn <- ready4class::write_r4_mthds
# x$subsequent_ls$s4_fns_ls$args_ls <- list(fns_dir_1L_chr = paste0(x$initial_ls$path_to_pkg_rt_1L_chr,"/data-raw/s4_fns"),
write_self_srvc_pkg(x)
