library(magrittr)
ready4fun::write_fn_type_dirs()
x <- ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Generic Functions for Modular Mental Health Models" %>% tools::toTitleCase(),
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
                           pkg_dmt_dv_dss_chr = c("https://doi.org/10.7910/DVN/HLLXZN",
                                                  "https://doi.org/10.7910/DVN/2Y9VF9"),
                           ready4_type_1L_chr = "foundation")
# x$subsequent_ls$s4_fns_ls$fn <- ready4class::write_r4_mthds
# x$subsequent_ls$s4_fns_ls$args_ls <- list(fns_dir_1L_chr = paste0(x$initial_ls$path_to_pkg_rt_1L_chr,"/data-raw/s4_fns"),
#                                                                import_from_chr = x$subsequent_ls$import_from_chr,
#                                                                output_dir_1L_chr = paste0(x$initial_ls$path_to_pkg_rt_1L_chr,"/R"),
#                                                                pkg_nm_1L_chr = x$initial_ls$pkg_desc_ls$Package)
x <- ready4fun::ratify.ready4fun_manifest(x)
if(!is.null(x$problems_ls)){
  message("Execution halted - fix issues with manifest before making a new call to author.")
}else{
  message("Manifest has been validated. Proceeding to package set-up.")
  dir.create('data-raw/safety')
  file.copy("R", "data-raw/safety", recursive=TRUE)
  ready4fun::author.ready4fun_metadata_a(x$initial_ls)
  # ANSWER "N" TO DELETE GENERICS AND FN FILES
  # ready4fun::write_to_delete_fls(c(#"R/imp_fns.R",
  #                                  "R/imp_mthds.R"))
  devtools::document()
  x <- ready4fun::authorData.ready4fun_manifest(x)
  # x <- ready4fun::authorClasses.ready4fun_manifest(x,
  #                                                  ##
  #                                                  self_serve_1L_lgl = T)
  x <- ready4fun::renew.ready4fun_manifest(x,
                                           type_1L_chr = "fns_dmt")
  fns_dmt_tb <- x$subsequent_ls$fns_dmt_tb
  x$subsequent_ls$fns_dmt_tb <- x$subsequent_ls$fns_dmt_tb %>%
    dplyr::filter(!file_nm_chr %>% endsWith("generics.R"))
  ready4fun::authorFunctions.ready4fun_manifest(x,
                  list_generics_1L_lgl = T)
  # file.copy("data-raw/generics/grp_generics.R",
  #           "R/grp_generics.R")
  # devtools::document()
 x$subsequent_ls$fns_dmt_tb <-  fns_dmt_tb
  ready4fun::report.ready4fun_manifest(x,
         key_1L_chr = Sys.getenv("DATAVERSE_KEY"))
}
# still need to manually add generics
