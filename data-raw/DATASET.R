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
                           custom_dmt_ls = ready4fun::make_custom_dmt_ls(user_manual_fns_chr = "make_gnrc_imports"),
                           copyright_holders_chr = "Orygen",
                           import_from_chr = NA_character_,
                           lifecycle_stage_1L_chr = "experimental",
                           path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/ready4-logo/default.png",
                           pkg_dmt_dv_dss_chr = c("https://doi.org/10.7910/DVN/HLLXZN",
                                                  "https://doi.org/10.7910/DVN/2Y9VF9"),
                           ready4_type_1L_chr = "authoring")
#fns_env <- ready4fun::read_fns("data-raw/gnrcs")
x <- ready4fun::ratify.ready4fun_manifest(x)
if(!is.null(x$problems_ls)){
  message("Execution halted - fix issues with manifest before making a new call to author.")
}else{
  message("Manifest has been validated. Proceeding to package set-up.")
  ready4fun::author.ready4fun_metadata_a(x$initial_ls)
  ready4fun::write_to_delete_fls(c("R/imp_fns.R","R/imp_mthds.R"))
  devtools::document()
  x <- ready4fun::authorData.ready4fun_manifest(x)
  # ready4fun::authorClasses.ready4fun_manifest(x,
  #               key_1L_chr = key_1L_chr,
  #               self_serve_1L_lgl = self_serve_1L_lgl,
  #               self_serve_fn_ls = self_serve_fn_ls)
  x <- ready4fun::renew.ready4fun_manifest(x,
             type_1L_chr = "fns_dmt",
             key_1L_chr = key_1L_chr)
  ready4fun::authorFunctions.ready4fun_manifest(x,
                  list_generics_1L_lgl = T)
  file.copy("data-raw/generics/grp_generics.R",
            "R/grp_generics.R")
  devtools::document()
  ready4fun::report.ready4fun_manifest(x,
         key_1L_chr = key_1L_chr)
}
# Manual edit of _pkgdown
devtools::document()
