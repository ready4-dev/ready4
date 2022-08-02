library(magrittr)
library(lifecycle)
source("data-raw/FUNCTIONS.R") # Required to manage conflicts
ready4fun::write_fn_type_dirs()
x <- ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Implement Open Source Computational Models of Youth Mental Health Systems" %>% tools::toTitleCase(),
                                                    pkg_desc_1L_chr = "ready4 provides bare bones foundational elements (classes, generics, methods, functions) of a framework for developing, extending and applying readyforwhatsnext - a modular, open source health economic model of young people's mental health.
  This development version of the ready4 package has been made available as part of the process of testing and documenting the package. If you have any questions, please contact the authors (matthew.hamilton@orygen.org.au).",
                                                    authors_prsn = c(utils::person(
                                                      given = "Matthew",family = "Hamilton", email =
                                                        "matthew.hamilton@orygen.org.au",role = c("aut",
                                                                                                  "cre"),
                                                      comment = c(ORCID = "0000-0001-7407-9194")
                                                    ),
                                                    utils::person("Orygen", role = c("cph", "fnd")),
                                                    utils::person("VicHealth",role = c("fnd")),
                                                    utils::person("Victoria University", role =c("fnd"))
                                                    ),
                                                    urls_chr = c("https://ready4-dev.github.io/ready4/",
                                                                 "https://github.com/ready4-dev/ready4",
                                                                 "https://www.ready4-dev.com/")) %>%
  ready4fun::make_manifest(addl_pkgs_ls = ready4fun::make_addl_pkgs_ls(depends_chr = "generics",
                                                                       suggests_chr = "rmarkdown"),
                           build_ignore_ls = ready4fun::make_build_ignore_ls(file_nms_chr = c("initial_setup.R")),
                           check_type_1L_chr = "ready4",
                           cls_fn_ls = list(),
                           custom_dmt_ls = ready4fun::make_custom_dmt_ls(
                             user_manual_fns_chr = c(
                               #"get_dv_fls_urls",
                               "get_from_lup_obj",#"get_rds_from_dv",
                               "get_methods",
                               "make_dvs_tb","make_libraries_tb", "make_methods_tb", "make_modules_tb",
                               "print_dss", "print_dvs", "print_methods", "print_modules", "print_packages"
                               #"write_ws"
                               )),
                           copyright_holders_chr = "Orygen",
                           import_from_chr = NA_character_,
                           lifecycle_stage_1L_chr = "experimental",
                           path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/ready4-logo/default.png",
                           piggyback_to_1L_chr = "ready4-dev/ready4",
                           ready4_type_1L_chr = "foundation",
                           zenodo_badge_1L_chr = "[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5606250.svg)](https://doi.org/10.5281/zenodo.5606250)")
##
## Reminder
## Unlike other workflows in the ready4 suite, in this instance it is necessary to answer "N", the FIRST time the following prompt appears:
## Do you confirm ('Y') that you want to delete these files: [Y|N]
## After doing so, all other such prompts should be answered in the affirmative.
x <- write_self_srvc_pkg(x)
 # ADD DOI OVERRIDE FOR RELEASES
#
# write_extensions() # Required only if extensions have changed since last build
# write_badges() # Only run if the ready4fun badges table has been updated.
# Very occasional calls to write_housestyle_fls (four a year tops)
devtools::build_vignettes()
