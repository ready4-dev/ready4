library(ready4fun)
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
                           copyright_holders_chr = "Orygen",
                           lifecycle_stage_1L_chr = "experimental",
                           path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/ready4-logo/default.png",
                           pkg_dmt_dv_dss_chr = c("https://doi.org/10.7910/DVN/HLLXZN",
                                                  "https://doi.org/10.7910/DVN/2Y9VF9"),
                           ready4_type_1L_chr = "authoring")
x <- ready4fun::author(x,
                       list_generics_1L_lgl = T)
