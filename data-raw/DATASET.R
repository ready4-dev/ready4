library(magrittr)
library(lifecycle)
library(generics)
source("data-raw/FUNCTIONS.R") # Required to manage conflicts
#ready4fun::write_fn_type_dirs()
#dir.create("data-raw/examples")
x <- ready4fun::make_pkg_desc_ls(
  pkg_title_1L_chr = "Implement Modular And Open-Source Health Economic Models" %>% tools::toTitleCase(),
  pkg_desc_1L_chr = "Programming syntax, a template model module and tools to help maintain a health economic modelling project's documentation website.
  These elements are the foundation for a prototype software framework to support transparent, reusable and updatable health economic models. The software framework is extended by other R libraries.
  For detailed documentation about the framework and how to use it visit <https://www.ready4-dev.com/>. For a background to the methodological issues that the framework is attempting to help solve, read <arXiv:2310.14138>.",
  authors_prsn = c(utils::person(
    given = "Matthew",family = "Hamilton", email = "matthew.hamilton1@monash.edu", role = c("aut", "cre"),
    comment = c(ORCID = "0000-0001-7407-9194")
    ),
    utils::person("Orygen", role = c("cph", "fnd")),
    utils::person("Australian Government Research Training Program", role =c("fnd")),
    utils::person("VicHealth",role = c("fnd")),
    utils::person("Victoria University", role =c("fnd"))
    ),
  urls_chr = c("https://ready4-dev.github.io/ready4/",
               "https://github.com/ready4-dev/ready4",
               "https://www.ready4-dev.com/"))
user_manual_fns_chr <-  c(
  "get_datasts_tb","get_from_lup_obj","get_libraries_tb",
  "get_methods", "get_methods_tb", "get_modules_tb",
  "make_code_releases_tbl", "make_datasets_tb", "make_ds_releases_tbl",
  "make_methods_tb", "make_modules_tb",  "make_programs_tbl",
  "print_data", "print_methods", "print_modules", "print_packages",
  "write_to_copy_rmds",#"write_to_force_links_in",
  "write_to_render_post", "write_ws"
)
x <- x %>%
  ready4fun::make_manifest(addl_pkgs_ls = ready4fun::make_addl_pkgs_ls(#depends_chr = "generics",
                                                                       suggests_chr = "rmarkdown"),
                           build_ignore_ls = ready4fun::make_build_ignore_ls(file_nms_chr = c("initial_setup.R",
                                                                                              "CITATION.cff")), # New
                           check_type_1L_chr = "ready4",
                           cls_fn_ls = list(),
                           custom_dmt_ls = ready4fun::make_custom_dmt_ls(
                             desc_ls = list("get_datasts_tb() retrieves an RDS file (for example, a tabular summary of data collections that can be used with ready4 model modules) from a specified GitHub repository release.", #Get a table of ready4 model data collections
                                            "get_from_lup_obj() retrieves from a lookup table (a data.frame) the values in a target column for cases where values in a second column match a specified value.",
                                            "get_libraries_tb() retrieves a tabular summary of ready4 libraries that have been developed within a specified GitHub organisation.",
                                            "get_methods() retrieves the ready4 methods that are available for a specified ready4 model module.",
                                            "get_methods_tb() ingests 'methods_tb.RDS' (a table of methods associated with ready4 model modules) from a specified GitHub repository release.",
                                            "get_modules_tb() ingests 'modules_tb.RDS' (a table of ready4 model modules) from a specified GitHub repository release.",
                                            "make_code_releases_tbl() scrapes the details of a specified GitHub repository to generate a release history of ready libraries and executables. To work all repositories without any release need to be supplied using the 'exclude_chr' argument.",                                            "make_datasts_tb() function searches the contents of a specified Dataverse collection and returns a summary of the the data collections it contains.",
                                            "make_datasts_tb() scrapes metadata from a specified Dataverse collection to create a summary table of its contents. The contents table can detail either subsidiary data collections or individual datasets from those subsidiary data collections.",
                                            "make_ds_releases_tbl() scrapes metadata from Dataverse datasets for which a valid Digital Object Identifier (DOI) has been supplied to create a table summarising the entire release history of these datasets.",
                                            "make_methods_tb() scrapes the documentation websites of all libraries of ready4 modules in a specified GitHub organisation and then creates a tabular summary of vignette examples of ready4 module methods.",
                                            "make_modules_tb() scrapes the documentation websites of all libraries of ready4 modules in a specified GitHub organisation and then creates a tabular summary of the modules included in those libraries and vignette examples of their use.",
                                            "make_programs_tbl() scrapes the GitHub organisation and Zenodo community associated specified for a ready4 model implementation to create a tabular summary of programs and sub-routines associated with that implementation.",
                                            "print_data() formats the output of either get_datasts_tb() or make_datasts_tb() as HTML. The type of output can be customised to display Dataverse data collections or Dataverse datasets. Similarly output can be restricted to real or toy datasets.",
                                            "print_methods() formats the output of either get_methods_tb() or make_methods_tb() as HTML.",
                                            "print_modules() formats the output of either get_modules_tb() or make_modules_tb() as HTML.",
                                            "print_packages() formats the output of get_libraries_tb() as HTML.",
                                            "write_to_copy_rmds() is used to copy template RMD or Rmarkdown files to specified sub-directories of a model documentation website. These template copies can then be manually edited before being rendered with write_to_render_post().",
                                            #
                                            "write_to_render_post() is designed for help overcome practical challenges of rendering RMD or Rmarkdown files to Markdown output in a modelling project's Hugo Docsy documentation website. You must have 'hugodown' installed for this function to work.",
                                            "write_ws() creates a standardised directory structure as a local development environment for modelling projects developed with the ready4 framework."
                             ) %>% stats::setNames(user_manual_fns_chr),#[1:7]
                             title_ls = list("Get data from the release assets of a GitHub repository",
                                             "Get a value from a lookup table",
                                             "Get a table of ready4 libraries",
                                             "Get the methods associated with a ready4 model module",
                                             "Get a table of methods associated with ready4 model modules",
                                             "Get a table of ready4 model modules",
                                             "Make a tabular summary of release history of ready4 code libraries and executables",
                                             "Make a tabular summary of ready4 model data collections",
                                             "Make a tabular summary of release history of ready4 model data collections",
                                             "Make a tabular summary of methods associated with ready model modules",
                                             "Make a tabular summary of ready4 model modules and sub-modules",
                                             "Make a tabular summary of programs using ready4 model modules",
                                             "Print a table of ready4 model data collections",
                                             "Print a table of methods associated with ready4 model modules",
                                             "Print a table of ready4 model modules",
                                             "Print a table of ready4 libraries",
                                             "Write a local copy of RMD or Rmarkdown files",
                                             #
                                             "Write ready4 model documentation website page from an RMD or Rmarkdown file",
                                             "Write ready4 software develoment local directories"
                                             ) %>% stats::setNames(user_manual_fns_chr),
                             user_manual_fns_chr = user_manual_fns_chr),
                           copyright_holders_chr = "Orygen",
                           import_from_chr = NA_character_,
                           lifecycle_stage_1L_chr = "experimental",
                           path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/ready4-logo/default.png",
                           piggyback_to_1L_chr = "ready4-dev/ready4",
                           ready4_type_1L_chr = "foundation",
                           zenodo_badge_1L_chr = "[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5606250.svg)](https://doi.org/10.5281/zenodo.5606250)")
##
x <- write_self_srvc_pkg(x)
write_to_edit_workflow("pkgdown.yaml", consent_1L_chr = "Y") # In other packages, run for "test-coverage.yaml" as well.
# write_extra_pkgs_to_actions(consent_1L_chr = "Y")
usethis::use_package("pkgload", type = "Suggests") # ??
readLines("README.md") %>% # update in ready4fun
  # stringr::str_replace("\\(https://CRAN.R-project.org/package=ready4\\)","") %>%
  # stringr::str_replace("https://www.r-pkg.org/badges/version/ready4\\)]","https://www.r-pkg.org/badges/version/ready4\\)") %>%
  # stringr::str_replace("\\[!\\[CRAN status","\\![CRAN status") %>%
  stringr::str_replace("https://app.codecov","https://codecov") %>% # port edit to ready4fun
  # gsub(pattern = "arXiv:([^&]+)", replacement = "https://arxiv.org/abs/\\1") %>%
  writeLines(con = "README.md")
c(readLines("R/imp_fns.R"), # update in ready4fun
  " ",
  "#' NSE equals function",
  "#'",
  "#' Import of non standard evaluation equals function for use in dplyr calls.",
  "#'",
  "#' @importFrom rlang :=",
  "#' @name :=",
  "#' @rdname nseequals",
  "#' @export",
  "#' @keywords internal",
  "NULL",
  " ",
  "#' Dot Data function",
  "#'",
  "#' Import of .data function for use in dataset manipulation within functions.",
  "#'",
  "#' @importFrom rlang .data",
  "#' @name .data",
  "#' @rdname dotdata",
  "#' @export",
  "#' @keywords internal",
  "NULL"
) %>%
  writeLines("R/imp_fns.R")
write_examples(consent_1L_chr = "Y")
write_examples(consent_1L_chr = "Y", type_1L_chr = "r4")
devtools::build_vignettes()
#
#
# ADD DOI OVERRIDE FOR RELEASES
#
# write_extensions() # Required only if extensions have changed since last build
# write_badges() # Only run if the ready4fun badges table has been updated.
# Very occasional calls to write_housestyle_fls (four a year tops)



