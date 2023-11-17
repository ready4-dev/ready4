library(magrittr)
library(lifecycle)
library(generics)
source("data-raw/FUNCTIONS.R") # Required to manage conflicts
#ready4fun::write_fn_type_dirs()
#dir.create("data-raw/examples")
x <- ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Implement Modular Health Economic Models" %>% tools::toTitleCase(),
                                                    pkg_desc_1L_chr = "A prototype software framework to support collaborative and modular implementation of health economic models. To help make health economic models more Transparent, Reusable And Updatable (TRU), 'ready4' provides a simple programming syntax, a template model module, and tools for documenting model implementations.
                                                    These foundational elements of the 'ready4' software framework are extended by other R packages. For detailed documentation about how to use 'ready4' and its extensions visit <https://www.ready4-dev.com/>.
                                                    For a background to and rationale for transparent, reusable and updatable computational health economic models read the manuscript <arXiv:2310.14138>.",
                                                    authors_prsn = c(utils::person(
                                                      given = "Matthew",family = "Hamilton", email =
                                                        "matthew.hamilton1@monash.edu",role = c("aut",
                                                                                                  "cre"),
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
  "make_code_releases_tbl", "make_datasts_tb", "make_ds_releases_tbl",
  "make_methods_tb", "make_modules_tb",  "make_programs_tbl",
  "print_data", "print_methods", "print_modules", "print_packages",
  "write_to_copy_rmds",#"write_to_force_links_in",
  "write_to_render_post", "write_ws"
)
x <- x %>%
  ready4fun::make_manifest(addl_pkgs_ls = ready4fun::make_addl_pkgs_ls(#depends_chr = "generics",
                                                                       suggests_chr = "rmarkdown"),
                           build_ignore_ls = ready4fun::make_build_ignore_ls(file_nms_chr = c("initial_setup.R")),
                           check_type_1L_chr = "ready4",
                           cls_fn_ls = list(),
                           custom_dmt_ls = ready4fun::make_custom_dmt_ls(
                             desc_ls = list("The `get_datasts_tb()` function retrieves an RDS file (for example, a tabular summary of data collections that can be used with ready4 model modules) from a specified GitHub repository release.", #Get a table of ready4 model data collections
                                            "From a `data.frame`, retrieve the values in a target column when the values in a second column match a specified value.",
                                            "The `get_libraries_tb()` function retrieves a tabular summary of ready4 libraries that have been developed within a specified GitHub organisation.",
                                            "The `get_methods()` function retrieves the ready4 methods that are available for a specified ready4 model module.",
                                            "The `get_methods_tb()` function ingests `methods_tb.RDS` (a table of methods associated with ready4 model modules) from a specified GitHub repository release.",
                                            "The `get_modules_tb()` function ingests `modules_tb.RDS` (a table of ready4 model modules) from a specified GitHub repository release.",
                                            "The `make_code_releases_tbl()` function scrapes the details of a specified GitHub repository to generate a release history of ready libraries and executables. To work all repositories without any release need to be supplied using the `exclude_chr` argument.",
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
## WARNING WARNING WARNING
## Unlike other workflows in the ready4 suite, in this instance it is necessary to answer "N", the FIRST time the following prompt appears:
## Are you sure that you want to delete the following files from your machine: [Y|N]
## After doing so, all other such prompts should be answered in the affirmative.
## If you make a mistake and write "Y" you will most likely have to delete your local copy of this repo and clone the repo again from its origin (https://github.com/ready4-dev/ready4).
x <- write_self_srvc_pkg(x)
# Check that the following corresponds with author.ready4fun_manifest
readLines(".github/workflows/R-CMD-check.yaml") %>%
  stringr::str_replace_all("r-lib/actions/setup-r@master", "r-lib/actions/setup-r@v2") %>%
  stringr::str_replace_all("r-lib/actions/setup-pandoc@master", "r-lib/actions/setup-pandoc@v2") %>%
  stringr::str_replace_all("- \\{os: windows-latest, r: '3.6'\\}", "#- \\{os: windows-latest, r: '3.6'\\}") %>%
  stringr::str_replace_all("- \\{os: ubuntu-20.04,   r: 'oldrel', ", "#- \\{os: ubuntu-20.04,   r: 'oldrel', ") %>%
  purrr::discard_at(2:4) %>%
  writeLines(con = ".github/workflows/R-CMD-check.yaml")
# Need to check that test-coverage includes fix: "Addresses issue with incompatibility between libcurl4-gnutls-dev and libcurl4-openssl-dev"
write_to_edit_workflow("pkgdown.yaml", consent_1L_chr = "Y") # In other packages, run for "test-coverage.yaml" as well.
write_extra_pkgs_to_actions(consent_1L_chr = "Y")
readLines("_pkgdown.yml") %>% # update in ready4fun
  stringr::str_replace_all("  - text: Model", "  - text: Framework & Model") %>%
  writeLines(con = "_pkgdown.yml")
readLines("DESCRIPTION")[-which(readLines("DESCRIPTION") %in% c("    knitr,", "    testthat,"))] %>%
  writeLines("DESCRIPTION") # update in ready4fun
devtools::document()
usethis::use_package("knitr", type = "Suggests") # update in ready4fun
usethis::use_package("pkgload", type = "Suggests") # ??
usethis::use_package("testthat", type = "Suggests") # update in ready4fun
citation_chr <- readLines("inst/CITATION") # update in ready4fun
citation_chr[3] <- stringr::str_replace(citation_chr[3], "citEntry", "bibentry")
citation_chr[4] <- stringr::str_replace(citation_chr[4], "entry", "bibtype")
citation_chr[8] <- stringr::str_replace(citation_chr[8], "2021", "2023")
citation_chr[12] <- stringr::str_replace(citation_chr[12], "2021", "2023")
citation_chr  %>%
  writeLines(con = "inst/CITATION")
readLines("README.md") %>% # update in ready4fun
  stringr::str_replace_all("svg\\)]\\(https://codecov.io","svg\\)]\\(https://app.codecov.io") %>%
  gsub(pattern = "arXiv:([^&]+)", replacement = "https://arxiv.org/abs/\\1") %>%
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
usethis::use_cran_badge() # Export to ready4fun
devtools::build_vignettes()
#
# Note currently first function title is appended to all function titles when using custom titles.
#
# ADD DOI OVERRIDE FOR RELEASES
#
# write_extensions() # Required only if extensions have changed since last build
# write_badges() # Only run if the ready4fun badges table has been updated.
# Very occasional calls to write_housestyle_fls (four a year tops)



