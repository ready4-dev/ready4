#' Ready4Project
#'
#' A module of the ready4 framework that contains details of a modelling project.
#'
#' @include C4_Ready4Module.R
#' @slot ds_tb Dataset (a tibble)
#' @slot dictionary_r3 Dictionary (a ready4 submodule)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @name Ready4Project-class
#' @rdname Ready4Project-class
#' @export Ready4Project
#' @exportClass Ready4Project
Ready4Project <- setClass("Ready4Project",
                          contains = "Ready4Module",
                          slots = c(additions_tb = "tbl_df",
                                    brochure_repos_chr = character(0),
                                    cls_extensions_tb = "tbl_df",
                                    dv_nm_1L_chr = "character",
                                    dv_root_1L_chr = "character",
                                    dvs_tb = "tbl_df",
                                    filter_cdns_ls = "list",
                                    server_1L_chr = "character",
                                    exclude_chr = "character",
                                    framework_repos_chr = "character",
                                    gh_repo_1L_chr = "character",
                                    gh_tag_1L_chr = "character",
                                    libraries_tb = "tbl_df",
                                    methods_tb = "tbl_df",
                                    module_pkgs_chr = "character",
                                    modules_tb = "tbl_df",
                                    org_1L_chr = "character",
                                    program_repos_chr = "character",
                                    project_badges_url_1L_chr = "character",
                                    repos_chr = "character",
                                    sections_chr = "character",
                                    subroutine_repos_chr = "character",
                                    toy_data_dv_1L_chr = "character",
                                    url_stub_1L_chr = "character",
                                    dissemination_1L_chr = "character"),
                        prototype = list(additions_tb = make_additions_tb(),
                                         brochure_repos_chr = character(0),
                                         cls_extensions_tb = get_cls_extensions(get_libraries_tb())[0,],# get_cls_extensions(get_libraries_tb()) # NULL,
                                         dv_nm_1L_chr = character(0),#"ready4",
                                         dv_root_1L_chr = character(0),#"https://dataverse.harvard.edu/dataverse/",# root_1L_chr - print_data
                                         dvs_tb = ready4::get_datasets_tb()[0,],#NULL,
                                         filter_cdns_ls = list(),#NULL,
                                         server_1L_chr = "dataverse.harvard.edu",
                                         exclude_chr = character(0),
                                         framework_repos_chr = character(0),
                                         gh_repo_1L_chr = character(0),#"ready4-dev/ready4",
                                         gh_tag_1L_chr = "Documentation_0.0",
                                         libraries_tb = make_libraries_tb(), #pkg_extensions_tb = NULL
                                         #methods_chr = NULL, # print_methods - Can be derived from included Ready4Module
                                         methods_tb = get_methods_tb(), #NULL, # print_methods
                                         module_pkgs_chr = character(0), # make_modules_pkgs_chr() model_repos_chr = character(0),
                                         modules_tb = ready4::get_modules_tb()[0,], #### print_modules
                                         org_1L_chr = character(0), # "ready4-dev",
                                         program_repos_chr = character(0),
                                         project_badges_url_1L_chr = character(0),#"https://img.shields.io/badge/ready4"
                                         repos_chr = character(0), #??
                                         sections_chr = character(0),
                                         subroutine_repos_chr = character(0),
                                         toy_data_dv_1L_chr = "fakes",
                                         url_stub_1L_chr = character(0))) #"https://ready4-dev.github.io/"
