#' Ready4Project
#'
#' A module of the ready4 framework that contains details of a modelling project.
#'
#' @include C4_Ready4Module.R
#' @slot badges_url_1L_chr Badges URL (a character vector of length one)
#' @slot dv_nm_1L_chr Dataverse name (a character vector of length one)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @slot dv_root_1L_chr Dataverse root (a character vector of length one)
#' @slot dv_server_1L_chr Dataverse server (a character vector of length one)
#' @slot dv_toy_data_1L_chr Dataverse toy dataset name (a character vector of length one)
#' @slot dvs_tb Dataverses (a tibble)
#' @slot extensions_tb Extensions (a tibble)
#' @slot filter_cdns_ls Filter conditions (a list)
#' @slot gh_org_1L_chr Github organisation (a character vector of length one)
#' @slot gh_repo_1L_chr Github repo (a character vector of length one)
#' @slot gh_repos_brochure_chr Github brochure websites (a character vector)
#' @slot gh_repos_exclude_chr Github repositories to exclude from GitHub organisation (a character vector)
#' @slot gh_repos_framework_chr Github repositories from GitHub organisation that contain framework libraries (a character vector)
#' @slot gh_repos_model_chr Github repositories from GitHub organisation that contain model libraries (a character vector)
#' @slot gh_repos_programs_chr Github repositories from GitHub organisation that contain programs (a character vector)
#' @slot gh_repos_subroutines_chr Github repositories from GitHub organisation that contain subroutines (a character vector)
#' @slot gh_tag_1L_chr Github release tag (a character vector of length one)
#' @slot gh_url_stub_1L_chr Github URL stub (a character vector of length one)
#' @slot libraries_chr Library names (a character vector)
#' @slot libraries_tb Library dataset (a tibble)
#' @slot methods_tb Methods dataset (a tibble)
#' @slot modules_tb Modules dataset (a tibble)
#' @slot sections_chr Model section names (a character vector)
#' @slot url_for_prj_1L_chr URL for project (a character vector  of length one)
#' @slot zenodo_1L_chr Zenodo repository (a character vector  of length one)
#' @slot dissemination_1L_chr Dissemination policy (a character vector  of length one)
#' @name Ready4Project-class
#' @rdname Ready4Project-class
#' @export Ready4Project
#' @exportClass Ready4Project
Ready4Project <- setClass("Ready4Project",
                          contains = "Ready4Module",
                          slots = c(#additions_tb = "tbl_df",
                            badges_url_1L_chr = "character",
                            dv_nm_1L_chr = "character",
                            dv_root_1L_chr = "character",
                            dv_server_1L_chr = "character",
                            dv_toy_data_1L_chr = "character",
                            dvs_tb = "tbl_df",
                            extensions_tb = "tbl_df",
                            filter_cdns_ls = "list",
                            gh_org_1L_chr = "character",
                            gh_repo_1L_chr = "character",
                            gh_repos_brochure_chr = "character",
                            gh_repos_exclude_chr = "character",
                            gh_repos_framework_chr ="character",
                            gh_repos_model_chr = "character",
                            gh_repos_programs_chr = "character",
                            gh_repos_subroutines_chr = "character",
                            gh_tag_1L_chr = "character",
                            gh_url_stub_1L_chr = "character",
                            libraries_chr = "character",
                            libraries_tb = "tbl_df",
                            methods_tb = "tbl_df",
                            modules_tb = "tbl_df",
                            sections_chr = "character",
                            url_for_prj_1L_chr = "character",
                            zenodo_1L_chr = "character",
                            dissemination_1L_chr = "character"),
                          prototype = list(#additions_tb = make_additions_tb(),
                            badges_url_1L_chr = character(0),#project_badges_url_1L_chr = "https://img.shields.io/badge/ready4"
                            dv_nm_1L_chr = character(0),#"ready4",
                            dv_root_1L_chr = character(0),#"https://dataverse.harvard.edu/dataverse/",# root_1L_chr - print_data
                            dv_server_1L_chr = "dataverse.harvard.edu", #server_1L_chr
                            dv_toy_data_1L_chr = "fakes", #toy_data_dv_1L_chr =
                            dvs_tb = tibble::tibble(), # get_datasets_tb()[0,],#NULL,
                            extensions_tb = tibble::tibble(), # get_cls_extensions(get_libraries_tb())[0,],# cls_extensions_tb = get_cls_extensions(get_libraries_tb()) # NULL,
                            filter_cdns_ls = list(),#NULL,
                            gh_org_1L_chr = character(0), # org_1L_chr = "ready4-dev",
                            gh_repo_1L_chr = character(0),#"ready4-dev/ready4",
                            gh_repos_brochure_chr = character(0), # brochure_repos_chr =
                            gh_repos_exclude_chr = character(0), #exclude_chr =
                            gh_repos_framework_chr = character(0), # framework_repos_chr
                            gh_repos_model_chr = character(0), # model_repos_chr
                            gh_repos_programs_chr = character(0), # program_repos_chr
                            gh_repos_subroutines_chr = character(0), # subroutine_repos_chr
                            gh_tag_1L_chr = "Documentation_0.0",
                            gh_url_stub_1L_chr = character(0), #url_stub_1L_chr = "https://ready4-dev.github.io/"
                            libraries_chr = character(0), # module_pkgs_chr = make_modules_pkgs_chr() model_repos_chr = character(0),
                            libraries_tb = tibble::tibble(), # make_libraries_tb(), #get_libraries_tb() # pkg_extensions_tb = NULL
                            #methods_chr = NULL, # print_methods - Can be derived from included Ready4Module
                            methods_tb = tibble::tibble(), # get_methods_tb(), #NULL, # print_methods
                            modules_tb = tibble::tibble(), # get_modules_tb()[0,], #### print_modules
                            url_for_prj_1L_chr = character(0),
                            zenodo_1L_chr = "ready4"
                          ))
