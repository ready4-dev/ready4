make_additions_tb <- function(category_chr = character(0),
                              library_chr = character(0),
                              type_chr  = character(0),
                              url_stub_1L_chr = "https://ready4-dev.github.io/"){
  additions_tb <- tibble::tibble(library_chr = library_chr, category_chr = category_chr, type_chr = type_chr) %>%
    dplyr::mutate(Link = paste0(url_stub_1L_chr,library_chr,"/index.html"))
  return(additions_tb)
}
make_code_releases_tbl <- function(repo_type_1L_chr = "Framework",
                                   as_kbl_1L_lgl = T,
                                   brochure_repos_chr = character(0),
                                   exclude_chr = character(0),
                                   format_1L_chr = "%d-%b-%Y",
                                   framework_repos_chr = character(0),
                                   model_repos_chr = character(0),
                                   program_repos_chr = character(0),
                                   org_1L_chr = "ready4-dev",
                                   repos_chr = character(0),
                                   subroutine_repos_chr = character(0),
                                   tidy_desc_1L_lgl = T,
                                   url_stub_1L_chr = "https://ready4-dev.github.io/",
                                   ...){
  if(identical(brochure_repos_chr,character(0))){
    brochure_repos_chr <- "ready4web"
  }
  if(identical(exclude_chr,character(0))){
    exclude_chr <- "rebuild"
  }
  if(identical(framework_repos_chr,character(0))){
    framework_repos_chr <- make_framework_pkgs_chr()
  }
  if(identical(model_repos_chr,character(0))){
    model_repos_chr <- make_modules_pkgs_chr(what_chr = "all")
  }
  if(identical(subroutine_repos_chr,character(0))){
    subroutine_repos_chr <- make_subroutine_repos()
  }
  if(identical(program_repos_chr,character(0))){
    program_repos_chr <- setdiff(natmanager::list_repo(org_1L_chr),
                                 c(brochure_repos_chr, exclude_chr, framework_repos_chr, model_repos_chr, subroutine_repos_chr))
  }
  if(identical(repos_chr,character(0))){
    if(repo_type_1L_chr == "Framework"){
      repos_chr <- framework_repos_chr
    }
    if(repo_type_1L_chr == "Module"){
      repos_chr <- model_repos_chr
    }
    if(repo_type_1L_chr %in% c("Program","Subroutine","Program_and_Subroutine")){
      if(repo_type_1L_chr == "Subroutine"){
        repos_chr <- subroutine_repos_chr
      }
      if(repo_type_1L_chr == "Program"){
        repos_chr <- program_repos_chr
      }
      if(repo_type_1L_chr == "Program_and_Subroutine"){
        repos_chr <- c(program_repos_chr, subroutine_repos_chr)
      }
    }else{
      repo_type_1L_chr <- "Package"
    }
  }
  releases_xx <- repos_chr %>%
    purrr::map_dfr(~tidyRSS::tidyfeed(paste0("https://github.com/",org_1L_chr,"/",.x,"/releases.atom"))) %>%
    dplyr::arrange(dplyr::desc(entry_last_updated)) %>%
    dplyr::select(feed_title, entry_title, entry_last_updated, entry_content, entry_link) %>%
    dplyr::mutate(feed_title = feed_title %>% stringr::str_remove_all("Release notes from ")) %>%
    dplyr::rename(!!rlang::sym(repo_type_1L_chr) := feed_title,
                  Release = entry_title,
                  Date = entry_last_updated,
                  Description = entry_content,
                  URL = entry_link
    ) %>%
    dplyr::filter(Release != "Documentation_0.0")
  if(tidy_desc_1L_lgl){
    releases_xx <- releases_xx %>%
      dplyr::mutate(Description = Description %>% purrr::map2_chr(!!rlang::sym(repo_type_1L_chr),
                                                                  ~ stringr::str_remove(.x,paste0(.y,": "))))
  }
  if(as_kbl_1L_lgl){
    releases_xx <- releases_xx  %>%
      dplyr::mutate(Release = Release %>% stringr::str_remove_all("Release ") %>%
                      stringr::str_remove_all("v") %>%
                      kableExtra::cell_spec(format = "html", link = URL),
                    Date = Date %>% format.Date(format_1L_chr) %>% as.character()) %>%
      # dplyr::mutate(Release = cell_spec(row.names(.), "html", link = dt_url)) %>%
      dplyr::select(Date, !!rlang::sym(repo_type_1L_chr), Release, Description#dplyr::everything()
      )
    if(repo_type_1L_chr %in% c("Package","Module","Framework")){
      logos_chr <- purrr::map_chr(releases_xx %>% dplyr::pull(repo_type_1L_chr),
                                  ~paste0(url_stub_1L_chr, .x, "/logo.png"))
      releases_xx <- releases_xx %>%
        dplyr::mutate(!!rlang::sym(repo_type_1L_chr) := "")
      indx_1L_int <-which(names(releases_xx) %in% c("Package","Module","Framework"))
    }
    releases_xx <- releases_xx %>%
      kableExtra::kable("html", escape = FALSE) %>% # kableExtra::kbl() %>%
      kableExtra::kable_styling(...)   # kableExtra::kable_styling(...) %>%
    if(repo_type_1L_chr %in% c("Package","Module","Framework"))
      releases_xx <- releases_xx %>%
      kableExtra::column_spec(indx_1L_int,
                              image = kableExtra::spec_image(logos_chr,
                                                             height = 160, width = 160))
  }
  return(releases_xx)
}
make_datasets_tb <- function(dv_nm_1L_chr = "ready4",
                             key_1L_chr = NULL,
                             server_1L_chr = "dataverse.harvard.edu"){
  contents_ls <- dataverse::dataverse_contents(dv_nm_1L_chr,
                                               key = key_1L_chr,
                                               server = server_1L_chr)
  dv_ls <- contents_ls[contents_ls %>% purrr::map_lgl(~.x$type == "dataverse")]
  ds_ls <- contents_ls[contents_ls %>% purrr::map_lgl(~.x$type == "dataset")]
  if(identical(ds_ls,list())){
    ds_ls <- NULL
  }else{
    extra_dv_ls <- dataverse::get_dataverse(dv_nm_1L_chr,
                                            key = key_1L_chr,
                                            server = server_1L_chr)
    dv_ls <- append(extra_dv_ls,
                    dv_ls)
  }
  dvs_tb <- dv_ls %>%
    purrr::map_dfr(~{
      dv_ls <- dataverse::get_dataverse(.x,
                                        key = key_1L_chr,
                                        server = server_1L_chr)
      tb <- tibble::tibble(Dataverse = dv_ls$alias,
                           Name = dv_ls$name,
                           Description = dv_ls$description,
                           Creator = dv_ls$affiliation)
      tb %>%
        dplyr::mutate(Contents =  purrr::map(Dataverse,
                                             ~{
                                               dv_all_ls <- dataverse::dataverse_contents(.x,
                                                                                          key = key_1L_chr,
                                                                                          server = server_1L_chr)
                                               #dv_ls <- dv_all_ls[dv_all_ls %>% purrr::map_lgl(~.x$type == "dataverse")]
                                               dv_all_ls[dv_all_ls %>% purrr::map_lgl(~.x$type == "dataset")] %>%
                                                 purrr::map_chr(~if("persistentUrl" %in% names(.x)){
                                                   .x$persistentUrl
                                                 }else{
                                                   NA_character_
                                                 })
                                             }))
    })
  dvs_tb <- dvs_tb %>%
    dplyr::mutate(Datasets_Meta = Contents %>%
                    purrr::map(~.x %>%
                                 purrr::map(~ .x %>%
                                              dataverse::dataset_metadata(key = key_1L_chr,
                                                                          server = server_1L_chr) %>%
                                              tryCatch(error = function(e) "ERROR")))) %>%
    dplyr::mutate(Contents = Contents %>%
                    purrr::map2(Datasets_Meta,
                                ~ {
                                  entry_ls <- .x %>%
                                    purrr::map2(.y,
                                                ~ if(identical(.y, "ERROR")){
                                                  NA_character_
                                                }else{
                                                  .x
                                                }
                                    ) %>%
                                    purrr::discard(is.na)
                                  if(identical(entry_ls, list())){
                                    NA_character_
                                  }else{
                                    entry_ls  %>%
                                      purrr::flatten_chr()
                                  }
                                }
                    ))
  dvs_tb <- dvs_tb %>%
    dplyr::mutate(Datasets_Meta = Datasets_Meta %>%
                    purrr::map(~ {
                      entry_ls <- .x %>%
                        purrr::map(~ if(identical(.x, "ERROR")){
                          NULL
                        }else{
                          .x
                        }
                        ) %>%
                        purrr::compact()
                      if(identical(entry_ls, list())){
                        NULL
                      }else{
                        entry_ls
                      }
                    }
                    )) %>%
    dplyr::arrange(Dataverse)
  return(dvs_tb)
}
make_ds_releases_tbl <- function (ds_dois_chr,
                                  format_1L_chr = "%d-%b-%Y",
                                  server_1L_chr = "dataverse.harvard.edu",
                                  as_kbl_1L_lgl = T,
                                  ...)
{
  ds_releases_xx <- ds_dois_chr %>% purrr::map_dfr(~{
    meta_ls <- dataverse::dataset_versions(.x, server = server_1L_chr)
    doi_1L_chr <- .x
    1:length(meta_ls) %>%
      purrr::map_dfr(~tibble::tibble(Date = meta_ls[[.x]]$releaseTime,
                                     Dataset = meta_ls[[1]]$metadataBlocks$citation$fields[[1]]$value,
                                     DOI = paste0("https://doi.org/", doi_1L_chr),
                                     Version = paste0(meta_ls[[.x]]$versionNumber,
                                                      ".",
                                                      meta_ls[[.x]]$versionMinorNumber),
                                     `Number of files` = length(meta_ls[[1]]$files)))
  }) %>%
    dplyr::arrange(dplyr::desc(Date)) %>%
    dplyr::mutate(Date = Date %>%
                    format.Date(format_1L_chr) %>%
                    as.character()) %>%
    dplyr::filter(!is.na(Date))
  if (as_kbl_1L_lgl) {
    ds_releases_xx <- ds_releases_xx %>% dplyr::mutate(Dataset = Dataset %>%
                                                         kableExtra::cell_spec(format = "html", link = DOI)) %>%
      dplyr::select(Date, Dataset,
                    Version, `Number of files`)
    ds_releases_xx <- ds_releases_xx %>%
      kableExtra::kable("html", escape = FALSE) %>%
      kableExtra::kable_styling(...)

  }
  return(ds_releases_xx)
}
# make_ds_releases_tbl <- function(ds_dois_chr, # OLD - - DELETE ONCE CONFIRMED
#                                  format_1L_chr = "%d-%b-%Y",
#                                  server_1L_chr = "dataverse.harvard.edu"){
#   ds_dois_chr %>%
#     purrr::map_dfr(~{
#       meta_ls <- dataverse::dataset_versions(.x, server = server_1L_chr)
#       doi_1L_chr <- .x
#       1:length(meta_ls) %>%
#         purrr::map_dfr(~tibble::tibble(Date = meta_ls[[.x]]$releaseTime,
#                                        DOI = paste0("https://doi.org/",doi_1L_chr),
#                                        Version = paste0(meta_ls[[.x]]$versionNumber,".",meta_ls[[.x]]$versionMinorNumber),
#                                        `Number of files` = length(meta_ls[[1]]$files)))
#
#     }) %>%
#     dplyr::arrange(dplyr::desc(Date)) %>%
#     dplyr::mutate(Date = Date %>% format.Date(format_1L_chr) %>% as.character())
# }
make_dss_tb <- function(dvs_tb,
                        filter_cdns_ls = list(),
                        what_1L_chr = "all"){
  if(identical(filter_cdns_ls,list())){
    filter_cdns_ls <- list(real = 'Dataverse != "fakes"',
                           fakes = 'Dataverse == "fakes"',
                           people = 'Dataverse %in% c("TTU", "springtolife")',
                           places = 'Dataverse %in% c("springtides") | DOI == "https://doi.org/10.7910/DVN/JHSCDJ"')
  }
  dss_tb <- dvs_tb %>% dplyr::filter(!is.na(Contents)) %>%
    dplyr::select(Contents, Datasets_Meta, Dataverse) %>%
    purrr::pmap_dfr(~{
      ..2 %>% purrr::map_dfr(~{
        fields_ls <- .x$fields
        tibble::tibble(Title = fields_ls$value[which(fields_ls$typeName ==  "title")][[1]],
                       Description = fields_ls$value[which(fields_ls$typeName == "dsDescription")][[1]][[1]][[4]])
      }) %>%
        dplyr::mutate(Dataverse = ..3, DOI = ..1)
    })
  dss_tb <- purrr::reduce(1:length(filter_cdns_ls),.init = dss_tb,
                          ~ {
                            condition_1L_chr = filter_cdns_ls %>% purrr::pluck(.y)
                            if(names(filter_cdns_ls)[.y] == what_1L_chr){
                              .x %>% update_tb_r3(filter_cdn_1L_chr = condition_1L_chr)
                            }else{
                              .x
                            }
                          })
  return(dss_tb)
}
make_files_tb <- function(paths_to_dirs_chr, # Make output into a class? Make fn a mthd?
                          recode_ls,
                          inc_fl_types_chr = NA_character_){
  files_tb <- purrr::map_dfr(paths_to_dirs_chr,
                             ~{
                               files_chr_vec <- list.files(.x)
                               if(!identical(files_chr_vec,character(0))){
                                 tb <- tibble::tibble(dir_chr = rep(.x,length(files_chr_vec)),
                                                      file_chr = files_chr_vec %>%
                                                        purrr::map_chr(~stringr::str_sub(.x,
                                                                                         end = as.vector(stringi::stri_locate_last_regex(.x, "\\.")[,1])-1)),
                                                      file_type_chr = files_chr_vec %>%
                                                        purrr::map_chr(~stringr::str_sub(.x,
                                                                                         start = as.vector(stringi::stri_locate_last_regex(.x, "\\.")[,1]))))

                                 tb
                               }
                             })
  if(!is.na(inc_fl_types_chr))
    files_tb <- files_tb %>%
      dplyr::filter(file_type_chr %in% inc_fl_types_chr)
  files_tb <- files_tb %>%
    dplyr::filter(file_chr %in% names(recode_ls))
  description_chr <- purrr::map_chr(files_tb$file_chr,
                                    ~ {
                                      arg_ls <- append(list(EXPR=.x),recode_ls)
                                      rlang::exec(.fn = switch, !!!arg_ls)
                                    })
  files_tb <- files_tb %>%
    dplyr::mutate(description_chr = description_chr,
                  ds_file_ext_chr = purrr::map_chr(file_type_chr,
                                                   ~ ifelse(.x %in% c(".csv", ".xls",".xlsx"),
                                                            ".tab",
                                                            ".zip")))
  assertthat::are_equal(nrow(files_tb),
                        paste0(files_tb$file_chr,
                               files_tb$file_type_chr) %>%
                          unique() %>%
                          length())
  return(files_tb)
}
make_fn_defaults_ls <- function(fn){
  fn_defaults_ls <- as.list(args(fn))
  fn_defaults_ls <- fn_defaults_ls[fn_defaults_ls %>%
                                     purrr::map_lgl(~!identical(.x %>% deparse(),"deprecated()"))]
  fn_defaults_ls <- fn_defaults_ls[2:(length(fn_defaults_ls)-1)]
  return(fn_defaults_ls)
}
make_framework_pkgs_chr <- function(){
  framework_pkgs_chr <- c("ready4","ready4fun","ready4class","ready4pack","ready4use","ready4show")
  return(framework_pkgs_chr)
}
make_libraries_ls <- function(additions_tb = make_additions_tb(),
                              libraries_tb = NULL,
                              ns_var_nm_1L_chr = "pt_ns_chr"){
  if(is.null(libraries_tb)){
    libraries_ls <- NULL
  }else{
    names_chr <- libraries_tb$Section %>% unique()
    libraries_ls <- names_chr %>% purrr::map(~get_from_lup_obj(libraries_tb, match_var_nm_1L_chr = "Section", match_value_xx = .x,
                                                               target_var_nm_1L_chr = ns_var_nm_1L_chr)) %>% stats::setNames(names_chr)
    }
  libraries_ls <- update_libraries_ls(libraries_ls, additions_tb)
  return(libraries_ls)
}
make_libraries_tb <- function(additions_tb = make_additions_tb(),
                              include_1L_chr = "modules",
                              module_pkgs_chr = character(0),
                              ns_var_nm_1L_chr = "pt_ns_chr",
                              reference_var_nm_1L_chr = "Reference",
                              url_stub_1L_chr = "https://ready4-dev.github.io/",
                              vignette_var_nm_1L_chr = "Vignettes",
                              vignette_url_var_nm_1L_chr = "Vignettes_URLs",
                              what_chr = "all")
{
  if(identical(additions_tb,make_additions_tb())){
    additions_tb <- make_additions_tb("Framework","ready4",c("Foundation"))
    include_1L_chr <- "framework"
    empty_1L_lgl <- T
  }else{
    empty_1L_lgl <- F
  }
  if(include_1L_chr %in% c("framework","Framework") && !"Framework" %in% additions_tb$category_chr){
    stop("No framework library included in additions_tb")
  }
  # if(identical(libraries_ls, list())){
  #   libraries_ls <- get_libraries_ls(gh_repo_1L_chr = gh_repo_1L_chr,
  #                                    gh_tag_1L_chr = gh_tag_1L_chr)
  # }
  libraries_ls <- NULL
  libraries_ls <- update_libraries_ls(libraries_ls, additions_tb)
  # if(identical(section_case_whens_chr, character(0))){
  #   section_case_whens_chr <- paste0(ns_var_nm_1L_chr, c(
  #     ' %in% make_modules_pkgs_chr("people") ~ "People"', # replace with new get function
  #     ' %in% make_modules_pkgs_chr("places") ~ "Places"',
  #     ' %in% make_modules_pkgs_chr("platforms") ~ "Platforms"',
  #     ' %in% make_modules_pkgs_chr("programs") ~ "Programs"'
  #   ))
  # }
  # if(identical(type_case_whens_chr, character(0))){
  #   type_case_whens_chr <- paste0(ns_var_nm_1L_chr, c(
  #    ' == "ready4" ~ "Foundation"',
  #    ' == "ready4class" ~ "Authoring (code - classes)"',
  #    ' == "ready4fun" ~ "Authoring (code - functions)"',
  #    ' == "ready4pack" ~ "Authoring (code - libraries)"',
  #    ' == "ready4show" ~ "Authoring (code - programs)"',
  #    ' == "ready4use" ~ "Authoring (datasets)"',
  #    ' == "youthvars" ~ "Description (datasets)"',
  #    ' == "scorz" ~ "Description (variable scoring)"',
  #    ' == "specific" ~ "Modelling (inverse problems)"',
  #    ' == "heterodox" ~ "Modelling (heterogeneity)"',
  #    ' == "mychoice" ~ "Modelling (choice)"',
  #    ' == "TTU" ~ "Modelling (health utility)"',
  #    ' == "youthu" ~ "Prediction (health utility)"',
  #    ' == "vicinity" ~ "Modelling (spatial)"',
  #    ' == "aus" ~ "Modelling (Australian spatial)"'
  #   ))
  # }
  if(is.null(libraries_ls$Framework)){
    fw_chr <- character(0)#make_framework_pkgs_chr()
  }else{
    fw_chr <- libraries_ls$Framework
  }
  if(identical(module_pkgs_chr, character(0)))
    module_pkgs_chr <- setdiff(libraries_ls %>% purrr::flatten_chr(), fw_chr) %>% sort()
    #make_modules_pkgs_chr(what_chr)
  if (include_1L_chr %in% c("modules", "Modules")) {
    libraries_chr <- module_pkgs_chr
  }  else {
    if (include_1L_chr %in% c("framework","Framework")) {
      libraries_chr <- fw_chr
    }
    if(include_1L_chr %in% c("all","All"))
      libraries_chr <- c(fw_chr, module_pkgs_chr)
  }
  libraries_tb <- tibble::tibble(!!rlang::sym(ns_var_nm_1L_chr) := libraries_chr
                                      #Type = "",Section = ""
                                      ) %>%
    dplyr::mutate(Type = !!rlang::sym(ns_var_nm_1L_chr) %>%
                    purrr::map_chr(~get_from_lup_obj(additions_tb, match_var_nm_1L_chr = "library_chr", match_value_xx = .x, target_var_nm_1L_chr = "type_chr")),
                  Section = !!rlang::sym(ns_var_nm_1L_chr) %>%
                    purrr::map_chr(~get_from_lup_obj(additions_tb, match_var_nm_1L_chr = "library_chr", match_value_xx = .x, target_var_nm_1L_chr = "category_chr"))
                  )
  # libraries_tb <- purrr::reduce(1:2, .init = libraries_tb,
  #                                    ~ {
  #                                      case_whens_ls <- list(type_case_whens_chr, section_case_whens_chr)
  #                                      vars_chr <- c("Type", "Section")
  #                                      false_chr <- c("","Framework")
  #                                      .x %>%
  #                                      update_tb_r3(case_when_false_1L_chr = false_chr[.y],
  #                                                   case_when_true_ls = as.list(case_whens_ls %>% purrr::pluck(.y)) %>%
  #                                                     stats::setNames(rep(vars_chr[.y], length(case_whens_ls %>% purrr::pluck(.y)))),
  #                                                   case_when_var_1L_chr = vars_chr[.y])
  #                                      }
  #                                    )
  if(include_1L_chr %in% c("framework","Framework")){
    libraries_tb <- libraries_tb %>%
      dplyr::arrange(dplyr::desc(Type),
                     !!rlang::sym(ns_var_nm_1L_chr))
  }else{
    libraries_tb <- libraries_tb %>%
      dplyr::arrange(Section,
                     Type,
                     !!rlang::sym(ns_var_nm_1L_chr))
  }
  libraries_tb <- libraries_tb %>%
    dplyr::mutate(Link = purrr::map_chr(!!rlang::sym(ns_var_nm_1L_chr), ~paste0(url_stub_1L_chr,
                                                           .x, "/index", ".html"))) %>%
    dplyr::mutate(Library = kableExtra::cell_spec(!!rlang::sym(ns_var_nm_1L_chr),
                                                  "html", link = Link))
  libraries_tb <- add_vignette_links(libraries_tb,
                                          ns_var_nm_1L_chr = ns_var_nm_1L_chr,
                                          reference_var_nm_1L_chr = reference_var_nm_1L_chr,
                                          url_stub_1L_chr = url_stub_1L_chr,
                                          vignette_var_nm_1L_chr = vignette_var_nm_1L_chr,
                                          vignette_url_var_nm_1L_chr = vignette_url_var_nm_1L_chr)
  libraries_tb <- libraries_tb %>% dplyr::mutate(Citation = paste0(url_stub_1L_chr,
                                                                             !!rlang::sym(ns_var_nm_1L_chr), "/authors.html")) %>%
    dplyr::mutate(manual_urls_ls = purrr::map2(!!rlang::sym(ns_var_nm_1L_chr),
                                               Link, ~get_manual_urls(.x, pkg_url_1L_chr = .y))) %>%
    dplyr::mutate(code_urls_ls = purrr::map2(!!rlang::sym(ns_var_nm_1L_chr), Link,
                                             ~get_source_code_urls(.x, pkg_url_1L_chr = .y)))
  y_tb <- purrr::map_dfr(libraries_tb$Citation, ~{
    if (T) {
      f <- tempfile(fileext = ".bib")
      sink(f)
      writeLines(rvest::read_html(.x) %>% rvest::html_elements("pre") %>%
                   rvest::html_text2())
      sink(NULL)
      suppressWarnings(bib2df::bib2df(f)) %>% dplyr::select(AUTHOR,
                                                            TITLE, DOI)
    }
    else {
    }
  }) %>% dplyr::mutate(!!rlang::sym(ns_var_nm_1L_chr) := libraries_tb %>% dplyr::pull(!!rlang::sym(ns_var_nm_1L_chr))) %>%
    dplyr::rename(DOI_chr = DOI, Title = TITLE, Authors = AUTHOR)
  libraries_tb <- dplyr::left_join(libraries_tb,
                                        y_tb,
                                        by = ns_var_nm_1L_chr)
  if(empty_1L_lgl){
    libraries_tb <- libraries_tb %>% dplyr::filter(F)
  }
  return(libraries_tb)
}
make_list_phrase <- function(items_chr){
  list_phrase_1L_chr <- items_chr %>%
    stringr::str_c(sep="",collapse=", ") %>%
    stringi::stri_replace_last(fixed=",", replacement = " and")
  return(list_phrase_1L_chr)
}
make_local_path_to_dv_data <- function(save_dir_path_1L_chr,
                                       fl_nm_1L_chr,
                                       save_fmt_1L_chr){
  path_chr <- paste0(ifelse(save_dir_path_1L_chr!="",paste0(save_dir_path_1L_chr,"/"),""),
                     fl_nm_1L_chr,
                     save_fmt_1L_chr)
  return(path_chr)
}
make_methods_tb <- function(packages_tb  = NULL,
                            exclude_mthds_for_chr = NA_character_,
                            gh_repo_1L_chr = "ready4-dev/ready4",
                            gh_tag_1L_chr = "Documentation_0.0",
                            #include_1L_chr = "modules",
                            module_pkgs_chr = character(0), ##
                            ns_var_nm_1L_chr = "pt_ns_chr", ##
                            path_1L_chr = character(0),
                            #reference_var_nm_1L_chr = "Reference",
                            return_1L_chr = "all"#,
                            #url_stub_1L_chr = "https://ready4-dev.github.io/",
                            #vignette_var_nm_1L_chr = "Vignettes",
                            #vignette_url_var_nm_1L_chr = "Vignettes_URLs"
                            ){


  if(is.null(packages_tb)){
    packages_tb <- get_libraries_tb(gh_repo_1L_chr = gh_repo_1L_chr,
                                    gh_tag_1L_chr = gh_tag_1L_chr)
      # make_libraries_tb(include_1L_chr = include_1L_chr, # get_libraries_tb
      #                                module_pkgs_chr = module_pkgs_chr,
      #                                ns_var_nm_1L_chr = ns_var_nm_1L_chr,
      #                                reference_var_nm_1L_chr = reference_var_nm_1L_chr,
      #                                url_stub_1L_chr = url_stub_1L_chr,
      #                                vignette_var_nm_1L_chr = vignette_var_nm_1L_chr,
      #                                vignette_url_var_nm_1L_chr = vignette_url_var_nm_1L_chr)
  }
  if(!identical(module_pkgs_chr, character(0))){
    packages_tb <- dplyr::filter(packages_tb,
                                 !!rlang::sym(ns_var_nm_1L_chr) %in% module_pkgs_chr | Section == "Framework")
  }
  methods_tb <- tibble::tibble(Method = get_generics(exclude_mthds_for_chr = exclude_mthds_for_chr,
                                                     return_1L_chr = return_1L_chr),
                               Purpose = get_mthd_titles(Method,
                                                         path_1L_chr = path_1L_chr),
                               Examples =  purrr::map(Method,
                                                      ~ get_examples(packages_tb$Vignettes_URLs %>% purrr::flatten_chr() %>% unique() %>% purrr::discard(is.na),
                                                                     term_1L_chr = .x)))
  return(methods_tb)
}
make_modules_pkgs_chr <- function(what_chr = "all"){
  modules_pkgs_chr <- character(0)
  if("people" %in% what_chr | "all" %in% what_chr)
    modules_pkgs_chr <- c(modules_pkgs_chr,
                         c("youthvars","scorz","specific","TTU","youthu","mychoice","heterodox")) # Read from database on GH page
  if("places" %in% what_chr | "all" %in% what_chr)
    modules_pkgs_chr <- c(modules_pkgs_chr,
                         "vicinity","aus")
  if("platforms" %in% what_chr | "all" %in% what_chr)
    modules_pkgs_chr <- c(modules_pkgs_chr,
                         character(0))
  if("programs" %in% what_chr | "all" %in% what_chr)
    modules_pkgs_chr <- c(modules_pkgs_chr,
                         c("bimp"))
  return(modules_pkgs_chr)
}
make_modules_tb <- function (pkg_extensions_tb = NULL, cls_extensions_tb = NULL,
                             gh_repo_1L_chr = "ready4-dev/ready4", gh_tag_1L_chr = "Documentation_0.0",
                             module_pkgs_chr = character(0),
                             include_1L_chr = "modules",
                             ns_var_nm_1L_chr = "pt_ns_chr",
                             url_stub_1L_chr = "https://ready4-dev.github.io/", ##
                             what_chr = "all")
{
  if (is.null(pkg_extensions_tb)){
    pkg_extensions_tb <- get_libraries_tb(gh_repo_1L_chr = gh_repo_1L_chr,
                                          gh_tag_1L_chr = gh_tag_1L_chr)
  }
  # make_libraries_tb(include_1L_chr = include_1L_chr, #make_libraries_tb
      #                                      module_pkgs_chr = module_pkgs_chr,
      #                                      what_chr = what_chr)
  if(include_1L_chr %in% c("framework","Framework")){
    pkg_extensions_tb <- dplyr::filter(pkg_extensions_tb,
                                       Section == "Framework")
  }
  if(include_1L_chr %in% c("modules","Modules")){
    pkg_extensions_tb <- dplyr::filter(pkg_extensions_tb,
                                       Section != "Framework")
  }
  if(!identical(module_pkgs_chr, character(0))){
    pkg_extensions_tb <- dplyr::filter(pkg_extensions_tb, !!rlang::sym(ns_var_nm_1L_chr) %in% module_pkgs_chr)
  }
  if(!what_chr %in% c("all","All")){
    libraries_ls <- make_libraries_ls(libraries_tb = pkg_extensions_tb)
    include_int <- what_chr %>% purrr::map(~stringr::str_which(names(libraries_ls),stringr::regex(.x, ignore_case = TRUE))) %>% purrr::flatten_int()
    if(identical(include_int, integer(0))){
      pkg_extensions_tb <- dplyr::filter(pkg_extensions_tb, F)
    }else{
      pkg_extensions_tb <- dplyr::filter(pkg_extensions_tb, !!rlang::sym(ns_var_nm_1L_chr) %in% purrr::flatten_chr(libraries_ls[include_int]))
    }
  }
  if (is.null(cls_extensions_tb))
    cls_extensions_tb <- get_cls_extensions(pkg_extensions_tb, gh_repo_1L_chr = gh_repo_1L_chr, gh_tag_1L_chr = gh_tag_1L_chr,
                                            url_stub_1L_chr = url_stub_1L_chr, validate_1L_lgl = T)
  modules_tb <- dplyr::inner_join(cls_extensions_tb, pkg_extensions_tb, by = ns_var_nm_1L_chr) %>%
    dplyr::arrange(type_chr, old_class_lgl)
  order_int <- modules_tb$Reference %>% purrr::flatten_int() %>% unique() %>% purrr::discard(is.na)
  modules_tb <- modules_tb %>%
    dplyr::mutate(Reference = dplyr::case_when(!is.na(Reference) ~ purrr::map(Reference,
                                                                              ~ {
                                                                                new_int <- which(.x ==order_int)
                                                                                if(length(new_int)==1)
                                                                                  new_int <-  rep(new_int,2)
                                                                                new_int
                                                                              }),
                                               T ~ Reference)
    )
  order_int <- modules_tb$Vignettes_URLs %>% # Prefer to keep this if removing potential duplicate order_int
    purrr::map(~{
      if(is.na(.x[[1]])){
        NA_integer_#.x[[1]]
      }else{
        .x %>%
          purrr::map_int(~ stringr::str_sub(.x,start=-5) %>%
                           stringr::str_remove_all("</a>") %>%
                           as.numeric())
      }
    }) %>% purrr::flatten_int()  %>%
    purrr::discard(is.na) %>% unique()
  modules_tb <- modules_tb %>%
    dplyr::mutate(Vignettes_URLs = dplyr::case_when(!is.na(Reference) ~ purrr::map(Vignettes_URLs,
                                                                                   ~ {
                                                                                     if(is.na(.x[1])){
                                                                                       new_chr <-  NA_character_
                                                                                     }else{
                                                                                       old_int <- .x %>%
                                                                                         purrr::map_int(~ {
                                                                                           start_1L_int <- 1 + (.x %>% stringr::str_locate("\"     \" >") %>% purrr::pluck(2))
                                                                                           stringr::str_sub(.x,start=start_1L_int) %>%
                                                                                             stringr::str_remove_all("</a>") %>%
                                                                                             as.numeric()})
                                                                                       #new_int <- which(old_int ==order_int)
                                                                                       new_chr <- .x %>%
                                                                                         purrr::map2_chr(old_int,
                                                                                                         ~{
                                                                                                           end_1L_int <- (.x %>% stringr::str_locate("\"     \" >") %>% purrr::pluck(2))
                                                                                                           paste0(stringr::str_sub(.x,end=end_1L_int),
                                                                                                                  which(.y==order_int),
                                                                                                                  "</a>")
                                                                                                         })
                                                                                     }

                                                                                     # if(length(new_int)==1)
                                                                                     #   new_int <-  rep(new_int,2)
                                                                                     new_chr
                                                                                   }),
                                                    T ~ Vignettes_URLs)
    )
  modules_tb <- modules_tb %>%
    dplyr::mutate(Class = purrr::pmap(list(!!rlang::sym(ns_var_nm_1L_chr),
                                           type_chr, old_class_lgl), ~{
                                             kableExtra::cell_spec(..2, "html", link = paste0(url_stub_1L_chr,
                                                                                              ..1, "/reference/", ifelse(..3, ..2, paste0(..2,
                                                                                                                                          "-class")), ".html"))
                                           })) %>% dplyr::mutate(Examples = purrr::map2(Vignettes_URLs,
                                                                                        type_chr, ~get_examples(.x, term_1L_chr = .y)))

  modules_tb <- modules_tb %>% dplyr::mutate(Description = purrr::map2_chr(Class,
                                                                           old_class_lgl, ~{
                                                                             rvest::read_html((.x %>% stringr::str_match("href=\"\\s*(.*?)\\s*\" style"))[,
                                                                                                                                                          2]) %>% rvest::html_elements(ifelse(.y, "h1",
                                                                                                                                                                                              "p")) %>% rvest::html_text2() %>% purrr::pluck(1)
                                                                           }) %>% stringi::stri_replace_last_regex("\\.", "")) %>%
    dplyr::select(Class, Description, Library, Examples,
                  old_class_lgl)
  return(modules_tb)
}
make_programs_tbl <- function(what_1L_chr = "Program",
                              as_kbl_1L_lgl = F,
                              exclude_chr = "dce_sa_cards",
                              format_1L_chr = "%d-%b-%Y",
                              tidy_desc_1L_lgl = T,
                              url_stub_1L_chr = "https://ready4-dev.github.io/",
                              zenodo_1L_chr = "ready4",
                              ...){
    programs_xx <- make_code_releases_tbl(what_1L_chr, as_kbl_1L_lgl = F, exclude_chr = exclude_chr,
                                          tidy_desc_1L_lgl = F, url_stub_1L_chr = url_stub_1L_chr) %>%
    dplyr::group_by(!!rlang::sym(what_1L_chr)) %>%
    dplyr::filter(dplyr::row_number()==1) %>%
    dplyr::arrange(!!rlang::sym(what_1L_chr)) %>%
    dplyr::ungroup()
  zenodo_records_ls <- zen4R::ZenodoManager$new()
  zenodo_records_ls <- zenodo_records_ls$getRecords(q = paste0("communities:(",zenodo_1L_chr,")"))
  descriptions_chr <- zenodo_records_ls %>%
    purrr::map_chr(~rvest::html_text(rvest::read_html(.x$metadata$description %>%
                                                        stringr::str_remove_all("&nbsp;"))) %>%
                     stringr::str_replace_all("[\r\n]" , ""))
  indices_int <- programs_xx$Description %>% purrr::map_int(~which(.x == descriptions_chr))
  programs_xx$DOI <- indices_int %>%
    purrr::map_chr(~zenodo_records_ls[[.x]]$links$conceptdoi)
  programs_xx$GitHub <- gsub("/releases/.*","",programs_xx$URL)
  if(tidy_desc_1L_lgl)
    programs_xx <- programs_xx %>%
    dplyr::mutate(Description = Description %>%
                    purrr::map2_chr(!!rlang::sym(what_1L_chr),
                                    ~ stringr::str_remove(.x,paste0(.y,": "))))
  if(as_kbl_1L_lgl){
    programs_xx <- programs_xx  %>%
      dplyr::mutate(Release = Release %>% stringr::str_remove_all("Release ") %>%
                      stringr::str_remove_all("v"),
                    Date = Date %>% format.Date(format_1L_chr) %>% as.character()) %>%
      dplyr::mutate(Source = purrr::pmap(list(GitHub, DOI),
                                         ~{
                                           kableExtra::cell_spec(c("Dev", "Archive"),
                                                                 format = "html",
                                                                 link = c(..1, ..2))
                                         }
      )) %>%
      dplyr::select(!!rlang::sym(what_1L_chr), Release, Date, Description, Source)
    programs_xx <- programs_xx %>%
      kableExtra::kable("html", escape = FALSE) %>%
      kableExtra::kable_styling(...)
  }
  return(programs_xx)
}
make_prompt <- function(prompt_1L_chr, options_chr = NULL, force_from_opts_1L_chr = F) {
  acknowledgement_1L_chr <- "This function is based on: https://debruine.github.io/posts/interactive-test/"
  con_conn <- getOption("prompt_opts.con", stdin())
  options_1L_chr <- paste(options_chr, collapse = "|")
  prompt_with_options_1L_chr <- paste0(prompt_1L_chr, " [", options_1L_chr, "]\n")
  cat(prompt_with_options_1L_chr)
  response_1L_chr <- readLines(con = con_conn, n = 1)
  if (!is.null(options_chr) & !response_1L_chr %in% options_chr & force_from_opts_1L_chr) {
    response_1L_chr  <- make_prompt(prompt_1L_chr, options_chr, force_from_opts_1L_chr = T)
  }
  return(response_1L_chr)
}
make_subroutine_repos <- function(){
  subroutine_repos_chr <- c("mychoice_results" ,"ttu_mdl_ctlg","ms_tmpl","ttu_lng_ss")
  return(subroutine_repos_chr)
}
# make_gnrc_imports <- function(){
#   generics_chr <- methods::getGenerics("package:ready4")@.Data
#     # c(
#     # "author","authorClasses","authorData","authorFunctions",
#     # "characterize","enhance","ingest","investigate","manufacture",
#     # "metamorphose","procure","prognosticate",
#     # "reckon","renew","ratify","report","share")
#   gnrc_imports_chr <- rep("ready4",length(generics_chr)) %>% stats::setNames(generics_chr)
#   more_generics_chr <- ls("package:generics")
#   more_generics_chr <- setdiff(more_generics_chr, generics_chr)
#   gnrc_imports_chr <- c(gnrc_imports_chr,
#                         rep("generics",length(more_generics_chr)) %>% stats::setNames(more_generics_chr))
#
#   return(gnrc_imports_chr)
# }
