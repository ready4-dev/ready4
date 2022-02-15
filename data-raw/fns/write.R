write_all_tbs_in_tbs_r4_to_csvs <- function(tbs_r4,
                                            r4_name_1L_chr,
                                            lup_dir_1L_chr,
                                            pfx_1L_chr){
  purrr::walk(methods::getSlots(r4_name_1L_chr) %>% names(),
              ~ write_tb_to_csv(tbs_r4 = tbs_r4,
                                slot_nm_1L_chr = .x,
                                r4_name_1L_chr = r4_name_1L_chr,
                                lup_dir_1L_chr = lup_dir_1L_chr,
                                pfx_1L_chr = pfx_1L_chr))
}
write_citation_cff <- function(pkg_desc_ls,
                               citation_chr,
                               publisher_1L_chr = "Zenodo"){
  meta <- pkg_desc_ls
  authors_ls <- parse(text=paste0("list(",
                                  citation_chr[startsWith(citation_chr,"  author   = ")] %>%
                                    stringr::str_sub(start=16, end=-3) %>%
                                    stringr::str_replace_all("person","c"),
                                  ")")) %>%
    eval()
  citation_cff_chr <- c("cff-version: 1.2.0",
                        paste0("message: \"",
                               parse(text = citation_chr[which(citation_chr %>%
                                                                 startsWith("  textVersion = ")):(length(citation_chr)-1)] %>%
                                       stringr::str_remove("  textVersion = ") %>%
                                       paste0(collapse = "\n")) %>%
                                 eval() %>%
                                 stringr::str_replace(paste0(meta$Version,"."),
                                                      paste0(meta$Version,". ",publisher_1L_chr,".")),
                               "\""),
                        "authors:",
                        purrr::map_chr(authors_ls,
                                        #1:length(authors_ls),
                                        ~ paste0("  - family-names: \"",.x[2],"\"\n",
                                                 "    given-names: \"",.x[1],"\""#ifelse(.y==length(authors_ls),"\"","\"\n")#"    orcid: ",
                                        )),
                        paste0("title: \"",paste0(meta$Package,": ",meta$Title),"\""),
                        paste0("version: ",meta$Version),
                        paste0("doi: ",citation_chr[startsWith(citation_chr,"  doi      = ")] %>% stringr::str_sub(start=15,end=-3)),
                        paste0("date-released: ",Sys.Date()),
                        paste0("url: \"",citation_chr[startsWith(citation_chr,"  url      = ")] %>% stringr::str_sub(start=15,end=-3),"\""))
  write_new_files("CITATION.cff",
                  fl_nm_1L_chr = "CITATION",
                  text_ls = list(citation_cff_chr))

}
write_dv_fl_to_loc <- function(ds_ui_1L_chr,
                               fl_nm_1L_chr = NA_character_,
                               fl_id_1L_int = NA_integer_,
                               repo_fl_fmt_1L_chr,
                               key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                               server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
                               save_type_1L_chr = "original",
                               dest_path_1L_chr,
                               consent_1L_chr = ""){
  ds_ls <- dataverse::get_dataset(ds_ui_1L_chr)
  if(ds_ls$versionState != "DRAFT"){
    if(!is.na(fl_id_1L_int)){
      ds_ui_1L_chr <- NULL
    }
    if(!consent_1L_chr %in% c("Y", "N")){
      consent_1L_chr <- make_prompt(prompt_1L_chr=paste0("Do you confirm ('Y') that you want to write the file ",
                                                         paste0(fl_nm_1L_chr,repo_fl_fmt_1L_chr),
                                                         " to ",
                                                         dest_path_1L_chr),
                                    options_chr = c("Y", "N"),
                                    force_from_opts_1L_chr = T)
    }
    if(consent_1L_chr %in% c("Y")){
      writeBin(dataverse::get_file(ifelse(is.na(fl_id_1L_int),
                                          paste0(fl_nm_1L_chr,repo_fl_fmt_1L_chr),
                                          fl_id_1L_int),
                                   dataset = ds_ui_1L_chr,
                                   format = save_type_1L_chr,
                                   key = key_1L_chr,
                                   server = server_1L_chr),
               dest_path_1L_chr)
      message(paste0("New file created in ",
                     dest_path_1L_chr,
                     " :\n",
                     paste0(fl_nm_1L_chr,repo_fl_fmt_1L_chr)))
    }
  }else{
    warning("Cannot write local copy of files from private Dataverse repo")
  }
}
write_env_objs_to_dv <- function(env_objects_ls,
                                 descriptions_chr,
                                 ds_url_1L_chr,
                                 key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                                 publish_dv_1L_lgl = F,
                                 piggyback_desc_1L_chr = "Documentation",
                                 piggyback_tag_1L_chr = "Documentation_0.0",
                                 piggyback_to_1L_chr = character(0),
                                 prerelease_1L_lgl = T,
                                 server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  tmp_dir <- tempdir()
  paths_chr <- env_objects_ls %>%
    purrr::map2_chr(names(env_objects_ls),
                    ~{
                      path_1L_chr <- ifelse(is.null(.x),
                                            NA_character_,
                                            paste0(tmp_dir,"/",.y,".RDS"))
                      if(!is.na(path_1L_chr)){
                        saveRDS(object = .x,
                                file = path_1L_chr)
                      }
                      path_1L_chr
                    })
  descriptions_chr <- descriptions_chr[!is.na(paths_chr)]
  paths_chr <- paths_chr[!is.na(paths_chr)]
  if(identical(piggyback_to_1L_chr,character(0))){
    ds_ls <- dataverse::get_dataset(ds_url_1L_chr)
  }else{
    ds_ls <- NULL
  }
  file_ids_int <- write_fls_to_repo(paths_chr,
                                    descriptions_chr = descriptions_chr,
                                    ds_url_1L_chr = ds_url_1L_chr,
                                    ds_ls = ds_ls,
                                    key_1L_chr = key_1L_chr,
                                    piggyback_desc_1L_chr = piggyback_desc_1L_chr,
                                    piggyback_tag_1L_chr = piggyback_tag_1L_chr,
                                    piggyback_to_1L_chr = piggyback_to_1L_chr,
                                    prerelease_1L_lgl = prerelease_1L_lgl,
                                    server_1L_chr = server_1L_chr)
  do.call(file.remove, list(paths_chr))
  unlink(tmp_dir)
  if(publish_dv_1L_lgl & identical(piggyback_to_1L_chr,character(0))){
    write_to_publish_dv_ds(dv_ds_1L_chr = ds_url_1L_chr)
  }
  return(file_ids_int)
}
write_fls_from_dv <- function(files_tb,
                            fl_ids_int,
                            ds_url_1L_chr,
                            local_dv_dir_1L_chr,
                            key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                            server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
                            consent_1L_chr = ""){
  if(!consent_1L_chr %in% c("Y", "N")){
    consent_1L_chr <- make_prompt(prompt_1L_chr=paste0("Do you confirm ('Y') that you want to write the file ",
                                                       ifelse(length(file_paths_chr)>1,"s",""),
                                                       " to ",
                                                       local_dv_dir_1L_chr),
                                  options_chr = c("Y", "N"),
                                  force_from_opts_1L_chr = T)
  }
  if(consent_1L_chr %in% c("Y")){
    purrr::walk(1:length(fl_ids_int),
                ~{
                  if(!(ds_ls$versionState=="DRAFT" | files_tb$file_type_chr[.x]==".zip")){
                    write_dv_fl_to_loc(ds_ui_1L_chr = ds_url_1L_chr,
                                       fl_nm_1L_chr = files_tb$file_chr[.x],
                                       fl_id_1L_int = fl_ids_int[.x],
                                       repo_fl_fmt_1L_chr = files_tb$ds_file_ext_chr[.x],
                                       key_1L_chr = key_1L_chr,
                                       server_1L_chr = server_1L_chr,
                                       save_type_1L_chr = "original",
                                       dest_path_1L_chr = make_local_path_to_dv_data(save_dir_path_chr = local_dv_dir_1L_chr,
                                                                                     filename_chr = files_tb$file_chr[.x],
                                                                                     save_format_chr = files_tb$file_type_chr[.x]),
                                       consent_1L_chr = consent_1L_chr)
                  }
                })
  }
}
write_fls_to_dv <- function(file_paths_chr,
                            descriptions_chr = NULL,
                            ds_url_1L_chr,
                            ds_ls = NULL,
                            key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                            server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  if(!identical(file_paths_chr, character(0))){
    message(paste0("Are you sure that you want to upload the following file",
                   ifelse(length(file_paths_chr)>1,"s",""),
                   " to dataverse ",
                   ds_url_1L_chr,
                   ": \n",
                   file_paths_chr %>%
                     purrr::map_chr(~fs::path_file(.x)) %>%
                     paste0(collapse = "\n"),
                   "?"))
    consent_1L_chr <- make_prompt(prompt_1L_chr = paste0("Type 'Y' to confirm that you want to upload ",
                                                         ifelse(length(file_paths_chr)>1,
                                                                "these files:",
                                                                "this file:")),
                                  options_chr = c("Y", "N"),
                                  force_from_opts_1L_chr = T)
    if(consent_1L_chr == "Y"){
      if(is.null(ds_ls))
        ds_ls <- dataverse::get_dataset(ds_url_1L_chr)
      is_draft_1L_lgl <- ds_ls$versionState == "DRAFT"
      nms_chr <- ds_ls$files$filename
      if(is.null(descriptions_chr))
        descriptions_chr <- purrr::map(file_paths_chr,
                                       ~ NULL)
      ids_int <- file_paths_chr %>%
        purrr::map2_int(descriptions_chr,
                        ~{
                          fl_nm_1L_chr <- fs::path_file(.x)
                          if (fl_nm_1L_chr %in% nms_chr) {
                            id_1L_int <- get_fl_id_from_dv_ls(ds_ls,
                                                              fl_nm_1L_chr = fl_nm_1L_chr,
                                                              nms_chr = nms_chr)
                            if (is_draft_1L_lgl) {
                              id_1L_int %>% dataverse::delete_file()
                              id_1L_int <- dataverse::add_dataset_file(file = .x,
                                                                       dataset = ds_url_1L_chr,
                                                                       description = .y,
                                                                       key = key_1L_chr,
                                                                       server = server_1L_chr)
                            }else {
                              dataverse::update_dataset_file(file = .x,
                                                             dataset = ds_url_1L_chr,
                                                             id = id_1L_int,
                                                             force = T,
                                                             description = .y,
                                                             key = key_1L_chr,
                                                             server = server_1L_chr)
                            }
                          }else {
                            id_1L_int <- dataverse::add_dataset_file(file = .x,
                                                                     dataset = ds_url_1L_chr,
                                                                     description = .y,
                                                                     key = key_1L_chr,
                                                                     server = server_1L_chr)
                          }
                          id_1L_int
                        })
    }else{
      ids_int <- NULL
    }
  }else{
    ids_int <- NULL
  }
  return(ids_int)
}
write_fls_to_repo <- function(paths_chr,
                              descriptions_chr,
                              ds_url_1L_chr = character(0),
                              ds_ls = NULL,
                              key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                              server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
                              piggyback_desc_1L_chr = "Documentation",
                              piggyback_tag_1L_chr = "Documentation_0.0",
                              piggyback_to_1L_chr = character(0),
                              prerelease_1L_lgl = T){
  if(!identical(piggyback_to_1L_chr,character(0))){
    releases_df <- piggyback::pb_list(repo = piggyback_to_1L_chr)
    if(!piggyback_tag_1L_chr %in% releases_df$tag)
      piggyback::pb_new_release(piggyback_to_1L_chr,
                                tag = piggyback_tag_1L_chr,
                                body = piggyback_desc_1L_chr,
                                prerelease = prerelease_1L_lgl)
    purrr::walk(paths_chr,
                ~ {
                  if(file.exists(.x)){
                  piggyback::pb_upload(.x,
                                       repo = piggyback_to_1L_chr,
                                       #overwrite = T,
                                       tag = piggyback_tag_1L_chr)
                  }
                  }
                )
    ids_int <- NULL
  }else{
    if(!identical(character(0),ds_url_1L_chr))
      ids_int <- write_fls_to_dv(paths_chr,
                                 descriptions_chr = descriptions_chr,
                                 ds_url_1L_chr = ds_url_1L_chr,
                                 ds_ls = ds_ls,
                                 key_1L_chr = key_1L_chr,
                                 server_1L_chr = server_1L_chr)
  }
  return(ids_int)
}
write_from_tmp <- function(tmp_paths_chr,
                           dest_paths_chr,
                           edit_fn_ls = list(NULL),
                           args_ls_ls = list(NULL)){
  text_ls <- purrr::pmap(list(tmp_paths_chr,
                              edit_fn_ls,
                              args_ls_ls),
                         ~{
                           fileConn <- file(..1)
                           txt_chr <- readLines(fileConn, warn=FALSE)
                           close(fileConn)
                           if(is.null(..2)){
                             edit_fn <- function(x){x}
                           }else{
                             edit_fn <- ..2
                           }
                           rlang::exec(edit_fn, txt_chr, !!!..3)
                         })
  write_to_delete_fls(intersect(tmp_paths_chr,dest_paths_chr))
  write_new_files(dest_paths_chr,
                  text_ls = text_ls)
}
write_new_dirs <- function(new_dirs_chr){
  new_dirs_chr <- new_dirs_chr[new_dirs_chr %>% purrr::map_lgl(~!dir.exists(.x))]
  if(!identical(new_dirs_chr, character(0))){
    message(paste0("Are you sure that you want to write the following director",
                   ifelse(length(new_dirs_chr)>1,"ies","y"),
                   " to your machine? \n",
                   new_dirs_chr %>% paste0(collapse = "\n")))
    consent_1L_chr <- make_prompt(prompt_1L_chr=paste0("Do you confirm ('Y') that you want to write ",
                                                       ifelse(length(new_dirs_chr)>1,
                                                              "these directories?",
                                                              "this directory?")),
                                  options_chr = c("Y", "N"),
                                  force_from_opts_1L_chr = T)
    if(consent_1L_chr %in% c("Y")){
      paths_ls <- new_dirs_chr %>% purrr::walk(~{
        dir.create(.x)
      })
      message(paste0("New directories created:\n", new_dirs_chr %>% paste0(collapse = "\n")))
    }else{
      message("Write request cancelled - no new directories created")
    }
  }
}
write_new_files <- function(paths_chr,
                            custom_write_ls = NULL,
                            fl_nm_1L_chr = NULL,
                            source_paths_ls = NULL,
                            text_ls = NULL){
  if(!is.null(source_paths_ls)){
    dest_dir_1L_chr <- paths_chr
    paths_chr <- purrr::map(source_paths_ls,
                            ~{
                              if(dir.exists(.x)){
                                list.files(.x,
                                           recursive = T)
                              }else{
                                fs::path_file(.x)
                              }
                            }) %>%
      purrr::flatten_chr() %>%
      purrr::map_chr(~paste0(dest_dir_1L_chr,
                             "/",
                             ifelse(is.null(fl_nm_1L_chr),
                                    .x,
                                    fl_nm_1L_chr)
      ))
    recursive_1L_lgl <- ifelse(source_paths_ls %>% purrr::map_lgl(~dir.exists(.x)) %>% any(),
                               T,
                               F)
  }
  new_files_chr <- paths_chr[paths_chr %>% purrr::map_lgl(~!file.exists(.x))]
  overwritten_files_chr <- setdiff(paths_chr, new_files_chr)
  if(!identical(paths_chr, character(0))){
    message(paste0("Are you sure that you want to write / overwrite the following file",
                   ifelse(length(paths_chr)>1,"s",""),
                   " to your machine: \n",
                   ifelse(identical(new_files_chr, character(0)),
                          "",
                          paste0("Files that will be created: \n",
                                 new_files_chr %>% paste0(collapse = "\n"))),
                   ifelse(identical(overwritten_files_chr, character(0)),
                          "",
                          paste0("Files that will be overwritten: \n",
                                 overwritten_files_chr %>% paste0(collapse = "\n"))),
                   "?"))
    consent_1L_chr <- make_prompt(prompt_1L_chr=paste0("Do you confirm ('Y') that you want to write ",
                                                       ifelse((length(new_files_chr) + length(overwritten_files_chr))>1,
                                                              "these files:",
                                                              "this file:")),
                                  options_chr = c("Y", "N"),
                                  force_from_opts_1L_chr = T)
    if(consent_1L_chr %in% c("Y")){
      if(!is.null(text_ls)){
        purrr::walk2(paths_chr,
                     text_ls,
                     ~ {
                       file_conn <- file(.x)
                       writeLines(.y, file_conn)
                       close(file_conn)
                     })
      }else{
        if(!is.null(source_paths_ls)){
          source_paths_chr <- purrr::map(source_paths_ls,
                                         ~{
                                           if(dir.exists(.x)){
                                             list.files(.x, full.names = T)
                                           }else{
                                             .x
                                           }
                                         }) %>%
            purrr::flatten_chr()
          purrr::walk(source_paths_chr,
                      ~ file.copy(.x,
                                  paste0(dest_dir_1L_chr,
                                         ifelse(is.null(fl_nm_1L_chr),
                                                "",
                                                paste0("/",fl_nm_1L_chr))),
                                  overwrite = T,
                                  recursive = recursive_1L_lgl))
        }
        if(!is.null(custom_write_ls)){
          if("consent_1L_chr" %in% formalArgs(custom_write_ls$fn))
            custom_write_ls$args_ls$consent_1L_chr <- consent_1L_chr
          rlang::exec(custom_write_ls$fn,
                      !!!custom_write_ls$args_ls)
        }
      }
    }else{
      message("Write request cancelled - no new directories created")
    }
  }
}
write_tb_to_csv <- function(tbs_r4,
                            slot_nm_1L_chr,
                            r4_name_1L_chr,
                            lup_dir_1L_chr,
                            pfx_1L_chr,
                            consent_1L_chr = ""){
  file_path_1L_chr <- paste0(lup_dir_1L_chr,"/",pfx_1L_chr,"_",slot_nm_1L_chr,".csv")
  if(!consent_1L_chr %in% c("Y", "N")){
    consent_1L_chr <- make_prompt(prompt_1L_chr=paste0("Do you confirm ('Y') that you want to write the file ",
                                                       file_path_1L_chr,
                                                       " ? "),
                                  options_chr = c("Y", "N"),
                                  force_from_opts_1L_chr = T)
  }
  if(consent_1L_chr %in% c("Y")){
    methods::slot(tbs_r4,slot_nm_1L_chr) %>%
      dplyr::mutate_if(is.list,.funs = dplyr::funs(ifelse(stringr::str_c(.)=="NULL",NA_character_ , stringr::str_c (.)))) %>%
      utils::write.csv(file = file_path_1L_chr,
                       row.names = F)
  }
}
write_to_delete_dirs <- function(dir_paths_chr){
  dir_paths_chr <- dir_paths_chr[dir_paths_chr %>% purrr::map_lgl(~dir.exists(.x))]
  if(!identical(dir_paths_chr, character(0))){
    fls_to_be_purged_chr <- dir_paths_chr %>%
      purrr::map(~list.files(.x, full.names = TRUE)) %>%
      purrr::flatten_chr()
    message(paste0("Are you sure that you want to delete the following director",
                   ifelse(length(dir_paths_chr)>1,"ies","y"),
                   ":\n",
                   dir_paths_chr %>% paste0(collapse = "\n"),
                   ifelse(identical(fls_to_be_purged_chr, character(0)),
                          "",
                          paste0(" and the following file",
                                 ifelse(length(fls_to_be_purged_chr)>1,
                                        "s:\n",
                                        ":\n"),
                                 fls_to_be_purged_chr %>% paste0(collapse = "\n"))),
                   " from your machine: \n",
                   "?"))
    consent_1L_chr <- make_prompt(prompt_1L_chr=paste0("Do you confirm ('Y') that you want to delete ",
                                                       ifelse(length(dir_paths_chr) > 1,
                                                              "these directories",
                                                              "this directory"),
                                                       ifelse(length(fls_to_be_purged_chr) > 0,
                                                              ifelse(length(fls_to_be_purged_chr) > 0,
                                                                     " and files",
                                                                     " and file"),
                                                              ""),
                                                       ":"),
                                  options_chr = c("Y", "N"),
                                  force_from_opts_1L_chr = T)
    if(consent_1L_chr %in% c("Y")){
      dir_paths_chr %>%
        purrr::walk(~unlink(.x, recursive=TRUE))
    }else{
      message("Delete directory request cancelled - no directories deleted")
    }
  }
}
write_to_delete_fls <- function(file_paths_chr){
  file_paths_chr <- file_paths_chr[file_paths_chr %>% purrr::map_lgl(~file.exists(.x))]
  if(!identical(file_paths_chr, character(0))){
    message(paste0("Are you sure that you want to delete the following file",
                   ifelse(length(file_paths_chr)>1,"s",""),
                   " from your machine: \n",
                   file_paths_chr %>% paste0(collapse = "\n"),
                   "?"))
    consent_1L_chr <-  make_prompt(prompt_1L_chr=paste0("Do you confirm ('Y') that you want to delete ",
                                                        ifelse(length(file_paths_chr)>1,"these files:","this file:")),
                                   options_chr = c("Y", "N"),
                                   force_from_opts_1L_chr = T)
    if(consent_1L_chr %in% c("Y")){
      paths_ls <- do.call(file.remove, list(file_paths_chr))
    }else{
      message("Delete files request cancelled - no files deleted")
    }
  }
}
write_to_dv_from_tbl <- function (files_tb, data_dir_rt_1L_chr = ".", ds_url_1L_chr,
                             key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                             server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
                             consent_1L_chr = "")
{
  if(!consent_1L_chr %in% c("Y", "N")){
    paths_chr <- paste0(ifelse(identical(character(0),data_dir_rt_1L_chr),
                                 "",
                                 paste0(data_dir_rt_1L_chr, "/")),
                          files_tb[,1], "/", files_tb[,2], files_tb[,3])
    consent_1L_chr <- make_prompt(prompt_1L_chr=paste0("Do you confirm ('Y') that you want to write the file",
                                                       ifelse(length(paths_chr)>1,"s ",""),
                                                       paths_chr,
                                                       " ? "),
                                  options_chr = c("Y", "N"),
                                  force_from_opts_1L_chr = T)
  }
  if(consent_1L_chr %in% c("Y")){
    ds_ls <- dataverse::get_dataset(ds_url_1L_chr)
    is_draft_1L_lgl <- ds_ls$versionState == "DRAFT"
    nms_chr <- ds_ls$files$filename
    fl_ids_int <- purrr::pmap_int(files_tb, ~{
      path_1L_chr <- paste0(ifelse(identical(character(0),data_dir_rt_1L_chr),
                                   "",
                                   paste0(data_dir_rt_1L_chr, "/")),
                            ..1, "/", ..2, ..3)
      fl_nm_1L_chr <- paste0(..2, ..3)
      write_fls_to_dv(path_1L_chr,
                      descriptions_chr = ..4,
                      ds_url_1L_chr = ds_url_1L_chr,
                      ds_ls = ds_ls,
                      key_1L_chr = key_1L_chr,
                      server_1L_chr = server_1L_chr)
    })
  }else{
    fl_ids_int <- NULL
  }
  return(fl_ids_int)
}
write_to_dv_with_wait <- function(dss_tb, # RENAME & Convert to two steps: Make class, apply method.
                                  dv_nm_1L_chr,
                                  ds_url_1L_chr,
                                  wait_time_in_secs_int = 5L,
                                  make_local_copy_1L_lgl = F,
                                  parent_dv_dir_1L_chr,
                                  paths_to_dirs_chr,
                                  paths_are_rltv_1L_lgl = T,
                                  inc_fl_types_chr = NA_character_,
                                  key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                                  server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
                                  consent_1L_chr = ""){
  ds_chr <- dss_tb$ds_obj_nm_chr
  files_tb <- make_files_tb(paths_to_dirs_chr = paths_to_dirs_chr,
                            recode_ls = dss_tb$title_chr %>% as.list() %>% stats::setNames(ds_chr),
                            inc_fl_types_chr = inc_fl_types_chr)
  if(paths_are_rltv_1L_lgl){
    data_dir_rt_1L_chr <- "."
  }else{
    data_dir_rt_1L_chr <- character(0)
  }
  paths_chr <- paste0(ifelse(identical(character(0),data_dir_rt_1L_chr),
                             "",
                             paste0(data_dir_rt_1L_chr, "/")),
                      files_tb[,1], "/", files_tb[,2], files_tb[,3])
  if(!consent_1L_chr %in% c("Y", "N")){
    consent_1L_chr <- make_prompt(prompt_1L_chr=paste0("Do you confirm ('Y') that you want to write the file",
                                                       ifelse(length(paths_chr)>1,"s ",""),
                                                       paths_chr,
                                                       "to dataverse ",
                                                       ds_url_1L_chr,
                                                       " ? "),
                                  options_chr = c("Y", "N"),
                                  force_from_opts_1L_chr = T)
  }
  if(consent_1L_chr %in% c("Y")){
    fl_ids_int <- 1:nrow(files_tb) %>%
      purrr::map_int(~{
        Sys.sleep(wait_time_in_secs_int)
        write_to_dv_from_tbl(files_tb[.x,],
                             data_dir_rt_1L_chr = data_dir_rt_1L_chr,
                             ds_url_1L_chr = ds_url_1L_chr,
                             key_1L_chr = key_1L_chr,
                             server_1L_chr = server_1L_chr,
                             consent_1L_chr = consent_1L_chr)
      }
      )
    ds_ls <- dataverse::get_dataset(ds_url_1L_chr)
    if(make_local_copy_1L_lgl | ds_ls$versionState != "DRAFT"){
      ds_ls <- dataverse::get_dataset(ds_url_1L_chr)
      dv_dir_1L_chr <- paste0(parent_dv_dir_1L_chr,"/",dv_nm_1L_chr)
      if(!dir.exists(dv_dir_1L_chr)){
        dir.create(dv_dir_1L_chr)
      }
      local_dv_dir_1L_chr <- paste0(dv_dir_1L_chr,"/",
                                    ds_ls$metadataBlocks$citation$fields$value[[3]])
      if(!dir.exists(local_dv_dir_1L_chr)){
        dir.create(local_dv_dir_1L_chr)
      }
      write_fls_from_dv(files_tb,
                        fl_ids_int = fl_ids_int,
                        ds_url_1L_chr = ds_url_1L_chr,
                        local_dv_dir_1L_chr = local_dv_dir_1L_chr)
    }
  }else{
    ds_ls <- NULL
  }
  return(ds_ls)
}
write_to_publish_dv_ds <- function(dv_ds_1L_chr){
  consent_1L_chr <- make_prompt(prompt_1L_chr=paste0("Do you confirm ('Y') that you wish to publish the current draft of dataverse ",
                                                     dv_ds_1L_chr,
                                                     "?"),
                                options_chr = c("Y", "N"),
                                force_from_opts_1L_chr = T)
  if(consent_1L_chr == "Y"){
    dataverse::publish_dataset(dv_ds_1L_chr,
                               minor = F)
  }
}
write_words <- function(new_words_chr,
                        gh_repo_1L_chr = "ready4-dev/ready4",
                        gh_tag_1L_chr = "Documentation_0.0"){
  dmt_urls_chr <- piggyback::pb_download_url(repo = gh_repo_1L_chr,
                                             tag = gh_tag_1L_chr,
                                             .token = "")
  b <- readRDS(url(dmt_urls_chr[dmt_urls_chr %>% endsWith("treat_as_words_chr.RDS")]))
  b <- c(b,new_words_chr) %>% sort()
  write_env_objs_to_dv(env_objects_ls = list(treat_as_words_chr = b),
                       descriptions_chr = NULL,
                       ds_url_1L_chr = character(0),
                       piggyback_desc_1L_chr = "Supplementary Files",
                       piggyback_tag_1L_chr =  gh_tag_1L_chr,
                       piggyback_to_1L_chr = gh_repo_1L_chr,
                       prerelease_1L_lgl = T)
}
write_ws <- function(path_1L_chr){
  top_level_chr <- paste0(path_1L_chr,
                          "/ready4/",
                          c("Code", "Data","Documentation", "Insight"))
  code_top_lvl_chr <- c("Application","Authoring","Brochure",
                        "Description","Modelling","Prediction",
                        "Foundation") %>%
    purrr::map_chr(~paste0(top_level_chr[1],"/",.x))
  code_sub_dirs_chr <- c(paste0(code_top_lvl_chr[2],"/Workflows",c("","/R")),
                         paste0(code_top_lvl_chr[3],"/HTML"),
                         paste0(code_top_lvl_chr[4],"/Datatypes",c("","/R")),
                         paste0(code_top_lvl_chr[5],"/Templates",c("","/R")),
                         paste0(code_top_lvl_chr[6],"/Example",c("","/Toolkit_1","/Toolkit_1/R")),
                         paste0(code_top_lvl_chr[7],"/Representations",c("","/R")))
  data_top_lvl_chr <- c("Dataverse","Project","R_Format","Raw_Format") %>%
    purrr::map_chr(~paste0(top_level_chr[2],"/",.x))
  data_sub_dirs_chr <- c("Agents","Attributes","Geometries","Metadata") %>%
    purrr::map_chr(~paste0(data_top_lvl_chr[4],"/",.x))
  dcmntn_top_lvl_chr <- c("Code", "Data","Images") %>%
    purrr::map_chr(~paste0(top_level_chr[3],"/",.x))
  dcmntn_sub_dirs_chr <- c("Developer", "User") %>%
    purrr::map_chr(~paste0(dcmntn_top_lvl_chr[1],"/",.x))
  insight_top_lvl_chr <- c("Analysis","Science","Team") %>%
    purrr::map_chr(~paste0(top_level_chr[4],"/",.x))
  new_dirs_chr <- c(paste0(path_1L_chr,"/ready4"),
                    top_level_chr,
                    code_top_lvl_chr, code_sub_dirs_chr,
                    data_top_lvl_chr, data_sub_dirs_chr,
                    dcmntn_top_lvl_chr, dcmntn_sub_dirs_chr,
                    insight_top_lvl_chr)
  write_new_dirs(new_dirs_chr)
}
