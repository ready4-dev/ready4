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
write_env_objs_to_dv <- function(env_objects_ls,
                                 descriptions_chr,
                                 ds_url_1L_chr,
                                 key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                                 publish_dv_1L_lgl = F,
                                 server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  tmp_dir <- tempdir()
  paths_chr <- env_objects_ls %>%
    purrr::map2_chr(names(env_objects_ls),
                    ~{
                      path_1L_chr <- paste0(tmp_dir,"/",.y,".RDS")
                      saveRDS(object = .x,
                              file = path_1L_chr)
                      path_1L_chr
                    })
  file_ids_int <- write_fls_to_dv(paths_chr,
                                  descriptions_chr = descriptions_chr,
                                  ds_url_1L_chr = ds_url_1L_chr,
                                  ds_ls = dataverse::get_dataset(ds_url_1L_chr),
                                  key_1L_chr = key_1L_chr,
                                  server_1L_chr = server_1L_chr)
  do.call(file.remove, list(paths_chr))
  unlink(tmp_dir)
  if(publish_dv_1L_lgl){
    write_to_publish_dv_ds(dv_ds_1L_chr = ds_url_1L_chr)
  }
  return(file_ids_int)
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
                                list.files(.x)
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
    recursive_1L_lgl <- ifelse(paths_chr %>% purrr::map_lgl(~dir.exists(.x)) %>% any(),
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
                            pfx_1L_chr){
  methods::slot(tbs_r4,slot_nm_1L_chr) %>%
    dplyr::mutate_if(is.list,.funs = dplyr::funs(ifelse(stringr::str_c(.)=="NULL",NA_character_ , stringr::str_c (.)))) %>%
    utils::write.csv(file = paste0(lup_dir_1L_chr,"/",pfx_1L_chr,"_",slot_nm_1L_chr,".csv"),
                     row.names = F)
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
write_ws <- function(path_1L_chr){
  top_level_chr <- paste0(path_1L_chr,
                          "/ready4/",
                          c("Code", "Data","Documentation", "Insight"))
  code_top_lvl_chr <- c("Application","Authoring","Brochure","Description","Modelling","Prediction") %>%
    purrr::map_chr(~paste0(top_level_chr[1],"/",.x))
  code_sub_dirs_chr <- c(paste0(code_top_lvl_chr[2],"/Workflows",c("","/R")),
                         paste0(code_top_lvl_chr[3],"/HTML"),
                         paste0(code_top_lvl_chr[4],"/Datatypes",c("","/R")),
                         paste0(code_top_lvl_chr[5],"/Templates",c("","/R")),
                         paste0(code_top_lvl_chr[6],"/Example",c("","/Toolkit_1","/Toolkit_1/R")))
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
