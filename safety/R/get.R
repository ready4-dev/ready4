get_dv_fls_urls <- function(file_nms_chr,
                            dv_ds_nm_1L_chr,
                            dv_url_pfx_1L_chr = character(0),
                            server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
                            key_1L_chr = NULL){
  if(identical(dv_url_pfx_1L_chr, character(0)))
    dv_url_pfx_1L_chr <- paste0("https://",
                                server_1L_chr,
                                "/api/access/datafile/")
  ds_ls <- dataverse::dataset_files(dv_ds_nm_1L_chr,
                                    server = server_1L_chr,
                                    key = key_1L_chr)
  all_items_chr <- purrr::map_chr(ds_ls,~.x$label)
  urls_chr <- file_nms_chr %>%
    purrr::map_chr(~{
      idx_1L_int <- which(all_items_chr == .x)
      if(identical(idx_1L_int, integer(0))){
        NA_character_
      }else{
        paste0(dv_url_pfx_1L_chr,ds_ls[[idx_1L_int]]$dataFile$id)
      }
    })
  return(urls_chr)
}
get_fl_id_from_dv_ls <-  function (ds_ls, fl_nm_1L_chr, nms_chr = NA_character_)
{
  if (is.na(nms_chr[1])) {
    nms_chr <- purrr::map2_chr(ds_ls$files$originalFileName,
                               ds_ls$files$filename, ~ifelse(is.na(.x), .y, .x))
  }
  if (fl_nm_1L_chr %in% nms_chr) {
    id_1L_chr <- get_from_lup_obj(ds_ls$files[, names(ds_ls$files) %>% unique()] %>%
                                            tibble::as_tibble(),
                                          match_var_nm_1L_chr = ifelse(fl_nm_1L_chr %in% ds_ls$files$originalFileName,
                                                                       "originalFileName",
                                                                       "filename"),
                                          match_value_xx = fl_nm_1L_chr,
                                          target_var_nm_1L_chr = "id",
                                          evaluate_1L_lgl = F)
  }
  else {
    id_1L_chr <- NA_character_
  }
  return(id_1L_chr)
}
get_from_lup_obj <- function(data_lookup_tb,
                             match_value_xx,
                             match_var_nm_1L_chr,
                             target_var_nm_1L_chr,
                             evaluate_1L_lgl = FALSE){
  return_object_ref <- data_lookup_tb %>%
    dplyr::filter(!!rlang::sym(match_var_nm_1L_chr)==match_value_xx) %>%
    dplyr::select(!!target_var_nm_1L_chr) %>%
    dplyr::pull()
  if(evaluate_1L_lgl){
    if(stringr::str_detect(return_object_ref,"::")){
      colon_positions <- stringr::str_locate(return_object_ref,
                                             "::")
      namespace_ref <- stringr::str_sub(return_object_ref,
                                        start=1,
                                        end=colon_positions[1,"start"]-1)
      object_ref <- stringr::str_sub(return_object_ref,
                                     start=colon_positions[1,"end"]+1)

      if(sum(stringr::str_detect(search(),paste0("package:",
                                                 namespace_ref))) == 0){
        namespace_ref_sym <- rlang::sym(namespace_ref)
        attachNamespace(namespace_ref)
        return_object_xx <- get(x = object_ref,
                                envir = as.environment(paste0("package:",
                                                              namespace_ref)))
        detach(paste0("package:",
                      namespace_ref),
               character.only = TRUE)
      }else{
        return_object_xx <- get(x = object_ref,
                                envir = as.environment(paste0("package:",
                                                              namespace_ref)))
      }
    }else{
      return_object_xx <- get(x = return_object_ref)
    }
  }else{
    return_object_xx <- return_object_ref
  }
  return(return_object_xx)
}
get_r4_obj_slots <- function(fn_name_1L_chr,
                             package_1L_chr = ""){
  slots_ls <- methods::className(fn_name_1L_chr,
                                 ifelse(package_1L_chr=="",".GlobalEnv",package_1L_chr)) %>%
    methods::getSlots()
  slots_chr <- purrr::map_chr(slots_ls, ~ .x)
  return(slots_chr)
}
get_rds_from_dv <- function(file_nm_1L_chr,
                            dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9",
                            dv_url_pfx_1L_chr = character(0),
                            key_1L_chr = NULL,
                            server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  if(identical(dv_url_pfx_1L_chr, character(0)))
    dv_url_pfx_1L_chr <- paste0("https://",
                                server_1L_chr,
                                "/api/access/datafile/")
  ds_ls <- dataverse::dataset_files(dv_ds_nm_1L_chr,
                                    server = server_1L_chr,
                                    key = key_1L_chr)
  all_items_chr <- purrr::map_chr(ds_ls,~.x$label)
  idx_1L_int <- which(all_items_chr == paste0(file_nm_1L_chr,".RDS"))
  if(identical(idx_1L_int, integer(0))){
    r_object_xx <- NULL
  }else{
    r_object_xx <- readRDS(url(paste0(dv_url_pfx_1L_chr,
                                      ds_ls[[idx_1L_int]]$dataFile$id)))
  }
  return(r_object_xx)
}
