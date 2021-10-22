#' Get dataverse files urls
#' @description get_dv_fls_urls() is a Get Data function that retrieves data from R objects loaded in memory. Specifically, this function implements an algorithm to get dataverse files urls. The function returns Urls (a character vector).
#' @param file_nms_chr File names (a character vector)
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one)
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: character(0)
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @return Urls (a character vector)
#' @rdname get_dv_fls_urls
#' @export
#' @importFrom lifecycle deprecate_soft
#' @importFrom dataverse dataset_files
#' @importFrom purrr map_chr
get_dv_fls_urls <- function (file_nms_chr, dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = character(0),
                             server_1L_chr = Sys.getenv("DATAVERSE_SERVER"), key_1L_chr = NULL)
{
  lifecycle::deprecate_soft("0.0.0.9446", "ready4fun::get_dv_fls_urls()",
                            "get_dv_fls_urls()")
  if (identical(dv_url_pfx_1L_chr, character(0)))
    dv_url_pfx_1L_chr <- paste0("https://", server_1L_chr,
                                "/api/access/datafile/")
  ds_ls <- dataverse::dataset_files(dv_ds_nm_1L_chr, server = server_1L_chr,
                                    key = key_1L_chr)
  all_items_chr <- purrr::map_chr(ds_ls, ~.x$label)
  urls_chr <- file_nms_chr %>% purrr::map_chr(~{
    idx_1L_int <- which(all_items_chr == .x)
    if (identical(idx_1L_int, integer(0))) {
      NA_character_
    }
    else {
      paste0(dv_url_pfx_1L_chr, ds_ls[[idx_1L_int]]$dataFile$id)
    }
  })
  return(urls_chr)
}
#' Get file identity from dataverse list
#' @description get_fl_id_from_dv_ls() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get file identity from dataverse list. Function argument ds_ls specifies the where to look for the required object. The function returns Identity (a character vector of length one).
#' @param ds_ls Dataset (a list)
#' @param fl_nm_1L_chr File name (a character vector of length one)
#' @param nms_chr Names (a character vector), Default: 'NA'
#' @return Identity (a character vector of length one)
#' @rdname get_fl_id_from_dv_ls
#' @export
#' @importFrom lifecycle deprecate_soft
#' @importFrom purrr map2_chr
#' @importFrom ready4 get_from_lup_obj
#' @importFrom tibble as_tibble
#' @keywords internal
get_fl_id_from_dv_ls <- function (ds_ls, fl_nm_1L_chr, nms_chr = NA_character_)
{
  lifecycle::deprecate_soft("0.0.0.9446", "ready4fun::get_fl_id_from_dv_ls()",
                            "get_fl_id_from_dv_ls()")
  if (is.na(nms_chr[1])) {
    nms_chr <- purrr::map2_chr(ds_ls$files$originalFileName,
                               ds_ls$files$filename, ~ifelse(is.na(.x), .y, .x))
  }
  if (fl_nm_1L_chr %in% nms_chr) {
    id_1L_chr <- get_from_lup_obj(ds_ls$files[, names(ds_ls$files) %>%
                                                        unique()] %>% tibble::as_tibble(), match_var_nm_1L_chr = ifelse(fl_nm_1L_chr %in%
                                                                                                                          ds_ls$files$originalFileName, "originalFileName",
                                                                                                                        "filename"), match_value_xx = fl_nm_1L_chr, target_var_nm_1L_chr = "id",
                                          evaluate_1L_lgl = F)
  }
  else {
    id_1L_chr <- NA_character_
  }
  return(id_1L_chr)
}
#' Get from lookup table object
#' @description get_from_lup_obj() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get from lookup table object. Function argument data_lookup_tb specifies the where to look for the required object. The function returns Return object (an output object of multiple potential types).
#' @param data_lookup_tb Data lookup (a tibble)
#' @param match_value_xx Match value (an output object of multiple potential types)
#' @param match_var_nm_1L_chr Match variable name (a character vector of length one)
#' @param target_var_nm_1L_chr Target variable name (a character vector of length one)
#' @param evaluate_1L_lgl Evaluate (a logical vector of length one), Default: TRUE
#' @return Return object (an output object of multiple potential types)
#' @rdname get_from_lup_obj
#' @export
#' @importFrom lifecycle deprecate_soft
#' @importFrom dplyr filter select pull
#' @importFrom rlang sym
#' @importFrom stringr str_detect str_locate str_sub
get_from_lup_obj <- function (data_lookup_tb, match_value_xx, match_var_nm_1L_chr,
                              target_var_nm_1L_chr, evaluate_1L_lgl = TRUE)
{
  lifecycle::deprecate_soft("0.0.0.9446", "ready4fun::get_from_lup_obj()",
                            "get_from_lup_obj()")
  return_object_ref <- data_lookup_tb %>% dplyr::filter(!!rlang::sym(match_var_nm_1L_chr) ==
                                                          match_value_xx) %>% dplyr::select(!!target_var_nm_1L_chr) %>%
    dplyr::pull()
  if (evaluate_1L_lgl) {
    if (stringr::str_detect(return_object_ref, "::")) {
      colon_positions <- stringr::str_locate(return_object_ref,
                                             "::")
      namespace_ref <- stringr::str_sub(return_object_ref,
                                        start = 1, end = colon_positions[1, "start"] -
                                          1)
      object_ref <- stringr::str_sub(return_object_ref,
                                     start = colon_positions[1, "end"] + 1)
      if (sum(stringr::str_detect(search(), paste0("package:",
                                                   namespace_ref))) == 0) {
        namespace_ref_sym <- rlang::sym(namespace_ref)
        attachNamespace(namespace_ref)
        return_object_xx <- get(x = object_ref, envir = as.environment(paste0("package:",
                                                                              namespace_ref)))
        detach(paste0("package:", namespace_ref), character.only = TRUE)
      }
      else {
        return_object_xx <- get(x = object_ref, envir = as.environment(paste0("package:",
                                                                              namespace_ref)))
      }
    }
    else {
      return_object_xx <- get(x = return_object_ref)
    }
  }
  else {
    return_object_xx <- return_object_ref
  }
  return(return_object_xx)
}
#' Get ready4 S4 object slots
#' @description get_r4_obj_slots() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get ready4 s4 object slots. Function argument fn_name_1L_chr specifies the where to look for the required object. The function returns Slots (a character vector).
#' @param fn_name_1L_chr Function name (a character vector of length one)
#' @param package_1L_chr Package (a character vector of length one), Default: ''
#' @return Slots (a character vector)
#' @rdname get_r4_obj_slots
#' @export
#' @importFrom lifecycle deprecate_soft
#' @importFrom methods getSlots
#' @importFrom purrr map_chr
#' @keywords internal
get_r4_obj_slots <- function (fn_name_1L_chr, package_1L_chr = "")
{
  lifecycle::deprecate_soft("0.0.0.9446", "ready4fun::get_r4_obj_slots()",
                            "get_r4_obj_slots()")
  slots_ls <- className(fn_name_1L_chr, update_ns(package_1L_chr)) %>%
    methods::getSlots()
  slots_chr <- purrr::map_chr(slots_ls, ~.x)
  return(slots_chr)
}
#' Get rds from dataverse
#' @description get_rds_from_dv() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get rds from dataverse. Function argument file_nm_1L_chr specifies the where to look for the required object. The function returns R object (an output object of multiple potential types).
#' @param file_nm_1L_chr File name (a character vector of length one)
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'https://doi.org/10.7910/DVN/2Y9VF9'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: character(0)
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return R object (an output object of multiple potential types)
#' @rdname get_rds_from_dv
#' @export
#' @importFrom lifecycle deprecate_soft
#' @importFrom dataverse dataset_files
#' @importFrom purrr map_chr
get_rds_from_dv <- function (file_nm_1L_chr, dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9",
                             dv_url_pfx_1L_chr = character(0), key_1L_chr = NULL, server_1L_chr = Sys.getenv("DATAVERSE_SERVER"))
{
  lifecycle::deprecate_soft("0.0.0.9446", "ready4fun::get_rds_from_dv()",
                            "get_rds_from_dv()")
  if (identical(dv_url_pfx_1L_chr, character(0)))
    dv_url_pfx_1L_chr <- paste0("https://", server_1L_chr,
                                "/api/access/datafile/")
  ds_ls <- dataverse::dataset_files(dv_ds_nm_1L_chr, server = server_1L_chr,
                                    key = key_1L_chr)
  all_items_chr <- purrr::map_chr(ds_ls, ~.x$label)
  idx_1L_int <- which(all_items_chr == paste0(file_nm_1L_chr,
                                              ".RDS"))
  if (identical(idx_1L_int, integer(0))) {
    r_object_xx <- NULL
  }
  else {
    r_object_xx <- readRDS(url(paste0(dv_url_pfx_1L_chr,
                                      ds_ls[[idx_1L_int]]$dataFile$id)))
  }
  return(r_object_xx)
}
