#' Write all tibbles in tibbles ready4 module to comma separated variables files
#' @description write_all_tbs_in_tbs_r4_to_csvs() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write all tibbles in tibbles ready4 module to comma separated variables files. The function is called for its side effects and does not return a value.
#' @param tbs_r4 Tibbles (a ready4 module)
#' @param r4_name_1L_chr Ready4 module name (a character vector of length one)
#' @param lup_dir_1L_chr Lookup table directory (a character vector of length one)
#' @param pfx_1L_chr Prefix (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @return No return value, called for side effects.
#' @rdname write_all_tbs_in_tbs_r4_to_csvs
#' @export 
#' @importFrom purrr walk
#' @importFrom methods getSlots
write_all_tbs_in_tbs_r4_to_csvs <- function (tbs_r4, r4_name_1L_chr, lup_dir_1L_chr, pfx_1L_chr, 
    consent_1L_chr = "", consent_indcs_int = 1L, options_chr = c("Y", 
        "N")) 
{
    purrr::walk(methods::getSlots(r4_name_1L_chr) %>% names(), 
        ~write_tb_to_csv(tbs_r4 = tbs_r4, slot_nm_1L_chr = .x, 
            r4_name_1L_chr = r4_name_1L_chr, lup_dir_1L_chr = lup_dir_1L_chr, 
            pfx_1L_chr = pfx_1L_chr, consent_1L_chr = consent_1L_chr, 
            consent_indcs_int = consent_indcs_int, options_chr = options_chr))
}
#' Write blog entries
#' @description write_blog_entries() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write blog entries. The function is called for its side effects and does not return a value.
#' @param dir_path_1L_chr Directory path (a character vector of length one)
#' @param fl_nm_1L_chr File name (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @return No return value, called for side effects.
#' @rdname write_blog_entries
#' @export 
#' @importFrom rmarkdown render
write_blog_entries <- function (dir_path_1L_chr, fl_nm_1L_chr, consent_1L_chr = "", 
    consent_indcs_int = 1L, options_chr = c("Y", "N")) 
{
    if (!requireNamespace("rmarkdown", quietly = TRUE)) {
        stop("rmarkdown package is required - please install it and rerun the last command.")
    }
    rmarkdown::render(paste0(dir_path_1L_chr, "/", fl_nm_1L_chr, 
        "/index_Body.Rmd"), output_dir = paste0(dir_path_1L_chr, 
        "/", fl_nm_1L_chr))
    write_to_trim_html(paste0(dir_path_1L_chr, "/", fl_nm_1L_chr, 
        "/index_Body.html"), consent_1L_chr = consent_1L_chr)
    rmarkdown::render(paste0(dir_path_1L_chr, "/", fl_nm_1L_chr, 
        "/index.Rmd"), output_dir = paste0(dir_path_1L_chr, "/", 
        fl_nm_1L_chr))
    unlink(paste0(dir_path_1L_chr, "/", fl_nm_1L_chr, "/index_Body.html"))
    if (file.exists(paste0(dir_path_1L_chr, "/", fl_nm_1L_chr, 
        "/index.html"))) 
        unlink(paste0(dir_path_1L_chr, "/", fl_nm_1L_chr, "/index.html"))
    file_chr <- readLines(paste0(dir_path_1L_chr, "/", fl_nm_1L_chr, 
        "/index.md"))
    file_chr <- file_chr[file_chr != "<div class='highlight'>"]
    file_chr <- file_chr[c(1:(max(which(file_chr == "</div>")) - 
        1), (max(which(file_chr == "</div>")) + 1):length(file_chr))]
    write_with_consent(consented_fn = writeLines, prompt_1L_chr = paste0("Do you confirm that you want to write the file ", 
        paste0(fl_nm_1L_chr, "/index.md"), " to ", dir_path_1L_chr, 
        "?"), consent_1L_chr = consent_1L_chr, consent_indcs_int = consent_indcs_int, 
        consented_args_ls = list(text = file_chr, con = paste0(dir_path_1L_chr, 
            "/", fl_nm_1L_chr, "/index.md")), consented_msg_1L_chr = paste0("File ", 
            paste0(fl_nm_1L_chr, "/index.md"), " has been written to ", 
            dir_path_1L_chr, "."), declined_msg_1L_chr = "Write request cancelled - no new files have been written.", 
        options_chr = options_chr)
}
#' Write citation cff
#' @description write_citation_cff() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write citation cff. The function is called for its side effects and does not return a value.
#' @param pkg_desc_ls Package description (a list)
#' @param citation_chr Citation (a character vector)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param publisher_1L_chr Publisher (a character vector of length one), Default: 'Zenodo'
#' @return No return value, called for side effects.
#' @rdname write_citation_cff
#' @export 
#' @importFrom stringr str_sub str_replace_all str_remove str_replace
#' @importFrom purrr map_chr
write_citation_cff <- function (pkg_desc_ls, citation_chr, consent_1L_chr = "", consent_indcs_int = 1L, 
    options_chr = c("Y", "N"), publisher_1L_chr = "Zenodo") 
{
    meta <- pkg_desc_ls
    authors_ls <- parse(text = paste0("list(", citation_chr[startsWith(citation_chr, 
        "  author   = ")] %>% stringr::str_sub(start = 16, end = -3) %>% 
        stringr::str_replace_all("person", "c"), ")")) %>% eval()
    citation_cff_chr <- c("cff-version: 1.2.0", paste0("message: \"", 
        parse(text = citation_chr[which(citation_chr %>% startsWith("  textVersion = ")):(length(citation_chr) - 
            1)] %>% stringr::str_remove("  textVersion = ") %>% 
            paste0(collapse = "\n")) %>% eval() %>% stringr::str_replace(paste0(meta$Version, 
            "."), paste0(meta$Version, ". ", publisher_1L_chr, 
            ".")), "\""), "authors:", purrr::map_chr(authors_ls, 
        ~paste0("  - family-names: \"", .x[2], "\"\n", "    given-names: \"", 
            .x[1], "\"")), paste0("title: \"", paste0(meta$Package, 
        ": ", meta$Title), "\""), paste0("version: ", meta$Version), 
        paste0("doi: ", citation_chr[startsWith(citation_chr, 
            "  doi      = ")] %>% stringr::str_sub(start = 15, 
            end = -3)), paste0("date-released: ", Sys.Date()), 
        paste0("url: \"", citation_chr[startsWith(citation_chr, 
            "  url      = ")] %>% stringr::str_sub(start = 15, 
            end = -3), "\""))
    write_with_consent(consented_fn = write_new_files, prompt_1L_chr = paste0("Do you confirm that you want to write the file ", 
        "CITATION.cff", " to ", getwd(), "?"), consent_1L_chr = consent_1L_chr, 
        consent_indcs_int = consent_indcs_int, consented_args_ls = list(paths_chr = "CITATION.cff", 
            fl_nm_1L_chr = "CITATION", consent_1L_chr = consent_1L_chr, 
            text_ls = list(citation_cff_chr), consent_indcs_int = consent_indcs_int, 
            options_chr = options_chr, return_1L_lgl = FALSE), 
        consented_msg_1L_chr = paste0("File ", "CITATION.cff", 
            " has been written to ", getwd(), "."), declined_msg_1L_chr = "Write request cancelled - no new files have been written.", 
        options_chr = options_chr, return_1L_lgl = FALSE)
}
#' Write conditional tags
#' @description write_conditional_tags() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write conditional tags. The function is called for its side effects and does not return a value.
#' @param pkgs_chr Packages (a character vector)
#' @param path_to_pkg_root_1L_chr Path to package root (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param where_1L_chr Where (a character vector of length one), Default: character(0)
#' @return No return value, called for side effects.
#' @rdname write_conditional_tags
#' @export 
#' @importFrom purrr walk map_lgl reduce map_chr discard
#' @importFrom stringr str_remove_all str_sub
#' @importFrom usethis use_package
write_conditional_tags <- function (pkgs_chr, path_to_pkg_root_1L_chr, consent_1L_chr = "", 
    consent_indcs_int = 1L, options_chr = c("Y", "N"), where_1L_chr = character(0)) 
{
    if (identical(where_1L_chr, character(0))) {
        purrr::walk(c("R", "DESCRIPTION"), ~write_conditional_tags(pkgs_chr, 
            path_to_pkg_root_1L_chr = path_to_pkg_root_1L_chr, 
            consent_1L_chr = consent_1L_chr, consent_indcs_int = consent_indcs_int, 
            options_chr = options_chr, where_1L_chr = .x))
    }
    else {
        consented_fn <- function(path_to_pkg_root_1L_chr, pkgs_chr, 
            where_1L_chr) {
            if (where_1L_chr == "R") {
                path_to_R_dir_1L_chr <- paste0(path_to_pkg_root_1L_chr, 
                  "/R")
                paths_chr <- list.files(path_to_R_dir_1L_chr, 
                  full.names = TRUE)
                paths_chr[paths_chr %>% purrr::map_lgl(~startsWith(.x, 
                  paste0(path_to_R_dir_1L_chr, "/", "fn_")) | 
                  startsWith(.x, paste0(path_to_R_dir_1L_chr, 
                    "/", "mthd_")))] %>% purrr::walk(~{
                  file_chr <- readLines(.x)
                  file_chr <- purrr::reduce(pkgs_chr, .init = file_chr, 
                    ~{
                      text_chr <- .x
                      old_chr <- text_chr[text_chr %>% startsWith(paste0("#' @importFrom ", 
                        .y))]
                      pkg_1L_chr <- .y
                      new_chr <- old_chr %>% stringr::str_remove_all(paste0("#' @importFrom ", 
                        .y)) %>% trimws() %>% strsplit(split = " ") %>% 
                        purrr::map_chr(~paste0("#' @seealso ", 
                          paste0("[", pkg_1L_chr, "::", .x, "()]") %>% 
                            make_list_phrase()))
                      text_chr[text_chr %in% old_chr] <- new_chr[match(text_chr, 
                        old_chr, nomatch = 0)]
                      text_chr
                    })
                  file_chr %>% writeLines(con = .x)
                })
            }
            else {
                file_chr <- readLines(paste0(path_to_pkg_root_1L_chr, 
                  "/DESCRIPTION"))
                file_chr <- purrr::reduce(pkgs_chr, .init = file_chr, 
                  ~{
                    text_chr <- .x
                    text_chr[text_chr %>% purrr::map_chr(~trimws(.x)) %>% 
                      startsWith(.y)] <- NA_character_
                    text_chr
                  }) %>% purrr::discard(is.na)
                if (endsWith(file_chr[length(file_chr)], ",")) {
                  file_chr[length(file_chr)] <- stringr::str_sub(file_chr[length(file_chr)], 
                    end = -2)
                }
                file_chr %>% writeLines(con = paste0(path_to_pkg_root_1L_chr, 
                  "/DESCRIPTION"))
                if (requireNamespace("usethis", quietly = TRUE)) {
                  pkgs_chr %>% purrr::walk(~usethis::use_package(.x, 
                    type = "Suggests"))
                }
            }
        }
        if (where_1L_chr == "R") {
            path_to_R_dir_1L_chr <- paste0(path_to_pkg_root_1L_chr, 
                "/R")
            prompt_1L_chr <- paste0("Do you confirm that you want to overwrite files in directory '", 
                path_to_R_dir_1L_chr, "' to remove all '@importFrom' tags that reference packages ", 
                make_list_phrase(pkgs_chr), " and to instead use corresponding `@seealso` tags", 
                " ?")
            consented_msg_1L_chr <- paste0("Any files in ", path_to_R_dir_1L_chr, 
                " with a matching pattern ", make_list_phrase(paste0("#' @importFrom ", 
                  pkgs_chr)), " have been edited.")
        }
        if (where_1L_chr == "DESCRIPTION") {
            prompt_1L_chr <- paste0("Do you confirm that you want to edit the DESCRIPTION file in '", 
                path_to_pkg_root_1L_chr, "' to remove the packages ", 
                make_list_phrase(pkgs_chr), " from the 'Imports' list and add them to the 'Suggests' list", 
                " ?")
            consented_msg_1L_chr <- paste0("The DESCRIPTION file in ", 
                path_to_pkg_root_1L_chr, " has been updated to list the ", 
                make_list_phrase(pkgs_chr), " packages as suggested rather than imported.")
        }
        if (where_1L_chr %in% c("R", "DESCRIPTION")) {
            write_with_consent(consented_fn = consented_fn, prompt_1L_chr = prompt_1L_chr, 
                consent_1L_chr = consent_1L_chr, consent_indcs_int = consent_indcs_int, 
                consented_args_ls = list(path_to_pkg_root_1L_chr = path_to_pkg_root_1L_chr, 
                  pkgs_chr = pkgs_chr, where_1L_chr = where_1L_chr), 
                consented_msg_1L_chr = consented_msg_1L_chr, 
                declined_msg_1L_chr = "Write request cancelled - no files have been edited.", 
                options_chr = options_chr, return_1L_lgl = FALSE)
        }
    }
}
#' Write dataverse file to local
#' @description write_dv_fl_to_loc() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write dataverse file to local. The function is called for its side effects and does not return a value.
#' @param ds_url_1L_chr Dataset url (a character vector of length one), Default: character(0)
#' @param dest_path_1L_chr Destination path (a character vector of length one)
#' @param repo_fl_fmt_1L_chr Repository file format (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param fl_nm_1L_chr File name (a character vector of length one), Default: 'NA'
#' @param fl_id_1L_int File identity (an integer vector of length one), Default: NA
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param save_type_1L_chr Save type (a character vector of length one), Default: 'original'
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @param ds_ui_1L_chr Dataset user interface (a character vector of length one), Default: deprecated()
#' @return No return value, called for side effects.
#' @rdname write_dv_fl_to_loc
#' @export 
#' @importFrom lifecycle is_present deprecate_warn
#' @importFrom dataverse get_dataset
write_dv_fl_to_loc <- function (ds_url_1L_chr = character(0), dest_path_1L_chr, repo_fl_fmt_1L_chr, 
    consent_1L_chr = "", consent_indcs_int = 1L, fl_nm_1L_chr = NA_character_, 
    fl_id_1L_int = NA_integer_, key_1L_chr = Sys.getenv("DATAVERSE_KEY"), 
    options_chr = c("Y", "N"), save_type_1L_chr = "original", 
    server_1L_chr = Sys.getenv("DATAVERSE_SERVER"), ds_ui_1L_chr = deprecated()) 
{
    if (lifecycle::is_present(ds_ui_1L_chr)) {
        lifecycle::deprecate_warn("0.1.10.9005", "ready4::write_dv_fl_to_loc(ds_ui_1L_chr)", 
            details = "Please use `ready4::write_dv_fl_to_loc(ds_url_1L_chr)`.")
        if (identical(ds_url_1L_chr, character(0))) {
            ds_url_1L_chr <- ds_ui_1L_chr
        }
    }
    ds_ls <- get_gracefully(ds_url_1L_chr, fn = dataverse::get_dataset)
    if (!is.null(ds_ls)) {
        if (ds_ls$versionState != "DRAFT") {
            if (!is.na(fl_id_1L_int)) {
                ds_url_1L_chr <- NULL
            }
            write_with_consent(consented_fn = write_ingested_dv_fl, 
                prompt_1L_chr = paste0("Do you confirm that you want to write the file ", 
                  paste0(fl_nm_1L_chr, repo_fl_fmt_1L_chr), " to ", 
                  dest_path_1L_chr, " ?"), consent_1L_chr = consent_1L_chr, 
                consent_indcs_int = consent_indcs_int, consented_args_ls = list(ds_url_1L_chr = ds_url_1L_chr, 
                  fl_nm_1L_chr = fl_nm_1L_chr, fl_id_1L_int = fl_id_1L_int, 
                  repo_fl_fmt_1L_chr = repo_fl_fmt_1L_chr, key_1L_chr = key_1L_chr, 
                  server_1L_chr = server_1L_chr, save_type_1L_chr = save_type_1L_chr, 
                  dest_path_1L_chr = dest_path_1L_chr, consent_1L_chr = options_chr[consent_indcs_int[1]]), 
                consented_msg_1L_chr = paste0("New file created in ", 
                  dest_path_1L_chr, " :\n", paste0(fl_nm_1L_chr, 
                    repo_fl_fmt_1L_chr)), declined_msg_1L_chr = "Write request cancelled - no new files have been written.", 
                options_chr = options_chr, return_1L_lgl = FALSE)
        }
        else {
            warning("Cannot write local copy of files from private Dataverse repo.")
        }
    }
    else {
        message("Write action cancelled - online resources could not be found.")
    }
}
#' Write environment objects to dataverse
#' @description write_env_objs_to_dv() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write environment objects to dataverse. The function returns File identities (an integer vector).
#' @param env_objects_ls Environment objects (a list)
#' @param descriptions_chr Descriptions (a character vector)
#' @param ds_url_1L_chr Dataset url (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param publish_dv_1L_lgl Publish dataverse (a logical vector of length one), Default: FALSE
#' @param piggyback_desc_1L_chr Piggyback description (a character vector of length one), Default: 'Documentation'
#' @param piggyback_tag_1L_chr Piggyback tag (a character vector of length one), Default: 'Documentation_0.0'
#' @param piggyback_to_1L_chr Piggyback to (a character vector of length one), Default: character(0)
#' @param prerelease_1L_lgl Prerelease (a logical vector of length one), Default: TRUE
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return File identities (an integer vector)
#' @rdname write_env_objs_to_dv
#' @export 
#' @importFrom purrr map2_chr
#' @importFrom dataverse get_dataset
write_env_objs_to_dv <- function (env_objects_ls, descriptions_chr, ds_url_1L_chr, consent_1L_chr = "", 
    consent_indcs_int = 1L, key_1L_chr = Sys.getenv("DATAVERSE_KEY"), 
    options_chr = c("Y", "N"), publish_dv_1L_lgl = FALSE, piggyback_desc_1L_chr = "Documentation", 
    piggyback_tag_1L_chr = "Documentation_0.0", piggyback_to_1L_chr = character(0), 
    prerelease_1L_lgl = TRUE, server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    tmp_dir <- tempdir()
    paths_chr <- env_objects_ls %>% purrr::map2_chr(names(env_objects_ls), 
        ~{
            path_1L_chr <- ifelse(is.null(.x), NA_character_, 
                paste0(tmp_dir, "/", .y, ".RDS"))
            if (!is.na(path_1L_chr)) {
                saveRDS(object = .x, file = path_1L_chr)
            }
            path_1L_chr
        })
    descriptions_chr <- descriptions_chr[!is.na(paths_chr)]
    paths_chr <- paths_chr[!is.na(paths_chr)]
    if (identical(piggyback_to_1L_chr, character(0))) {
        ds_ls <- tryCatch(dataverse::get_dataset(ds_url_1L_chr, 
            key = key_1L_chr), error = function(cond) {
            NULL
        })
        if (is.null(ds_ls)) {
            ds_ls <- tryCatch(dataverse::get_dataset(ds_url_1L_chr, 
                key = NULL), error = function(cond) {
                NULL
            })
            if (is.null(ds_ls)) {
                ds_ls <- get_gracefully(ds_url_1L_chr, fn = dataverse::get_dataset)
            }
        }
    }
    else {
        ds_ls <- NULL
    }
    file_ids_int <- write_fls_to_repo(paths_chr, consent_1L_chr = consent_1L_chr, 
        consent_indcs_int = consent_indcs_int, descriptions_chr = descriptions_chr, 
        ds_url_1L_chr = ds_url_1L_chr, ds_ls = ds_ls, key_1L_chr = key_1L_chr, 
        options_chr = options_chr, piggyback_desc_1L_chr = piggyback_desc_1L_chr, 
        piggyback_tag_1L_chr = piggyback_tag_1L_chr, piggyback_to_1L_chr = piggyback_to_1L_chr, 
        prerelease_1L_lgl = prerelease_1L_lgl, server_1L_chr = server_1L_chr)
    do.call(file.remove, list(paths_chr))
    unlink(tmp_dir)
    if (publish_dv_1L_lgl & identical(piggyback_to_1L_chr, character(0))) {
        write_to_publish_dv_ds(consent_1L_chr = consent_1L_chr, 
            dv_ds_1L_chr = ds_url_1L_chr)
    }
    return(file_ids_int)
}
#' Write examples
#' @description write_examples() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write examples. The function is called for its side effects and does not return a value.
#' @param path_1L_chr Path (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param type_1L_chr Type (a character vector of length one), Default: 'fn'
#' @return No return value, called for side effects.
#' @rdname write_examples
#' @export 
#' @importFrom purrr map_chr pwalk map_lgl
#' @importFrom stringr str_sub str_locate str_remove str_replace
#' @importFrom devtools document
write_examples <- function (path_1L_chr, consent_1L_chr = "", consent_indcs_int = 1L, 
    options_chr = c("Y", "N"), type_1L_chr = "fn") 
{
    if (dir.exists(paste0(path_1L_chr, "/data-raw/examples"))) {
        methods_xx <- get_methods_tb()
        if (!is.null(methods_xx)) {
            methods_chr <- methods_xx$Method
            examples_chr <- list.files(paste0(path_1L_chr, "/data-raw/examples"))
            if (length(examples_chr) > 0) {
                file.copy(paste0(path_1L_chr, "/data-raw/examples"), 
                  "man", recursive = TRUE)
                fn_nms_chr <- examples_chr %>% purrr::map_chr(~stringr::str_sub(.x, 
                  end = -3))
                fn_fls_chr <- fn_nms_chr %>% purrr::map_chr(~stringr::str_sub(.x, 
                  end = stringr::str_locate(.x, "_")[1] - 1)) %>% 
                  purrr::map_chr(~paste0(path_1L_chr, "/R/", 
                    ifelse(.x %in% methods_chr, "mthd", "fn"), 
                    "_", .x, ".R"))
                consented_fn <- function(examples_chr, fn_fls_chr, 
                  fn_nms_chr) {
                  purrr::pwalk(list(examples_chr, fn_nms_chr, 
                    fn_fls_chr), ~{
                    fn_fl_1L_chr <- ..3
                    if (startsWith(stringr::str_remove(fn_fl_1L_chr, 
                      paste0(path_1L_chr, "/R/")), "fn_")) {
                      match_chr <- "fn"
                    }
                    else {
                      match_chr <- c("r3", "r4")
                    }
                    if (type_1L_chr %in% match_chr) {
                      if (type_1L_chr == "fn") 
                        starts_with_1L_chr <- paste0(..2, " <- function")
                      if (type_1L_chr == "r3") 
                        starts_with_1L_chr <- paste0(stringr::str_replace(..2, 
                          "_", "."), " <- function")
                      if (type_1L_chr == "r4") {
                        divider_1L_int <- stringr::str_locate(..2, 
                          "_")[1]
                        starts_with_1L_chr <- paste0("methods::setMethod(\"", 
                          stringr::str_sub(..2, end = divider_1L_int - 
                            1), "\", \"", stringr::str_sub(..2, 
                            start = divider_1L_int + 1), "\", function")
                      }
                      text_chr <- readLines(fn_fl_1L_chr)
                      addition_chr <- readLines(..1)
                      if (startsWith(addition_chr[1], "if (interactive())") | 
                        startsWith(addition_chr[1], "if(interactive())")) {
                        addition_chr <- c("#' @examplesIf interactive()", 
                          paste0("#' ", addition_chr[2:(length(addition_chr) - 
                            1)]))
                        if (startsWith(addition_chr[1], "  ")) {
                          addition_chr <- purrr::map_chr(addition_chr, 
                            ~stringr::str_replace(.x, "  ", ""))
                        }
                      }
                      else {
                        addition_chr <- paste0("#' @example man/examples/", 
                          ..2, ".R")
                      }
                      index_1L_int <- which(text_chr %>% purrr::map_lgl(~startsWith(.x, 
                        starts_with_1L_chr)))
                      c(text_chr[1:(index_1L_int - 1)], addition_chr, 
                        text_chr[index_1L_int:length(text_chr)]) %>% 
                        writeLines(fn_fl_1L_chr)
                    }
                  })
                }
                write_with_consent(consented_fn = consented_fn, 
                  prompt_1L_chr = paste0("Do you confirm that you wish to write examples to ", 
                    make_list_phrase(unique(fn_fls_chr)), "?"), 
                  consent_1L_chr = consent_1L_chr, consent_indcs_int = consent_indcs_int, 
                  consented_args_ls = list(examples_chr = paste0(path_1L_chr, 
                    "/data-raw/examples/", examples_chr), fn_fls_chr = fn_fls_chr, 
                    fn_nms_chr = fn_nms_chr), consented_msg_1L_chr = paste0("Examples have been written in ", 
                    make_list_phrase(unique(fn_fls_chr)), "."), 
                  declined_msg_1L_chr = "Write request cancelled - no examples have been written.", 
                  options_chr = options_chr, return_1L_lgl = FALSE)
            }
        }
        else {
            message("Write action cancelled - online resources could not be found.")
        }
    }
    if (requireNamespace("devtools", quietly = TRUE)) 
        devtools::document()
}
#' Write extra packages to actions
#' @description write_extra_pkgs_to_actions() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write extra packages to actions. The function is called for its side effects and does not return a value.
#' @param path_to_dir_1L_chr Path to directory (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @return No return value, called for side effects.
#' @rdname write_extra_pkgs_to_actions
#' @export 
#' @importFrom purrr walk
#' @importFrom stringr str_replace_all
write_extra_pkgs_to_actions <- function (path_to_dir_1L_chr, consent_1L_chr = "", consent_indcs_int = 1L, 
    options_chr = c("Y", "N")) 
{
    consented_fn <- function(path_to_dir_1L_chr) {
        list.files(path_to_dir_1L_chr, full.names = TRUE) %>% 
            purrr::walk(~{
                readLines(.x) %>% stringr::str_replace_all("extra-packages: ", 
                  "extra-packages: any::XML, ") %>% stringr::str_replace_all("any::XML, any::XML, ", 
                  "any::XML, ") %>% writeLines(con = .x)
            })
    }
    write_with_consent(consented_fn = consented_fn, prompt_1L_chr = paste0("Do you confirm that you want to modify all references to extra packages and XML in all files in  ", 
        path_to_dir_1L_chr, " ?"), consent_1L_chr = consent_1L_chr, 
        consent_indcs_int = consent_indcs_int, consented_args_ls = list(path_to_dir_1L_chr = path_to_dir_1L_chr), 
        consented_msg_1L_chr = paste0("Any files in ", path_to_dir_1L_chr, 
            " with a matching pattern ('extra-packages:' / 'any::XML, any::XML') have been edited."), 
        declined_msg_1L_chr = "Write request cancelled - no files have been edited.", 
        options_chr = options_chr, return_1L_lgl = FALSE)
}
#' Write files from dataverse
#' @description write_fls_from_dv() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write files from dataverse. The function is called for its side effects and does not return a value.
#' @param files_tb Files (a tibble)
#' @param fl_ids_int File identities (an integer vector)
#' @param ds_url_1L_chr Dataset url (a character vector of length one)
#' @param local_dv_dir_1L_chr Local dataverse directory (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return No return value, called for side effects.
#' @rdname write_fls_from_dv
#' @export 
#' @importFrom purrr walk
write_fls_from_dv <- function (files_tb, fl_ids_int, ds_url_1L_chr, local_dv_dir_1L_chr, 
    consent_1L_chr = "", consent_indcs_int = 1L, key_1L_chr = Sys.getenv("DATAVERSE_KEY"), 
    options_chr = c("Y", "N"), server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    consented_fn <- function(files_tb, fl_ids_int, ds_url_1L_chr, 
        local_dv_dir_1L_chr, key_1L_chr, server_1L_chr, consent_1L_chr) {
        purrr::walk(1:length(fl_ids_int), ~{
            if (!(files_tb$file_type_chr[.x] == ".zip")) {
                write_dv_fl_to_loc(ds_url_1L_chr = ds_url_1L_chr, 
                  fl_nm_1L_chr = files_tb$file_chr[.x], fl_id_1L_int = fl_ids_int[.x], 
                  repo_fl_fmt_1L_chr = files_tb$ds_file_ext_chr[.x], 
                  key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr, 
                  save_type_1L_chr = "original", dest_path_1L_chr = make_local_path_to_dv_data(save_dir_path_1L_chr = local_dv_dir_1L_chr, 
                    fl_nm_1L_chr = files_tb$file_chr[.x], save_fmt_1L_chr = files_tb$file_type_chr[.x]), 
                  consent_1L_chr = consent_1L_chr)
            }
        })
    }
    write_with_consent(consented_fn = consented_fn, prompt_1L_chr = paste0("Do you confirm that you want to write the file", 
        ifelse(length(files_tb$file_chr[fl_ids_int]) > 1, "s", 
            ""), make_list_phrase(files_tb$file_chr[fl_ids_int]), 
        " to ", local_dv_dir_1L_chr, "?"), consent_1L_chr = consent_1L_chr, 
        consent_indcs_int = consent_indcs_int, consented_args_ls = list(files_tb = files_tb, 
            fl_ids_int = fl_ids_int, ds_url_1L_chr = ds_url_1L_chr, 
            local_dv_dir_1L_chr = local_dv_dir_1L_chr, key_1L_chr = key_1L_chr, 
            server_1L_chr = server_1L_chr, consent_1L_chr = options_chr[consent_indcs_int[1]]), 
        consented_msg_1L_chr = paste0("File", ifelse(length(files_tb$file_chr[fl_ids_int]) > 
            1, "s", ""), make_list_phrase(files_tb$file_chr[fl_ids_int]), 
            " written to ", local_dv_dir_1L_chr, "."), declined_msg_1L_chr = "Write request cancelled - no files have been written.", 
        options_chr = options_chr, return_1L_lgl = FALSE)
}
#' Write files to dataverse
#' @description write_fls_to_dv() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write files to dataverse. The function returns Identities (an integer vector).
#' @param file_paths_chr File paths (a character vector)
#' @param ds_url_1L_chr Dataset url (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param descriptions_chr Descriptions (a character vector), Default: NULL
#' @param ds_ls Dataset (a list), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return Identities (an integer vector)
#' @rdname write_fls_to_dv
#' @export 
#' @importFrom dataverse get_dataset delete_file add_dataset_file update_dataset_file
#' @importFrom purrr map map2_int map_chr
write_fls_to_dv <- function (file_paths_chr, ds_url_1L_chr, consent_1L_chr = "", 
    consent_indcs_int = 1L, descriptions_chr = NULL, ds_ls = NULL, 
    key_1L_chr = Sys.getenv("DATAVERSE_KEY"), options_chr = c("Y", 
        "N"), server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    if (!identical(file_paths_chr, character(0))) {
        consented_fn <- function(file_paths_chr, ds_url_1L_chr, 
            descriptions_chr, ds_ls, key_1L_chr, server_1L_chr) {
            ids_int <- NULL
            if (is.null(ds_ls)) 
                ds_ls <- get_gracefully(ds_url_1L_chr, fn = dataverse::get_dataset)
            if (!is.null(ds_ls)) {
                is_draft_1L_lgl <- ds_ls$versionState == "DRAFT"
                nms_chr <- ds_ls$files$filename
                if (is.null(descriptions_chr)) 
                  descriptions_chr <- purrr::map(file_paths_chr, 
                    ~NULL)
                ids_int <- file_paths_chr %>% purrr::map2_int(descriptions_chr, 
                  ~{
                    fl_nm_1L_chr <- get_fl_nm_from_path(.x)
                    if (fl_nm_1L_chr %in% nms_chr) {
                      id_1L_int <- get_fl_id_from_dv_ls(ds_ls, 
                        fl_nm_1L_chr = fl_nm_1L_chr, nms_chr = nms_chr)
                      if (is_draft_1L_lgl) {
                        id_1L_int %>% dataverse::delete_file()
                        id_1L_int <- dataverse::add_dataset_file(file = .x, 
                          dataset = ds_url_1L_chr, description = .y, 
                          key = key_1L_chr, server = server_1L_chr)
                      }
                      else {
                        dataverse::update_dataset_file(file = .x, 
                          dataset = ds_url_1L_chr, id = id_1L_int, 
                          force = TRUE, description = .y, key = key_1L_chr, 
                          server = server_1L_chr)
                      }
                    }
                    else {
                      id_1L_int <- dataverse::add_dataset_file(file = .x, 
                        dataset = ds_url_1L_chr, description = .y, 
                        key = key_1L_chr, server = server_1L_chr)
                    }
                    id_1L_int
                  })
            }
            else {
                message("Write action cancelled - online resources could not be found.")
            }
            return(ids_int)
        }
        ids_int <- write_with_consent(consented_fn = consented_fn, 
            prompt_1L_chr = paste0("Are you sure that you want to upload the following file", 
                ifelse(length(file_paths_chr) > 1, "s", ""), 
                " to dataverse ", ds_url_1L_chr, ": \n", file_paths_chr %>% 
                  purrr::map_chr(~get_fl_nm_from_path(.x)) %>% 
                  paste0(collapse = "\n"), "?"), consent_1L_chr = consent_1L_chr, 
            consent_indcs_int = consent_indcs_int, consented_args_ls = list(file_paths_chr = file_paths_chr, 
                ds_url_1L_chr = ds_url_1L_chr, descriptions_chr = descriptions_chr, 
                ds_ls = ds_ls, key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr), 
            declined_msg_1L_chr = "Write request cancelled - no files have been written.", 
            options_chr = options_chr, return_1L_lgl = TRUE)
    }
    else {
        ids_int <- NULL
    }
    return(ids_int)
}
#' Write files to repository
#' @description write_fls_to_repo() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write files to repository. The function returns Identities (an integer vector).
#' @param paths_chr Paths (a character vector)
#' @param descriptions_chr Descriptions (a character vector)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param ds_url_1L_chr Dataset url (a character vector of length one), Default: character(0)
#' @param ds_ls Dataset (a list), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param piggyback_desc_1L_chr Piggyback description (a character vector of length one), Default: 'Documentation'
#' @param piggyback_tag_1L_chr Piggyback tag (a character vector of length one), Default: 'Documentation_0.0'
#' @param piggyback_to_1L_chr Piggyback to (a character vector of length one), Default: character(0)
#' @param prerelease_1L_lgl Prerelease (a logical vector of length one), Default: TRUE
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return Identities (an integer vector)
#' @rdname write_fls_to_repo
#' @export 
#' @importFrom piggyback pb_list pb_new_release pb_upload
#' @importFrom purrr walk
write_fls_to_repo <- function (paths_chr, descriptions_chr, consent_1L_chr = "", consent_indcs_int = 1L, 
    ds_url_1L_chr = character(0), ds_ls = NULL, key_1L_chr = Sys.getenv("DATAVERSE_KEY"), 
    options_chr = c("Y", "N"), piggyback_desc_1L_chr = "Documentation", 
    piggyback_tag_1L_chr = "Documentation_0.0", piggyback_to_1L_chr = character(0), 
    prerelease_1L_lgl = TRUE, server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    ids_int <- NULL
    if (!identical(piggyback_to_1L_chr, character(0))) {
        consented_fn <- function(paths_chr, piggyback_desc_1L_chr, 
            piggyback_to_1L_chr, piggyback_tag_1L_chr, prerelease_1L_lgl) {
            ids_int <- NULL
            releases_xx <- get_gracefully(piggyback_to_1L_chr, 
                fn = piggyback::pb_list, args_ls = list(tag = piggyback_tag_1L_chr, 
                  .token = ""))
            if (!is.null(releases_xx)) {
                releases_df <- releases_xx
                if (!piggyback_tag_1L_chr %in% releases_df$tag) 
                  piggyback::pb_new_release(piggyback_to_1L_chr, 
                    tag = piggyback_tag_1L_chr, body = piggyback_desc_1L_chr, 
                    prerelease = prerelease_1L_lgl)
                purrr::walk(paths_chr, ~{
                  if (file.exists(.x)) {
                    piggyback::pb_upload(.x, repo = piggyback_to_1L_chr, 
                      tag = piggyback_tag_1L_chr)
                  }
                })
            }
            else {
                message("Write action cancelled - repository could not be found.")
            }
            return(ids_int)
        }
        ids_int <- write_with_consent(consented_fn = consented_fn, 
            prompt_1L_chr = paste0("Do you confirm that you want to write the file", 
                ifelse(length(paths_chr) > 1, "s ", " "), make_list_phrase(basename(paths_chr)), 
                " to release ", piggyback_tag_1L_chr, " in ", 
                piggyback_to_1L_chr), consent_1L_chr = consent_1L_chr, 
            consent_indcs_int = consent_indcs_int, consented_args_ls = list(paths_chr = paths_chr, 
                piggyback_desc_1L_chr = piggyback_desc_1L_chr, 
                piggyback_to_1L_chr = piggyback_to_1L_chr, piggyback_tag_1L_chr = piggyback_tag_1L_chr, 
                prerelease_1L_lgl = prerelease_1L_lgl), declined_msg_1L_chr = "Write request cancelled - no files have been written.", 
            options_chr = options_chr, return_1L_lgl = TRUE)
    }
    else {
        if (!identical(character(0), ds_url_1L_chr)) 
            ids_int <- write_fls_to_dv(paths_chr, consent_1L_chr = consent_1L_chr, 
                consent_indcs_int = consent_indcs_int, descriptions_chr = descriptions_chr, 
                ds_url_1L_chr = ds_url_1L_chr, ds_ls = ds_ls, 
                key_1L_chr = key_1L_chr, options_chr = options_chr, 
                server_1L_chr = server_1L_chr)
    }
    return(ids_int)
}
#' Write from temporary
#' @description write_from_tmp() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write from temporary. The function is called for its side effects and does not return a value.
#' @param tmp_paths_chr Temporary paths (a character vector)
#' @param dest_paths_chr Destination paths (a character vector)
#' @param args_ls_ls Arguments (a list of lists), Default: list(NULL)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param edit_fn_ls Edit (a list of functions), Default: list(NULL)
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @return No return value, called for side effects.
#' @rdname write_from_tmp
#' @export 
#' @importFrom purrr pmap
#' @importFrom rlang exec
write_from_tmp <- function (tmp_paths_chr, dest_paths_chr, args_ls_ls = list(NULL), 
    consent_1L_chr = "", consent_indcs_int = 1L, edit_fn_ls = list(NULL), 
    options_chr = c("Y", "N")) 
{
    text_ls <- purrr::pmap(list(tmp_paths_chr, edit_fn_ls, args_ls_ls), 
        ~{
            fileConn <- file(..1)
            txt_chr <- readLines(fileConn, warn = FALSE)
            close(fileConn)
            if (is.null(..2)) {
                edit_fn <- function(x) {
                  x
                }
            }
            else {
                edit_fn <- ..2
            }
            rlang::exec(edit_fn, txt_chr, !!!..3)
        })
    write_to_delete_fls(intersect(tmp_paths_chr, dest_paths_chr), 
        consent_1L_chr = consent_1L_chr, consent_indcs_int = consent_indcs_int, 
        options_chr = options_chr, return_1L_lgl = FALSE)
    write_new_files(dest_paths_chr, text_ls = text_ls, consent_1L_chr = consent_1L_chr, 
        consent_indcs_int = consent_indcs_int, options_chr = options_chr, 
        return_1L_lgl = FALSE)
}
#' Write ingested dataverse file
#' @description write_ingested_dv_fl() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write ingested dataverse file. The function is called for its side effects and does not return a value.
#' @param ds_url_1L_chr Dataset url (a character vector of length one), Default: character(0)
#' @param dest_path_1L_chr Destination path (a character vector of length one)
#' @param repo_fl_fmt_1L_chr Repository file format (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param fl_nm_1L_chr File name (a character vector of length one), Default: 'NA'
#' @param fl_id_1L_int File identity (an integer vector of length one), Default: NA
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param save_type_1L_chr Save type (a character vector of length one), Default: 'original'
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @param ds_ui_1L_chr Dataset user interface (a character vector of length one), Default: deprecated()
#' @return No return value, called for side effects.
#' @rdname write_ingested_dv_fl
#' @export 
#' @importFrom lifecycle is_present deprecate_warn
#' @importFrom dataverse get_file
write_ingested_dv_fl <- function (ds_url_1L_chr = character(0), dest_path_1L_chr, repo_fl_fmt_1L_chr, 
    consent_1L_chr = "", consent_indcs_int = 1L, fl_nm_1L_chr = NA_character_, 
    fl_id_1L_int = NA_integer_, key_1L_chr = Sys.getenv("DATAVERSE_KEY"), 
    options_chr = c("Y", "N"), save_type_1L_chr = "original", 
    server_1L_chr = Sys.getenv("DATAVERSE_SERVER"), ds_ui_1L_chr = deprecated()) 
{
    if (lifecycle::is_present(ds_ui_1L_chr)) {
        lifecycle::deprecate_warn("0.1.10.9005", "ready4::write_ingested_dv_fl(ds_ui_1L_chr)", 
            details = "Please use `ready4::write_ingested_dv_fl(ds_url_1L_chr)`.")
        if (identical(ds_url_1L_chr, character(0))) {
            ds_url_1L_chr <- ds_ui_1L_chr
        }
    }
    prompt_1L_chr <- paste0("Do you confirm that you want to write the file ", 
        paste0(fl_nm_1L_chr, repo_fl_fmt_1L_chr), " to ", dest_path_1L_chr, 
        "?")
    if (!consent_1L_chr %in% c("Y", "N")) {
        consent_1L_chr <- make_prompt(prompt_1L_chr = prompt_1L_chr, 
            options_chr = c("Y", "N"), force_from_opts_1L_chr = TRUE)
    }
    object_xx <- get_gracefully(ds_url_1L_chr, fn = dataverse::get_file, 
        args_ls = list(file = ifelse(is.na(fl_id_1L_int), paste0(fl_nm_1L_chr, 
            repo_fl_fmt_1L_chr), fl_id_1L_int), format = save_type_1L_chr, 
            key = key_1L_chr, server = server_1L_chr))
    if (!is.null(object_xx)) {
        write_with_consent(consented_fn = writeBin, prompt_1L_chr = prompt_1L_chr, 
            consent_1L_chr = consent_1L_chr, consent_indcs_int = consent_indcs_int, 
            consented_args_ls = list(object = object_xx, con = dest_path_1L_chr), 
            consented_msg_1L_chr = paste0("New file created in ", 
                dest_path_1L_chr, " :\n", paste0(fl_nm_1L_chr, 
                  repo_fl_fmt_1L_chr)), declined_msg_1L_chr = "Write request cancelled - no new files have been written.", 
            options_chr = options_chr, return_1L_lgl = FALSE)
    }
    else {
        message("Write action cancelled - online resources could not be found.")
    }
}
#' Write library metadata
#' @description write_library_metadata() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write library metadata. The function is called for its side effects and does not return a value.
#' @param additions_tb Additions (a tibble), Default: make_additions_tb()
#' @param libraries_ls Libraries (a list), Default: NULL
#' @param libraries_tb Libraries (a tibble), Default: NULL
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param exclude_mthds_for_chr Exclude methods for (a character vector), Default: 'NA'
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @param include_1L_chr Include (a character vector of length one), Default: 'all'
#' @param module_pkgs_chr Module packages (a character vector), Default: character(0)
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param ns_var_nm_1L_chr Namespace variable name (a character vector of length one), Default: 'pt_ns_chr'
#' @param path_1L_chr Path (a character vector of length one), Default: character(0)
#' @param reference_var_nm_1L_chr Reference variable name (a character vector of length one), Default: 'Reference'
#' @param return_1L_chr Return (a character vector of length one), Default: 'all'
#' @param url_stub_1L_chr Url stub (a character vector of length one), Default: 'https://ready4-dev.github.io/'
#' @param vignette_var_nm_1L_chr Vignette variable name (a character vector of length one), Default: 'Vignettes'
#' @param vignette_url_var_nm_1L_chr Vignette url variable name (a character vector of length one), Default: 'Vignettes_URLs'
#' @param what_chr What (a character vector), Default: 'all'
#' @return No return value, called for side effects.
#' @rdname write_library_metadata
#' @export 
write_library_metadata <- function (additions_tb = make_additions_tb(), libraries_ls = NULL, 
    libraries_tb = NULL, consent_1L_chr = "", consent_indcs_int = 1L, 
    exclude_mthds_for_chr = NA_character_, gh_repo_1L_chr = "ready4-dev/ready4", 
    gh_tag_1L_chr = "Documentation_0.0", include_1L_chr = "all", 
    module_pkgs_chr = character(0), options_chr = c("Y", "N"), 
    ns_var_nm_1L_chr = "pt_ns_chr", path_1L_chr = character(0), 
    reference_var_nm_1L_chr = "Reference", return_1L_chr = "all", 
    url_stub_1L_chr = "https://ready4-dev.github.io/", vignette_var_nm_1L_chr = "Vignettes", 
    vignette_url_var_nm_1L_chr = "Vignettes_URLs", what_chr = "all") 
{
    update_list_1L_lgl <- update_table_1L_lgl <- TRUE
    if (is.null(libraries_tb)) {
        libraries_tb <- get_libraries_tb(gh_repo_1L_chr = gh_repo_1L_chr, 
            gh_tag_1L_chr = gh_tag_1L_chr)
        update_table_1L_lgl <- FALSE
    }
    if (!is.null(libraries_tb)) {
        if (!identical(additions_tb, make_additions_tb())) {
            update_list_1L_lgl <- update_table_1L_lgl <- TRUE
            libraries_tb <- libraries_tb %>% update_libraries_tb(include_1L_chr = include_1L_chr, 
                module_pkgs_chr = module_pkgs_chr, ns_var_nm_1L_chr = ns_var_nm_1L_chr, 
                reference_var_nm_1L_chr = reference_var_nm_1L_chr, 
                url_stub_1L_chr = url_stub_1L_chr, vignette_var_nm_1L_chr = vignette_var_nm_1L_chr, 
                vignette_url_var_nm_1L_chr = vignette_url_var_nm_1L_chr, 
                what_chr = what_chr)
            libraries_ls <- make_libraries_ls(libraries_tb = libraries_tb, 
                ns_var_nm_1L_chr = ns_var_nm_1L_chr)
        }
        else {
            if (is.null(libraries_ls)) {
                libraries_ls <- get_libraries_ls(gh_repo_1L_chr = gh_repo_1L_chr, 
                  gh_tag_1L_chr = gh_tag_1L_chr)
                update_list_1L_lgl <- FALSE
            }
        }
        env_objects_ls <- list()
        if (update_list_1L_lgl) {
            env_objects_ls$libraries_ls <- libraries_ls
        }
        if (update_table_1L_lgl) {
            env_objects_ls$libraries_tb <- libraries_tb
        }
        if (update_list_1L_lgl | update_table_1L_lgl) {
            env_objects_ls$methods_tb <- make_methods_tb(packages_tb = libraries_tb, 
                exclude_mthds_for_chr = exclude_mthds_for_chr, 
                gh_repo_1L_chr = gh_repo_1L_chr, gh_tag_1L_chr = gh_tag_1L_chr, 
                module_pkgs_chr = module_pkgs_chr, ns_var_nm_1L_chr = ns_var_nm_1L_chr, 
                path_1L_chr = path_1L_chr, return_1L_chr = return_1L_chr)
        }
        if (!identical(env_objects_ls, list())) {
            write_env_objs_to_dv(env_objects_ls = env_objects_ls, 
                consent_1L_chr = consent_1L_chr, consent_indcs_int = consent_indcs_int, 
                descriptions_chr = NULL, ds_url_1L_chr = character(0), 
                options_chr = options_chr, piggyback_desc_1L_chr = "Library metadata", 
                piggyback_tag_1L_chr = gh_tag_1L_chr, piggyback_to_1L_chr = gh_repo_1L_chr, 
                prerelease_1L_lgl = TRUE)
        }
    }
}
#' Write new credentials
#' @description write_new_credentials() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write new credentials. The function is called for its side effects and does not return a value.
#' @param path_to_file_1L_chr Path to file (a character vector of length one)
#' @param new_credentials_1L_chr New credentials (a character vector of length one)
#' @param old_credentials_1L_chr Old credentials (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @return No return value, called for side effects.
#' @rdname write_new_credentials
#' @export 
#' @importFrom stringr str_replace
write_new_credentials <- function (path_to_file_1L_chr, new_credentials_1L_chr, old_credentials_1L_chr, 
    consent_1L_chr = "", consent_indcs_int = 1L, options_chr = c("Y", 
        "N")) 
{
    consented_fn <- function(path_to_file_1L_chr, old_credentials_1L_chr, 
        new_credentials_1L_chr) {
        readLines(path_to_file_1L_chr) %>% stringr::str_replace(pattern = old_credentials_1L_chr, 
            replace = new_credentials_1L_chr) %>% writeLines(con = path_to_file_1L_chr)
    }
    write_with_consent(consented_fn = consented_fn, prompt_1L_chr = paste0("Do you confirm that you want to edit (overwrite) the file ", 
        path_to_file_1L_chr), consent_1L_chr = consent_1L_chr, 
        consent_indcs_int = consent_indcs_int, consented_args_ls = list(path_to_file_1L_chr = path_to_file_1L_chr, 
            old_credentials_1L_chr = old_credentials_1L_chr, 
            new_credentials_1L_chr = new_credentials_1L_chr), 
        consented_msg_1L_chr = paste0("The following file has been edited (overwritten): ", 
            path_to_file_1L_chr), declined_msg_1L_chr = "Write request cancelled - no files have been written.", 
        options_chr = options_chr, return_1L_lgl = FALSE)
}
#' Write new directories
#' @description write_new_dirs() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write new directories. The function is called for its side effects and does not return a value.
#' @param new_dirs_chr New directories (a character vector)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @return No return value, called for side effects.
#' @rdname write_new_dirs
#' @export 
#' @importFrom purrr map_lgl walk
write_new_dirs <- function (new_dirs_chr, consent_1L_chr = "", consent_indcs_int = 1L, 
    options_chr = c("Y", "N")) 
{
    new_dirs_chr <- new_dirs_chr[new_dirs_chr %>% purrr::map_lgl(~!dir.exists(.x))]
    if (!identical(new_dirs_chr, character(0))) {
        consented_fn <- function(new_dirs_chr) {
            paths_ls <- new_dirs_chr %>% purrr::walk(~{
                dir.create(.x)
            })
        }
        paths_ls <- write_with_consent(consented_fn = consented_fn, 
            prompt_1L_chr = paste0("Are you sure that you want to write the following director", 
                ifelse(length(new_dirs_chr) > 1, "ies", "y"), 
                " to your machine? \n", new_dirs_chr %>% paste0(collapse = "\n")), 
            consent_1L_chr = consent_1L_chr, consent_indcs_int = consent_indcs_int, 
            consented_args_ls = list(new_dirs_chr = new_dirs_chr), 
            consented_msg_1L_chr = paste0("New directories created:\n", 
                new_dirs_chr %>% paste0(collapse = "\n")), declined_msg_1L_chr = "Write request cancelled - no directories have been created.", 
            options_chr = options_chr, return_1L_lgl = TRUE)
    }
}
#' Write new files
#' @description write_new_files() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write new files. The function returns Object (an output object of multiple potential types).
#' @param paths_chr Paths (a character vector)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param custom_write_ls Custom write (a list), Default: NULL
#' @param fl_nm_1L_chr File name (a character vector of length one), Default: NULL
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param source_paths_ls Source paths (a list), Default: NULL
#' @param text_ls Text (a list), Default: NULL
#' @param return_1L_lgl Return (a logical vector of length one), Default: FALSE
#' @return Object (an output object of multiple potential types)
#' @rdname write_new_files
#' @export 
#' @importFrom purrr map flatten_chr map_chr map_lgl walk2 walk
#' @importFrom rlang exec
write_new_files <- function (paths_chr, consent_1L_chr = "", consent_indcs_int = 1L, 
    custom_write_ls = NULL, fl_nm_1L_chr = NULL, options_chr = c("Y", 
        "N"), source_paths_ls = NULL, text_ls = NULL, return_1L_lgl = FALSE) 
{
    if (!is.null(source_paths_ls)) {
        dest_dir_1L_chr <- paths_chr
        paths_chr <- purrr::map(source_paths_ls, ~{
            if (dir.exists(.x)) {
                list.files(.x, recursive = TRUE)
            }
            else {
                get_fl_nm_from_path(.x)
            }
        }) %>% purrr::flatten_chr() %>% purrr::map_chr(~paste0(dest_dir_1L_chr, 
            "/", ifelse(is.null(fl_nm_1L_chr), .x, fl_nm_1L_chr)))
        recursive_1L_lgl <- ifelse(source_paths_ls %>% purrr::map_lgl(~dir.exists(.x)) %>% 
            any(), TRUE, FALSE)
    }
    new_files_chr <- paths_chr[paths_chr %>% purrr::map_lgl(~!file.exists(.x))]
    overwritten_files_chr <- setdiff(paths_chr, new_files_chr)
    object_xx <- NULL
    if (!identical(paths_chr, character(0))) {
        consented_fn <- function(paths_chr, consent_1L_chr, custom_write_ls, 
            fl_nm_1L_chr, source_paths_ls, text_ls) {
            if (!is.null(text_ls)) {
                purrr::walk2(paths_chr, text_ls, ~{
                  file_conn <- file(.x)
                  writeLines(.y, file_conn)
                  close(file_conn)
                })
            }
            else {
                if (!is.null(source_paths_ls)) {
                  source_paths_chr <- purrr::map(source_paths_ls, 
                    ~{
                      if (dir.exists(.x)) {
                        list.files(.x, full.names = TRUE)
                      }
                      else {
                        .x
                      }
                    }) %>% purrr::flatten_chr()
                  purrr::walk(source_paths_chr, ~file.copy(.x, 
                    paste0(dest_dir_1L_chr, ifelse(is.null(fl_nm_1L_chr), 
                      "", paste0("/", fl_nm_1L_chr))), overwrite = TRUE, 
                    recursive = recursive_1L_lgl))
                }
                if (!is.null(custom_write_ls)) {
                  if ("consent_1L_chr" %in% formalArgs(custom_write_ls$fn)) 
                    custom_write_ls$args_ls$consent_1L_chr <- consent_1L_chr
                  rlang::exec(custom_write_ls$fn, !!!custom_write_ls$args_ls)
                }
            }
        }
        object_xx <- write_with_consent(consented_fn = consented_fn, 
            prompt_1L_chr = paste0("Are you sure that you want to write / overwrite the following file", 
                ifelse(length(paths_chr) > 1, "s", ""), " to your machine: \n", 
                ifelse(identical(new_files_chr, character(0)), 
                  "", paste0(" \n", new_files_chr %>% paste0(collapse = "\n"))), 
                ifelse(identical(overwritten_files_chr, character(0)), 
                  "", paste0(" \n", overwritten_files_chr %>% 
                    paste0(collapse = "\n"))), "?"), consent_1L_chr = consent_1L_chr, 
            consent_indcs_int = consent_indcs_int, consented_args_ls = list(paths_chr = paths_chr, 
                consent_1L_chr = options_chr[consent_indcs_int[1]], 
                custom_write_ls = custom_write_ls, fl_nm_1L_chr = fl_nm_1L_chr, 
                source_paths_ls = source_paths_ls, text_ls = text_ls), 
            consented_msg_1L_chr = paste0("The following file", 
                ifelse(length(paths_chr) > 1, "s have been", 
                  " has been"), " written to your machine: \n", 
                ifelse(identical(new_files_chr, character(0)), 
                  "", paste0(" \n", new_files_chr %>% paste0(collapse = "\n"))), 
                ifelse(identical(overwritten_files_chr, character(0)), 
                  "", paste0(" \n", overwritten_files_chr %>% 
                    paste0(collapse = "\n"))), "."), declined_msg_1L_chr = "Write request cancelled - no files have been written.", 
            options_chr = options_chr, return_1L_lgl = TRUE)
    }
    if (return_1L_lgl) 
        return(object_xx)
}
#' Write object with prompt
#' @description write_obj_with_prompt() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write object with prompt. The function is called for its side effects and does not return a value.
#' @param object_xx Object (an output object of multiple potential types)
#' @param obj_nm_1L_chr Object name (a character vector of length one)
#' @param outp_dir_1L_chr Output directory (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param return_1L_lgl Return (a logical vector of length one), Default: FALSE
#' @return No return value, called for side effects.
#' @rdname write_obj_with_prompt
#' @export 
#' @importFrom rlang exec
write_obj_with_prompt <- function (object_xx, obj_nm_1L_chr, outp_dir_1L_chr, consent_1L_chr = "", 
    consent_indcs_int = 1L, options_chr = c("Y", "N"), return_1L_lgl = FALSE) 
{
    path_1L_chr <- paste0(outp_dir_1L_chr, "/", obj_nm_1L_chr, 
        ".RDS")
    custom_write_ls = list(fn = saveRDS, args_ls = list(object = object_xx, 
        file = path_1L_chr))
    if (!consent_1L_chr %in% options_chr) {
        write_new_files(path_1L_chr, custom_write_ls = custom_write_ls, 
            consent_1L_chr = consent_1L_chr, consent_indcs_int = consent_indcs_int, 
            options_chr = options_chr, return_1L_lgl = return_1L_lgl)
    }
    else {
        if (consent_1L_chr %in% options_chr[consent_indcs_int]) 
            rlang::exec(custom_write_ls$fn, !!!custom_write_ls$args_ls)
    }
}
#' Write project output directories
#' @description write_prj_outp_dirs() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write project output directories. The function returns New paths (a list).
#' @param prj_dirs_chr Project directories (a character vector)
#' @param output_data_dir_1L_chr Output data directory (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param paths_ls Paths (a list), Default: NULL
#' @return New paths (a list)
#' @rdname write_prj_outp_dirs
#' @export 
#' @importFrom purrr walk keep
#' @importFrom stats setNames
write_prj_outp_dirs <- function (prj_dirs_chr, output_data_dir_1L_chr, consent_1L_chr = "", 
    consent_indcs_int = 1L, options_chr = c("Y", "N"), paths_ls = NULL) 
{
    paths_chr <- paste0(paste0(output_data_dir_1L_chr, "/"), 
        prj_dirs_chr)
    if (!consent_1L_chr %in% options_chr) {
        write_new_dirs(paths_chr, consent_1L_chr = consent_1L_chr, 
            consent_indcs_int = consent_indcs_int, options_chr = options_chr)
    }
    else {
        consented_fn <- function(paths_chr) {
            new_paths_ls <- paths_chr %>% purrr::walk(~{
                dir.create(.x)
            })
        }
        write_with_consent(consented_fn = consented_fn, prompt_1L_chr = "", 
            consent_1L_chr = consent_1L_chr, consent_indcs_int = consent_indcs_int, 
            consented_args_ls = list(paths_chr = paths_chr), 
            consented_msg_1L_chr = paste0("The following folder", 
                ifelse(length(paths_chr) > 1, "s have been", 
                  " has been"), " written to your machine: \n", 
                ifelse(identical(paths_chr, character(0)), "", 
                  paste0(" \n", paths_chr %>% paste0(collapse = "\n"))), 
                "."), declined_msg_1L_chr = "Write request cancelled - no files have been written.", 
            options_chr = options_chr, return_1L_lgl = FALSE)
    }
    new_paths_ls <- as.list(paths_chr) %>% stats::setNames(prj_dirs_chr) %>% 
        purrr::keep(dir.exists)
    if (!is.null(paths_ls)) 
        new_paths_ls <- append(new_paths_ls, paths_ls)
    return(new_paths_ls)
}
#' Write tibble to comma separated variables file
#' @description write_tb_to_csv() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write tibble to comma separated variables file. The function is called for its side effects and does not return a value.
#' @param tbs_r4 Tibbles (a ready4 module)
#' @param slot_nm_1L_chr Slot name (a character vector of length one)
#' @param r4_name_1L_chr Ready4 module name (a character vector of length one)
#' @param lup_dir_1L_chr Lookup table directory (a character vector of length one)
#' @param pfx_1L_chr Prefix (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @return No return value, called for side effects.
#' @rdname write_tb_to_csv
#' @export 
#' @importFrom methods slot
#' @importFrom dplyr mutate across
#' @importFrom tidyselect where
#' @importFrom stringr str_c
#' @importFrom utils write.csv
write_tb_to_csv <- function (tbs_r4, slot_nm_1L_chr, r4_name_1L_chr, lup_dir_1L_chr, 
    pfx_1L_chr, consent_1L_chr = "", consent_indcs_int = 1L, 
    options_chr = c("Y", "N")) 
{
    file_path_1L_chr <- paste0(lup_dir_1L_chr, "/", pfx_1L_chr, 
        "_", slot_nm_1L_chr, ".csv")
    consented_fn <- function(file_path_1L_chr, tbs_r4, slot_nm_1L_chr) {
        methods::slot(tbs_r4, slot_nm_1L_chr) %>% dplyr::mutate(dplyr::across(tidyselect::where(is.list), 
            ~suppressWarnings(ifelse(stringr::str_c(.x) == "NULL", 
                NA_character_, stringr::str_c(.x))))) %>% utils::write.csv(file = file_path_1L_chr, 
            row.names = FALSE)
    }
    write_with_consent(consented_fn = consented_fn, prompt_1L_chr = paste0("Do you confirm that you want to write the file ", 
        file_path_1L_chr, " ? "), consent_1L_chr = consent_1L_chr, 
        consent_indcs_int = consent_indcs_int, consented_args_ls = list(file_path_1L_chr = file_path_1L_chr, 
            tbs_r4 = tbs_r4, slot_nm_1L_chr = slot_nm_1L_chr), 
        consented_msg_1L_chr = paste0("New file created:\n", 
            file_path_1L_chr), declined_msg_1L_chr = "Write request cancelled - no new files have been created.", 
        options_chr = options_chr, return_1L_lgl = FALSE)
}
#' Write a local copy of RMD or Rmarkdown files
#' @description write_to_copy_rmds() is used to copy template RMD or Rmarkdown files to specified sub-directories of a model documentation website. These template copies can then be manually edited before being rendered with write_to_render_post().
#' @param dir_path_1L_chr Directory path (a character vector of length one)
#' @param fl_nm_1L_chr File name (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param rmds_dir_1L_chr R Markdowns directory (a character vector of length one), Default: 'R/RMD Templates'
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param return_1L_lgl Return (a logical vector of length one), Default: FALSE
#' @return No return value, called for side effects.
#' @rdname write_to_copy_rmds
#' @export 
#' @importFrom purrr walk
write_to_copy_rmds <- function (dir_path_1L_chr, fl_nm_1L_chr, consent_1L_chr = "", 
    rmds_dir_1L_chr = "R/RMD Templates", consent_indcs_int = 1L, 
    options_chr = c("Y", "N"), return_1L_lgl = FALSE) 
{
    file_nms_chr <- list.files(rmds_dir_1L_chr)
    destination_1L_chr <- paste0(dir_path_1L_chr, "/", fl_nm_1L_chr)
    if (!dir.exists(destination_1L_chr)) 
        dir.create(destination_1L_chr)
    purrr::walk(file_nms_chr, ~write_new_files(destination_1L_chr, 
        source_paths_ls = list(paste0(rmds_dir_1L_chr, "/", .x)), 
        consent_1L_chr = consent_1L_chr, fl_nm_1L_chr = .x, consent_indcs_int = consent_indcs_int, 
        options_chr = options_chr, return_1L_lgl = return_1L_lgl))
}
#' Write to delete directories
#' @description write_to_delete_dirs() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write to delete directories. The function is called for its side effects and does not return a value.
#' @param dir_paths_chr Directory paths (a character vector)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @return No return value, called for side effects.
#' @rdname write_to_delete_dirs
#' @export 
#' @importFrom purrr map_lgl map flatten_chr walk
write_to_delete_dirs <- function (dir_paths_chr, consent_1L_chr = "", consent_indcs_int = 1L, 
    options_chr = c("Y", "N")) 
{
    dir_paths_chr <- dir_paths_chr[dir_paths_chr %>% purrr::map_lgl(~dir.exists(.x))]
    if (!identical(dir_paths_chr, character(0))) {
        fls_to_be_purged_chr <- dir_paths_chr %>% purrr::map(~list.files(.x, 
            full.names = TRUE)) %>% purrr::flatten_chr()
        consented_fn <- function(dir_paths_chr) {
            dir_paths_chr %>% purrr::walk(~unlink(.x, recursive = TRUE))
        }
        write_with_consent(consented_fn = consented_fn, prompt_1L_chr = paste0("Are you sure that you want to delete the following director", 
            ifelse(length(dir_paths_chr) > 1, "ies", "y"), ":\n", 
            dir_paths_chr %>% paste0(collapse = "\n"), ifelse(identical(fls_to_be_purged_chr, 
                character(0)), "", paste0(" and the following file", 
                ifelse(length(fls_to_be_purged_chr) > 1, "s:\n", 
                  ":\n"), fls_to_be_purged_chr %>% paste0(collapse = "\n"))), 
            " from your machine: \n", "?"), consent_1L_chr = consent_1L_chr, 
            consent_indcs_int = consent_indcs_int, consented_args_ls = list(dir_paths_chr = dir_paths_chr), 
            consented_msg_1L_chr = paste0("The following director", 
                ifelse(length(dir_paths_chr) > 1, "ies", "y"), 
                ":\n", dir_paths_chr %>% paste0(collapse = "\n"), 
                ifelse(identical(fls_to_be_purged_chr, character(0)), 
                  "", paste0(" and the following file", ifelse(length(fls_to_be_purged_chr) > 
                    1, "s:\n", ":\n"), fls_to_be_purged_chr %>% 
                    paste0(collapse = "\n"))), ifelse((length(dir_paths_chr) + 
                  length(fls_to_be_purged_chr)) > 1, " have been", 
                  " has been"), " deleted from your machine."), 
            declined_msg_1L_chr = "Delete directory request cancelled - no directories deleted.", 
            options_chr = options_chr, return_1L_lgl = FALSE)
    }
}
#' Write to delete files
#' @description write_to_delete_fls() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write to delete files. The function returns Object (an output object of multiple potential types).
#' @param file_paths_chr File paths (a character vector)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param return_1L_lgl Return (a logical vector of length one), Default: FALSE
#' @return Object (an output object of multiple potential types)
#' @rdname write_to_delete_fls
#' @export 
#' @importFrom purrr map_lgl
write_to_delete_fls <- function (file_paths_chr, consent_1L_chr = "", consent_indcs_int = 1L, 
    options_chr = c("Y", "N"), return_1L_lgl = FALSE) 
{
    file_paths_chr <- file_paths_chr[file_paths_chr %>% purrr::map_lgl(~file.exists(.x))]
    object_xx <- NULL
    if (!identical(file_paths_chr, character(0))) {
        object_xx <- write_with_consent(consented_fn = do.call, 
            prompt_1L_chr = paste0("Are you sure that you want to delete the following file", 
                ifelse(length(file_paths_chr) > 1, "s", ""), 
                " from your machine: \n", file_paths_chr %>% 
                  paste0(collapse = "\n"), "?"), consent_1L_chr = consent_1L_chr, 
            consent_indcs_int = consent_indcs_int, consented_args_ls = list(what = file.remove, 
                args = list(file_paths_chr)), consented_msg_1L_chr = "File deletion request has been executed.", 
            declined_msg_1L_chr = "File deletion request cancelled - no files have been deleted.", 
            options_chr = options_chr, return_1L_lgl = TRUE)
    }
    if (return_1L_lgl) 
        return(object_xx)
}
#' Write to dataverse from table
#' @description write_to_dv_from_tbl() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write to dataverse from table. The function returns File identities (an integer vector).
#' @param files_tb Files (a tibble)
#' @param ds_url_1L_chr Dataset url (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param data_dir_rt_1L_chr Data directory root (a character vector of length one), Default: '.'
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return File identities (an integer vector)
#' @rdname write_to_dv_from_tbl
#' @export 
#' @importFrom dataverse get_dataset
#' @importFrom purrr pmap_int
write_to_dv_from_tbl <- function (files_tb, ds_url_1L_chr, consent_1L_chr = "", consent_indcs_int = 1L, 
    data_dir_rt_1L_chr = ".", key_1L_chr = Sys.getenv("DATAVERSE_KEY"), 
    options_chr = c("Y", "N"), server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    fl_ids_int <- NULL
    consented_fn <- function(files_tb, consent_indcs_int, data_dir_rt_1L_chr, 
        ds_url_1L_chr, key_1L_chr, options_chr, server_1L_chr) {
        fl_ids_int <- NULL
        ds_ls <- get_gracefully(ds_url_1L_chr, fn = dataverse::get_dataset)
        if (!is.null(ds_ls)) {
            is_draft_1L_lgl <- ds_ls$versionState == "DRAFT"
            nms_chr <- ds_ls$files$filename
            fl_ids_int <- purrr::pmap_int(files_tb, ~{
                path_1L_chr <- paste0(ifelse(identical(character(0), 
                  data_dir_rt_1L_chr), "", paste0(data_dir_rt_1L_chr, 
                  "/")), ..1, "/", ..2, ..3)
                fl_nm_1L_chr <- paste0(..2, ..3)
                id_1L_int <- write_fls_to_dv(path_1L_chr, consent_1L_chr = consent_1L_chr, 
                  consent_indcs_int = consent_indcs_int, descriptions_chr = ..4, 
                  ds_url_1L_chr = ds_url_1L_chr, ds_ls = ds_ls, 
                  key_1L_chr = key_1L_chr, options_chr = options_chr, 
                  server_1L_chr = server_1L_chr)
                if (id_1L_int) {
                  id_1L_int <- NA_integer_
                }
                id_1L_int
            })
        }
        else {
            message("Write action cancelled - online resources could not be found.")
        }
        return(fl_ids_int)
    }
    paths_chr <- paste0(ifelse(identical(character(0), data_dir_rt_1L_chr), 
        "", paste0(data_dir_rt_1L_chr, "/")), files_tb[, 1], 
        "/", files_tb[, 2], files_tb[, 3])
    fl_ids_int <- write_with_consent(consented_fn = consented_fn, 
        prompt_1L_chr = paste0("Do you confirm that you want to upload the file", 
            ifelse(length(paths_chr) > 1, "s ", " "), make_list_phrase(paths_chr), 
            " to dataverse ", ds_url_1L_chr, " ? "), consent_1L_chr = consent_1L_chr, 
        consent_indcs_int = consent_indcs_int, consented_args_ls = list(files_tb = files_tb, 
            consent_indcs_int = consent_indcs_int, data_dir_rt_1L_chr = data_dir_rt_1L_chr, 
            ds_url_1L_chr = ds_url_1L_chr, key_1L_chr = key_1L_chr, 
            options_chr = options_chr, server_1L_chr = server_1L_chr), 
        consented_msg_1L_chr = character(0), declined_msg_1L_chr = "Write request cancelled - no files uploaded.", 
        options_chr = options_chr, return_1L_lgl = TRUE)
    return(fl_ids_int)
}
#' Write to dataverse with wait
#' @description write_to_dv_with_wait() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write to dataverse with wait. The function returns Dataset (a list).
#' @param dss_tb Datasets (a tibble)
#' @param dv_nm_1L_chr Dataverse name (a character vector of length one)
#' @param ds_url_1L_chr Dataset url (a character vector of length one)
#' @param parent_dv_dir_1L_chr Parent dataverse directory (a character vector of length one)
#' @param paths_to_dirs_chr Paths to directories (a character vector)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param make_local_copy_1L_lgl Make local copy (a logical vector of length one), Default: FALSE
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param paths_are_rltv_1L_lgl Paths are relative (a logical vector of length one), Default: TRUE
#' @param inc_fl_types_chr Include file types (a character vector), Default: 'NA'
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @param wait_time_in_secs_int Wait time in secs (an integer vector), Default: 5
#' @return Dataset (a list)
#' @rdname write_to_dv_with_wait
#' @export 
#' @importFrom stats setNames
#' @importFrom purrr pmap map_int
#' @importFrom dataverse get_dataset
write_to_dv_with_wait <- function (dss_tb, dv_nm_1L_chr, ds_url_1L_chr, parent_dv_dir_1L_chr, 
    paths_to_dirs_chr, consent_1L_chr = "", consent_indcs_int = 1L, 
    make_local_copy_1L_lgl = FALSE, options_chr = c("Y", "N"), 
    paths_are_rltv_1L_lgl = TRUE, inc_fl_types_chr = NA_character_, 
    key_1L_chr = Sys.getenv("DATAVERSE_KEY"), server_1L_chr = Sys.getenv("DATAVERSE_SERVER"), 
    wait_time_in_secs_int = 5L) 
{
    ds_ls <- NULL
    ds_chr <- dss_tb$ds_obj_nm_chr
    files_tb <- make_files_tb(paths_to_dirs_chr = paths_to_dirs_chr, 
        recode_ls = dss_tb$title_chr %>% as.list() %>% stats::setNames(ds_chr), 
        inc_fl_types_chr = inc_fl_types_chr)
    if (paths_are_rltv_1L_lgl) {
        data_dir_rt_1L_chr <- "."
    }
    else {
        data_dir_rt_1L_chr <- character(0)
    }
    paths_chr <- paste0(ifelse(identical(character(0), data_dir_rt_1L_chr), 
        "", paste0(data_dir_rt_1L_chr, "/")), files_tb %>% purrr::pmap(~paste0(paste0(..1, 
        "/", ..2, ..3))))
    consented_fn <- function(consent_1L_chr, consent_indcs_int, 
        dv_nm_1L_chr, ds_url_1L_chr, files_tb, key_1L_chr, make_local_copy_1L_lgl, 
        options_chr, parent_dv_dir_1L_chr, server_1L_chr, wait_time_in_secs_int) {
        fl_ids_int <- 1:nrow(files_tb) %>% purrr::map_int(~{
            Sys.sleep(wait_time_in_secs_int)
            write_to_dv_from_tbl(files_tb[.x, ], consent_1L_chr = consent_1L_chr, 
                consent_indcs_int = consent_indcs_int, data_dir_rt_1L_chr = data_dir_rt_1L_chr, 
                ds_url_1L_chr = ds_url_1L_chr, key_1L_chr = key_1L_chr, 
                options_chr = options_chr, server_1L_chr = server_1L_chr)
        })
        ds_ls <- get_gracefully(ds_url_1L_chr, fn = dataverse::get_dataset)
        if (!is.null(ds_ls)) {
            if (make_local_copy_1L_lgl | ds_ls$versionState != 
                "DRAFT") {
                dv_dir_1L_chr <- paste0(parent_dv_dir_1L_chr, 
                  "/", dv_nm_1L_chr)
                if (!dir.exists(dv_dir_1L_chr)) {
                  write_new_dirs(dv_dir_1L_chr, consent_1L_chr = consent_1L_chr, 
                    consent_indcs_int = consent_indcs_int, options_chr = options_chr)
                }
                local_dv_dir_1L_chr <- paste0(dv_dir_1L_chr, 
                  "/", ds_ls$metadataBlocks$citation$fields$value[[3]])
                if (!dir.exists(local_dv_dir_1L_chr)) {
                  write_new_dirs(local_dv_dir_1L_chr, consent_1L_chr = consent_1L_chr, 
                    consent_indcs_int = consent_indcs_int, options_chr = options_chr)
                }
                write_fls_from_dv(consent_1L_chr = consent_1L_chr, 
                  consent_indcs_int = consent_indcs_int, ds_url_1L_chr = ds_url_1L_chr, 
                  files_tb = files_tb, fl_ids_int = fl_ids_int, 
                  local_dv_dir_1L_chr = local_dv_dir_1L_chr, 
                  options_chr = options_chr)
            }
        }
        else {
            message("Write action cancelled - online resources could not be found.")
        }
        return(ds_ls)
    }
    ds_ls <- write_with_consent(consented_fn = consented_fn, 
        prompt_1L_chr = paste0("Do you confirm ('Y') that you want to write the file", 
            ifelse(length(paths_chr) > 1, "s ", " "), make_list_phrase(paths_chr), 
            " to dataverse ", ds_url_1L_chr, " ? "), consent_1L_chr = consent_1L_chr, 
        consent_indcs_int = consent_indcs_int, consented_args_ls = list(consent_1L_chr = consent_1L_chr, 
            consent_indcs_int = consent_indcs_int, dv_nm_1L_chr = dv_nm_1L_chr, 
            ds_url_1L_chr = ds_url_1L_chr, files_tb = files_tb, 
            key_1L_chr = key_1L_chr, make_local_copy_1L_lgl = make_local_copy_1L_lgl, 
            options_chr = options_chr, parent_dv_dir_1L_chr = parent_dv_dir_1L_chr, 
            server_1L_chr = server_1L_chr, wait_time_in_secs_int = wait_time_in_secs_int), 
        consented_msg_1L_chr = character(0), declined_msg_1L_chr = "Write request cancelled - no files uploaded.", 
        options_chr = options_chr, return_1L_lgl = TRUE)
    return(ds_ls)
}
#' Write to edit workflow
#' @description write_to_edit_workflow() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write to edit workflow. The function is called for its side effects and does not return a value.
#' @param fl_nm_1L_chr File name (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param dir_path_1L_chr Directory path (a character vector of length one), Default: '.github/workflows'
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @return No return value, called for side effects.
#' @rdname write_to_edit_workflow
#' @export 
write_to_edit_workflow <- function (fl_nm_1L_chr, consent_1L_chr = "", consent_indcs_int = 1L, 
    dir_path_1L_chr = ".github/workflows", options_chr = c("Y", 
        "N")) 
{
    path_1L_chr <- paste0(dir_path_1L_chr, "/", fl_nm_1L_chr)
    workflow_chr <- readLines(path_1L_chr)
    index_1L_int <- which(workflow_chr == "      - uses: r-lib/actions/setup-pandoc@v2")
    workflow_chr <- c(workflow_chr[1:index_1L_int - 1], c("      - uses: r-lib/actions/setup-tinytex@v2", 
        ""), workflow_chr[index_1L_int:length(workflow_chr)])
    index_1L_int <- which(workflow_chr == "      - uses: r-lib/actions/setup-r-dependencies@v2")
    workflow_chr <- c(workflow_chr[1:index_1L_int - 1], c("    # Addresses issue with incompatibility between libcurl4-gnutls-dev and libcurl4-openssl-dev", 
        "    # Below fix is a customisation of approach outlined in https://github.com/r-hub/sysreqsdb/issues/77#issuecomment-620025428", 
        "      - name: Install libraptor on Linux", "        if: runner.os == 'Linux'", 
        "        run: |", "          sudo add-apt-repository ppa:cran/librdf", 
        "          sudo apt update", ""), workflow_chr[index_1L_int:length(workflow_chr)])
    write_with_consent(consented_fn = writeLines, prompt_1L_chr = paste0("Do you confirm that you want to write the file ", 
        fl_nm_1L_chr, " to ", dir_path_1L_chr, "?"), consent_1L_chr = consent_1L_chr, 
        consent_indcs_int = consent_indcs_int, consented_args_ls = list(text = workflow_chr, 
            con = path_1L_chr), consented_msg_1L_chr = paste0("File ", 
            fl_nm_1L_chr, " has been written to ", dir_path_1L_chr, 
            "."), declined_msg_1L_chr = "Write request cancelled - no new files have been written.", 
        options_chr = options_chr)
}
#' Write to force links in
#' @description write_to_force_links_in() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write to force links in. The function is called for its side effects and does not return a value.
#' @param path_to_mkdn_1L_chr Path to markdown (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param shorten_doi_1L_lgl Shorten digital object identifier (a logical vector of length one), Default: TRUE
#' @return No return value, called for side effects.
#' @rdname write_to_force_links_in
#' @export 
#' @importFrom purrr map_chr
#' @importFrom stringr str_match str_remove str_replace
write_to_force_links_in <- function (path_to_mkdn_1L_chr, consent_1L_chr = "", consent_indcs_int = 1L, 
    options_chr = c("Y", "N"), shorten_doi_1L_lgl = TRUE) 
{
    file_chr <- readLines(path_to_mkdn_1L_chr)
    file_chr <- file_chr %>% purrr::map_chr(~{
        url_1L_chr <- paste0("https://", stringr::str_match(.x, 
            "<https://\\s*(.*?)\\s*>")[2])
        link_1L_chr <- paste0("<a href=\"", url_1L_chr, "\">", 
            ifelse(shorten_doi_1L_lgl, stringr::str_remove(url_1L_chr, 
                "https://doi.org/"), url_1L_chr), "</a>")
        stringr::str_replace(.x, "<https://\\s*(.*?)\\s*>", link_1L_chr)
    })
    write_with_consent(consented_fn = writeLines, prompt_1L_chr = paste0("Do you confirm that you wish to edit (overwrite) the file", 
        path_to_mkdn_1L_chr, "?"), consent_1L_chr = consent_1L_chr, 
        consent_indcs_int = consent_indcs_int, consented_args_ls = list(text = file_chr, 
            con = path_to_mkdn_1L_chr), consented_msg_1L_chr = paste0("The file ", 
            path_to_mkdn_1L_chr, " has been edited (overwritten)."), 
        declined_msg_1L_chr = "Write request cancelled - no changes were made to the file.", 
        options_chr = options_chr, return_1L_lgl = FALSE)
}
#' Write to publish dataverse dataset
#' @description write_to_publish_dv_ds() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write to publish dataverse dataset. The function is called for its side effects and does not return a value.
#' @param dv_ds_1L_chr Dataverse dataset (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param minor_1L_lgl Minor (a logical vector of length one), Default: FALSE
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @return No return value, called for side effects.
#' @rdname write_to_publish_dv_ds
#' @export 
#' @importFrom dataverse publish_dataset
write_to_publish_dv_ds <- function (dv_ds_1L_chr, consent_1L_chr = "", consent_indcs_int = 1L, 
    minor_1L_lgl = FALSE, options_chr = c("Y", "N")) 
{
    write_with_consent(consented_fn = dataverse::publish_dataset, 
        prompt_1L_chr = paste0("Do you confirm that you wish to publish the current draft of dataverse ", 
            dv_ds_1L_chr, "?"), consent_1L_chr = consent_1L_chr, 
        consent_indcs_int = consent_indcs_int, consented_args_ls = list(dataset = dv_ds_1L_chr, 
            minor = minor_1L_lgl), consented_msg_1L_chr = character(0), 
        declined_msg_1L_chr = "Publish request cancelled - no changes has been made to dataverse dataset visibility.", 
        options_chr = options_chr, return_1L_lgl = FALSE)
}
#' Write ready4 model documentation website page from an RMD or Rmarkdown file
#' @description write_to_render_post() is designed for help overcome practical challenges of rendering RMD or Rmarkdown files to Markdown output in a modelling project's Hugo Docsy documentation website. You must have 'hugodown' installed for this function to work.
#' @param included_dirs_chr Included directories (a character vector)
#' @param path_to_main_dir_1L_chr Path to main directory (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param is_rmd_1L_lgl Is Markdown (a logical vector of length one), Default: TRUE
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @return No return value, called for side effects.
#' @rdname write_to_render_post
#' @export 
#' @importFrom purrr walk
#' @importFrom rmarkdown render
write_to_render_post <- function (included_dirs_chr, path_to_main_dir_1L_chr, consent_1L_chr = "", 
    consent_indcs_int = 1L, is_rmd_1L_lgl = TRUE, options_chr = c("Y", 
        "N")) 
{
    note_1L_chr <- "To use this function, the non-CRAN R package 'hugodown' must be installed."
    if (!requireNamespace("rmarkdown", quietly = TRUE)) {
        stop("rmarkdown package is required - please install it and rerun the last command.")
    }
    consented_fn <- function(consent_1L_chr, consent_indcs_int, 
        included_dirs_chr, is_rmd_1L_lgl, options_chr, path_to_main_dir_1L_chr) {
        included_dirs_chr %>% purrr::walk(~{
            if (is_rmd_1L_lgl) {
                write_blog_entries(dir_path_1L_chr = path_to_main_dir_1L_chr, 
                  consent_1L_chr = consent_1L_chr, consent_indcs_int = consent_indcs_int, 
                  fl_nm_1L_chr = .x, options_chr = options_chr)
            }
            else {
                rmarkdown::render(paste0(path_to_main_dir_1L_chr, 
                  "/", .x, "/index.en.Rmarkdown"))
            }
        })
    }
    write_with_consent(consented_fn = consented_fn, prompt_1L_chr = paste0("Do you confirm that you wish to render posts in ", 
        make_list_phrase(included_dirs_chr), "?"), consent_1L_chr = consent_1L_chr, 
        consent_indcs_int = consent_indcs_int, consented_args_ls = list(consent_1L_chr = consent_1L_chr, 
            consent_indcs_int = consent_indcs_int, included_dirs_chr = included_dirs_chr, 
            is_rmd_1L_lgl = is_rmd_1L_lgl, options_chr = options_chr, 
            path_to_main_dir_1L_chr = path_to_main_dir_1L_chr), 
        consented_msg_1L_chr = paste0("Posts have been rendered in ", 
            make_list_phrase(included_dirs_chr), "."), declined_msg_1L_chr = "Render request cancelled - no posts have been rendered.", 
        options_chr = options_chr, return_1L_lgl = FALSE)
}
#' Write to trim html
#' @description write_to_trim_html() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write to trim html. The function is called for its side effects and does not return a value.
#' @param path_to_html_1L_chr Path to html (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @return No return value, called for side effects.
#' @rdname write_to_trim_html
#' @export 
write_to_trim_html <- function (path_to_html_1L_chr, consent_1L_chr = "", consent_indcs_int = 1L, 
    options_chr = c("Y", "N")) 
{
    file_chr <- readLines(path_to_html_1L_chr)
    file_chr <- file_chr[file_chr != "<!DOCTYPE html>"]
    file_chr <- file_chr[c(1:(which(file_chr == "<head>") - 1), 
        (which(file_chr == "</head>") + 1):length(file_chr))]
    file_chr <- file_chr[file_chr != "<div class=\"container-fluid main-container\">"]
    file_chr <- file_chr[c(1:(max(which(file_chr == "</div>")) - 
        1), (max(which(file_chr == "</div>")) + 1):length(file_chr))]
    write_with_consent(consented_fn = writeLines, prompt_1L_chr = paste0("Do you confirm that you want to edit (overwrite) the file ", 
        path_to_html_1L_chr, "?"), consent_1L_chr = consent_1L_chr, 
        consent_indcs_int = consent_indcs_int, consented_args_ls = list(text = file_chr, 
            con = path_to_html_1L_chr), consented_msg_1L_chr = paste0("The file ", 
            path_to_html_1L_chr, " has been edited (overwritten)."), 
        declined_msg_1L_chr = "Edit request cancelled - no files have been changed.", 
        options_chr = options_chr, return_1L_lgl = FALSE)
}
#' Write with consent
#' @description write_with_consent() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write with consent. The function returns Object (an output object of multiple potential types).
#' @param consented_fn Consented (a function)
#' @param prompt_1L_chr Prompt (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consented_args_ls Consented arguments (a list), Default: NULL
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param consented_msg_1L_chr Consented message (a character vector of length one), Default: character(0)
#' @param declined_args_ls Declined arguments (a list), Default: NULL
#' @param declined_fn Declined (a function), Default: NULL
#' @param declined_msg_1L_chr Declined message (a character vector of length one), Default: 'No files have been written.'
#' @param force_from_opts_1L_chr Force from opts (a character vector of length one), Default: TRUE
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param return_1L_lgl Return (a logical vector of length one), Default: FALSE
#' @return Object (an output object of multiple potential types)
#' @rdname write_with_consent
#' @export 
#' @importFrom rlang exec
write_with_consent <- function (consented_fn, prompt_1L_chr, consent_1L_chr = "", consented_args_ls = NULL, 
    consent_indcs_int = 1L, consented_msg_1L_chr = character(0), 
    declined_args_ls = NULL, declined_fn = NULL, declined_msg_1L_chr = "No files have been written.", 
    force_from_opts_1L_chr = TRUE, options_chr = c("Y", "N"), 
    return_1L_lgl = FALSE) 
{
    if (!consent_1L_chr %in% options_chr && !identical(prompt_1L_chr, 
        character(0))) {
        consent_1L_chr <- make_prompt(prompt_1L_chr = prompt_1L_chr, 
            options_chr = options_chr, force_from_opts_1L_chr = force_from_opts_1L_chr)
    }
    object_xx <- NULL
    if (consent_1L_chr %in% options_chr[consent_indcs_int]) {
        object_xx <- rlang::exec(consented_fn, !!!consented_args_ls)
        if (!identical(consented_msg_1L_chr, character(0))) 
            message(consented_msg_1L_chr)
    }
    else {
        if (!is.null(declined_fn)) {
            object_xx <- rlang::exec(declined_fn, !!!declined_args_ls)
        }
        if (!identical(declined_msg_1L_chr, character(0))) 
            message(declined_msg_1L_chr)
    }
    if (return_1L_lgl) 
        return(object_xx)
}
#' Write words
#' @description write_words() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write words. The function is called for its side effects and does not return a value.
#' @param new_words_chr New words (a character vector)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @return No return value, called for side effects.
#' @rdname write_words
#' @export 
#' @importFrom piggyback pb_download_url
write_words <- function (new_words_chr, consent_1L_chr = "", consent_indcs_int = 1L, 
    gh_repo_1L_chr = "ready4-dev/ready4", gh_tag_1L_chr = "Documentation_0.0", 
    options_chr = c("Y", "N")) 
{
    dmt_urls_xx <- get_gracefully(NULL, fn = piggyback::pb_download_url, 
        args_ls = list(repo = gh_repo_1L_chr, tag = gh_tag_1L_chr, 
            .token = ""))
    if (!is.null(dmt_urls_xx)) {
        dmt_urls_chr <- dmt_urls_xx
        b <- readRDS(url(dmt_urls_chr[dmt_urls_chr %>% endsWith("treat_as_words_chr.RDS")]))
        b <- c(b, new_words_chr) %>% sort()
        write_env_objs_to_dv(env_objects_ls = list(treat_as_words_chr = b), 
            consent_1L_chr = consent_1L_chr, consent_indcs_int = consent_indcs_int, 
            descriptions_chr = NULL, ds_url_1L_chr = character(0), 
            options_chr = options_chr, piggyback_desc_1L_chr = "Supplementary Files", 
            piggyback_tag_1L_chr = gh_tag_1L_chr, piggyback_to_1L_chr = gh_repo_1L_chr, 
            prerelease_1L_lgl = TRUE)
    }
    else {
        message("Write action cancelled - unable to retrieve repository details.")
    }
}
#' Write ready4 software development local directories
#' @description write_ws() creates a standardised directory structure as a local development environment for modelling projects developed with the ready4 framework.
#' @param path_1L_chr Path (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @return No return value, called for side effects.
#' @rdname write_ws
#' @export 
#' @importFrom purrr map_chr
write_ws <- function (path_1L_chr, consent_1L_chr = "", consent_indcs_int = 1L, 
    options_chr = c("Y", "N")) 
{
    top_level_chr <- paste0(path_1L_chr, "/ready4/", c("Code", 
        "Data", "Documentation", "Insight"))
    code_top_lvl_chr <- c("Application", "Authoring", "Brochure", 
        "Description", "Modelling", "Prediction", "Foundation") %>% 
        purrr::map_chr(~paste0(top_level_chr[1], "/", .x))
    code_sub_dirs_chr <- c(paste0(code_top_lvl_chr[2], "/Workflows", 
        c("", "/R")), paste0(code_top_lvl_chr[3], "/HTML"), paste0(code_top_lvl_chr[4], 
        "/Datatypes", c("", "/R")), paste0(code_top_lvl_chr[5], 
        "/Templates", c("", "/R")), paste0(code_top_lvl_chr[6], 
        "/Example", c("", "/Toolkit_1", "/Toolkit_1/R")), paste0(code_top_lvl_chr[7], 
        "/Representations", c("", "/R")))
    data_top_lvl_chr <- c("Dataverse", "Project", "R_Format", 
        "Raw_Format") %>% purrr::map_chr(~paste0(top_level_chr[2], 
        "/", .x))
    data_sub_dirs_chr <- c("Agents", "Attributes", "Geometries", 
        "Metadata") %>% purrr::map_chr(~paste0(data_top_lvl_chr[4], 
        "/", .x))
    dcmntn_top_lvl_chr <- c("Code", "Data", "Images") %>% purrr::map_chr(~paste0(top_level_chr[3], 
        "/", .x))
    dcmntn_sub_dirs_chr <- c("Developer", "User") %>% purrr::map_chr(~paste0(dcmntn_top_lvl_chr[1], 
        "/", .x))
    insight_top_lvl_chr <- c("Analysis", "Science", "Team") %>% 
        purrr::map_chr(~paste0(top_level_chr[4], "/", .x))
    new_dirs_chr <- c(paste0(path_1L_chr, "/ready4"), top_level_chr, 
        code_top_lvl_chr, code_sub_dirs_chr, data_top_lvl_chr, 
        data_sub_dirs_chr, dcmntn_top_lvl_chr, dcmntn_sub_dirs_chr, 
        insight_top_lvl_chr)
    write_new_dirs(new_dirs_chr, consent_1L_chr = consent_1L_chr, 
        consent_indcs_int = consent_indcs_int, options_chr = options_chr)
}
