add_lups <- function(template_lup,
                     new_lup,
                     key_var_nm_1L_chr,
                     priority_lup_for_dupls_1L_chr = "template"){
  if(names(template_lup)!=names(new_lup)){
    stop("Look up tables must have same column names")
  }
  if(!requireNamespace("Hmisc", quietly = TRUE)) {
    stop("Hmisc package is required - please install it and rerun the last command.")
  }
  if(priority_lup_for_dupls_1L_chr == "template"){
    new_lup <- new_lup %>%
      dplyr::filter(!(!!rlang::sym(key_var_nm_1L_chr) %in% (template_lup %>% dplyr::pull(!!rlang::sym(key_var_nm_1L_chr)))))
    labels_chr <- Hmisc::label(template_lup) %>% unname()
  }else{
    template_lup <- template_lup %>%
      dplyr::filter(!(!!rlang::sym(key_var_nm_1L_chr) %in% (new_lup %>% dplyr::pull(!!rlang::sym(key_var_nm_1L_chr)))))
    labels_chr <- Hmisc::label(new_lup) %>% unname()
  }
  if(!all(labels_chr %>% unique() =="")){
    template_lup <- template_lup %>% remove_lbls_from_df()
    new_lup <- new_lup %>% remove_lbls_from_df()
    Hmisc::label(template_lup) <-  as.list(labels_chr %>% unname())
    Hmisc::label(new_lup) <- as.list(labels_chr %>% unname())
  }
  combined_lups <- dplyr::bind_rows(template_lup,
                                    new_lup) %>%
    dplyr::arrange(!!rlang::sym(key_var_nm_1L_chr))
  combined_lups <- combined_lups[rowSums(is.na(combined_lups)) != ncol(combined_lups), ]
  return(combined_lups)
}
add_references <- function(ds_tb,
                           data_var_nm_1L_chr = "URL",
                           data_url_var_nm_1L_chr = "REF_URL",
                           reference_var_nm_1L_chr = "Reference"){
  examples_1L_int <- ds_tb %>%
    dplyr::pull(!!rlang::sym(data_var_nm_1L_chr)) %>%
    purrr::compact() %>%
    purrr::flatten_chr() %>%
    length()
  ds_tb <- ds_tb %>%
    dplyr::mutate(!!rlang::sym(reference_var_nm_1L_chr) := !!rlang::sym(data_var_nm_1L_chr) %>%
                    purrr::reduce(.init = list(ref_int = c(0,0),
                                               content_ls = NULL),
                                  ~{
                                    if(is.null(.y) | identical(.y, character(0)) | is.na(.y[1])){
                                      .x$content_ls <- append(.x$content_ls,list(NA_integer_))
                                    }else{
                                      .x$ref_int <- c(.x$ref_int[2] + 1,
                                                      .x$ref_int[2] + length(.y))
                                      .x$content_ls <- append(.x$content_ls,
                                                              list((1:examples_1L_int)[.x$ref_int]))
                                    }
                                    .x
                                  }) %>%
                    purrr::pluck(2)) %>%
    dplyr::mutate(!!rlang::sym(data_url_var_nm_1L_chr) := purrr::map2(!!rlang::sym(reference_var_nm_1L_chr),
                                                                          !!rlang::sym(data_var_nm_1L_chr),
                                                                          ~ {
                                                                            if(is.na(.x[1])){
                                                                              NA_character_
                                                                            }else{
                                                                              kableExtra::cell_spec(.x[1]:.x[2],
                                                                                                    "html",
                                                                                                    link = .y)
                                                                            }

                                                                          }
    ))
  return(ds_tb)
}
add_rows_from_fn_args <- function(tbl_r3,
                                  fn,
                                  fn_env_ls){
  if(!requireNamespace("Hmisc", quietly = TRUE)) {
    stop("Hmisc package is required - please install it and rerun the last command.")
  }
  fn_defaults_ls <- make_fn_defaults_ls(fn)
  tbl_args_ls <- fn_env_ls[names(tbl_r3)]
  bind_1L_lgl <- names(tbl_args_ls) %>%
    purrr::map_lgl(~{
      !identical(fn_defaults_ls[[.x]],tbl_args_ls[[.x]])
    }) %>% any()
  if(bind_1L_lgl){
    new_tb <- tibble::as_tibble(tbl_args_ls)
    labels_chr <- Hmisc::label(tbl_r3) %>% unname()
    if (!all(labels_chr %>% unique() == "")) {
      tbl_r3 <- tbl_r3 %>% remove_lbls_from_df()
      Hmisc::label(tbl_r3) <- as.list(labels_chr %>% unname())
      Hmisc::label(new_tb) <- as.list(labels_chr %>% unname())
    }
    tbl_r3 <- dplyr::bind_rows(tbl_r3,
                               new_tb)
  }
  return(tbl_r3)
}
add_scroll_box <- function(table_kbl,
                           scroll_height_1L_chr = character(0),
                           scroll_width_1L_chr = character(0),
                           ...){
  if(!identical(c(scroll_height_1L_chr,scroll_width_1L_chr), character(0))){
    table_kbl <- table_kbl %>%
      kableExtra::scroll_box(height = {
        if(identical(scroll_height_1L_chr, character(0))){
          NULL
        }else{
          scroll_height_1L_chr
        }
      },
      width = {
        if(identical(scroll_width_1L_chr, character(0))){
          NULL
        }else{
          scroll_width_1L_chr
        }
      })
  }
  return(table_kbl)
}
add_vignette_links <- function(pkg_extensions_tb,
                               ns_var_nm_1L_chr = "pt_ns_chr",
                               reference_var_nm_1L_chr = "Reference",
                               url_stub_1L_chr = "https://ready4-dev.github.io/",
                               vignette_var_nm_1L_chr = "Vignettes",
                               vignette_url_var_nm_1L_chr = "Vignettes_URLs"){
  pkg_extensions_tb <- pkg_extensions_tb %>%
    dplyr::mutate(!!rlang::sym(vignette_var_nm_1L_chr) := purrr::map(!!rlang::sym(ns_var_nm_1L_chr),
                                                                     ~ rvest::read_html(paste0(url_stub_1L_chr,.x,"/index.html")) %>%
                                                                       rvest::html_elements(".dropdown-item")  %>%
                                                                       rvest::html_attr("href") %>%
                                                                       stringr::str_remove("articles/") %>% #as.vector(browseVignettes(.x)[[.x]][,7]) %>%
                                                                       purrr::keep(~startsWith(.x,"V_")) %>% sort())) %>%
    dplyr::mutate(!!rlang::sym(vignette_var_nm_1L_chr) := purrr::map2(!!rlang::sym(ns_var_nm_1L_chr),
                                                                      !!rlang::sym(vignette_var_nm_1L_chr) ,
                                                                      ~if(!identical(.y, character(0))){
                                                                        paste0(url_stub_1L_chr,.x,"/articles/",.y)
                                                                      }else{
                                                                        NA_character_
                                                                      }))
  pkg_extensions_tb <- add_references(pkg_extensions_tb,
                                      data_var_nm_1L_chr = vignette_var_nm_1L_chr,
                                      data_url_var_nm_1L_chr = vignette_url_var_nm_1L_chr,
                                      reference_var_nm_1L_chr = reference_var_nm_1L_chr)

  return(pkg_extensions_tb)
}
