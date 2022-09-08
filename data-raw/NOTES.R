library(ready4)
library(ready4use)
library(ready4fun)
X <- ready4use::Ready4useRepos(gh_repo_1L_chr = "ready4-dev/ready4",
                               gh_tag_1L_chr = "Documentation_0.0")
Y <- ingest(X)
abbreviations_lup <- procure(procureSlot(Y,
                                         "b_Ready4useIngest"),
                             "abbreviations_lup")
get_abbrs("numeric",abbreviations_lup) # from ready4fun
get_abbrs("num",abbreviations_lup,F) # from ready4fun
abbreviations_lup <- ready4fun::renew.ready4fun_abbreviations(abbreviations_lup,
                                                              short_name_chr = "num",
                                                              long_name_chr = "numeric",
                                                              plural_lgl = F)
# abbreviations_lup <-  abbreviations_lup %>%
#   dplyr::mutate(short_name_chr = dplyr::case_when(short_name_chr == "RI" ~ "ri",
#                                                   short_name_chr == "RIs" ~ "ris",
#                                                   T ~ short_name_chr)) %>%
#   dplyr::mutate(long_name_chr = dplyr::case_when(long_name_chr == "Rand Indexs" ~ "Rand Indices",
#                                                  T ~ long_name_chr))
Y <- renewSlot(Y,
               new_val_xx = Ready4useIngest(objects_ls = list(abbreviations_lup = abbreviations_lup)),
               slot_nm_1L_chr = "b_Ready4useIngest")
Y <- share(Y,
           type_1L_chr = "prefer_gh")


# x <- ready4::renew(x_xx$x_ready4fun_manifest,
#                                            are_words_chr = c("backend", "betareg",
#                                                              "boruta", "covariates",
#                                                              "deterministic", "multi",
#                                                              "shareable", "timepoint",
#                                                              "ungroup","yhat"),
#                                            type_1L_chr = "words")
# write_words("datasets")
fn_types_lup <- ready4fun::get_rds_from_pkg_dmt(fl_nm_1L_chr = "fn_types_lup",
                                                piggyback_to_1L_chr = "ready4-dev/ready4")
fn_types_lup <- fn_types_lup %>%
  #dplyr::filter(!fn_type_nm_chr %in% c("Add Class", "Analyse", "Report","get_read_fn")) %>%
  tibble::add_case(fn_type_nm_chr = c("authorSlot", "depictSlot", "exhibitSlot", "ingestSlot", "investigateSlot", "manufactureSlot", "prognosticateSlot", "reckonSlot"),
                   #fn_type_desc_chr = c("Procures data contained in the slot of a class instance"),
                   is_generic_lgl = T,
                   is_method_lgl = T) %>%
  dplyr::arrange(fn_type_nm_chr) %>%
  dplyr::mutate(fn_type_desc_chr = dplyr::case_when(fn_type_nm_chr %>% purrr::map_lgl(~ endsWith(.x,"Slot")) ~ paste0("Applies the ",fn_type_nm_chr %>% stringr::str_remove_all("Slot"), " method to a specified slot"),
                                                    T ~ fn_type_desc_chr)) %>%
  dplyr::mutate(fn_type_desc_chr = dplyr::case_when(fn_type_nm_chr == "procureSlot" ~ "Procures (gets) the value of a specified slot (default behaviour) or the value returned by applying the procure method to the slot",
                                                    fn_type_nm_chr == "renewSlot" ~ "Renews (sets) the value of a specified slot with either a specified new value or the value returned by applying the renew method to the slot (default behaviour)",
                                                    T ~ fn_type_desc_chr))
# generics_chr <- c("author","authorClasses", "authorData","authorFunctions", "authorReport",
#                   "characterize","characterizeSlot","depict","enhance","enhanceSlot","exhibit","ingest",
#                   "investigate","manufacture", "metamorphose","metamorphoseSlot", "procure",
#                   "prognosticate","ratify","ratifySlot","reckon","renew",
#                   "renewSlot","share","shareSlot")

# fn_types_lup <- fn_types_lup %>%
#   dplyr::filter(!fn_type_nm_chr %in% c("Add Class", "Analyse", "Report")) %>%
#   tibble::add_case(fn_type_nm_chr = c("AuthorReport", "CharacterizeSlot", "EnhanceSlot", "MetamorphoseSlot", "RatifySlot", "ShareSlot"),
#                    #fn_type_desc_chr = "Authors and saves a report.",
#                    is_generic_lgl = T,
#                    is_method_lgl = T) %>%
#   dplyr::mutate(is_generic_lgl = dplyr::case_when(fn_type_nm_chr %in% Hmisc::capitalize(generics_chr) ~ T,
#                                                   T ~ F)) %>%
#   dplyr::mutate(is_method_lgl = is_generic_lgl) %>%
#   dplyr::mutate(fn_type_nm_chr = dplyr::case_when(is_generic_lgl ~ purrr::map_chr(fn_type_nm_chr,
#                                                                                   ~  {
#                                                                                     substr(.x, 1, 1) <- tolower(substr(.x, 1, 1))
#                                                                                     .x}),
#                                                   T ~ fn_type_nm_chr)) %>%
#   dplyr::mutate(fn_type_desc_chr = dplyr::case_when(fn_type_nm_chr == "author" ~ "Authors and saves files to local or remote locations.",
#                                                     fn_type_nm_chr == "authorReport" ~ "Authors and saves a report.",
#                                                     fn_type_nm_chr == "characterize" ~ "Characterizes a class instance by generating descriptive information.",
#                                                     fn_type_nm_chr == "characterizeSlot" ~ "Characterizes a an object stored in a slot of a class instance by generating descriptive information.",
#                                                     fn_type_nm_chr == "enhance" ~ "Enhances an instance of a class with new data, transforming the original object into an instance of a child class.",
#                                                     fn_type_nm_chr == "enhanceSlot" ~ "Enhances an instance of a class by adding new data fields and values to an object stored in a slot of that instance.",
#                                                     fn_type_nm_chr == "ingest" ~ "Ingests data saved in external files into R objects stored in working memory.",
#                                                     fn_type_nm_chr == "investigate" ~ "Investigates data stored in an instance of a class by executing an algorithm to solve an inverse problem.",
#                                                     fn_type_nm_chr == "manufacture" ~ "Manufactures a novel R object using data contained in an instance of a class.",
#                                                     fn_type_nm_chr == "metamorphose" ~ "Metamorphoses an instance of a class into an instance of a different (non-child) class.",
#                                                     fn_type_nm_chr == "metamorphoseSlot" ~ "Metamorphoses data stored in a slot of a class instance into an object of a different (non-child) class.",
#                                                     fn_type_nm_chr == "procure" ~ "Procures data by executing a search and retrieval algorithm using data contained in an instance of a class.",
#                                                     fn_type_nm_chr == "prognosticate" ~ "Prognosticates by applying an algorithm to solve a forward problem through simulation or prediction.",
#                                                     fn_type_nm_chr == "ratify" ~ "Ratifies that an instance of a class conforms to specified criteria.",
#                                                     fn_type_nm_chr == "ratifySlot" ~ "Ratifies that data contained in a slot of a class instance conforms to specified criteria.",
#                                                     fn_type_nm_chr == "reckon" ~ "Reckons a value by performing a calculation using data contained in an instance of a class.",
#                                                     fn_type_nm_chr == "renew" ~ "Renew an instance of a class by updating it with new data.",
#                                                     fn_type_nm_chr == "renewSlot" ~ "Renew a slot of a class instance by updating it with new data.",
#                                                     fn_type_nm_chr == "share" ~ "Shares data contained in an instance of a class via an online repository.",
#                                                     fn_type_nm_chr == "shareSlot" ~ "Shares data contained in a slot of a class instance via an online repository.",
#                                                     T ~ fn_type_desc_chr )) %>%
#   dplyr::arrange(fn_type_nm_chr)
# fn_types_lup <- fn_types_lup %>%
#   dplyr::mutate(is_method_lgl = is_generic_lgl) %>%
#   dplyr::mutate(fn_type_nm_chr = dplyr::case_when(is_generic_lgl ~ purrr::map_chr(fn_type_nm_chr,
#                                                                                   ~  {
#                                                                                     substr(.x, 1, 1) <- tolower(substr(.x, 1, 1))
#                                                                                     .x}),
#                                                   T ~ fn_type_nm_chr))
# fn_types_lup <- fn_types_lup %>%
#   dplyr::mutate(fn_type_desc_chr = dplyr::case_when(fn_type_desc_chr %>% startsWith("Renew ") ~ fn_type_desc_chr %>% purrr::map_chr(~stringi::stri_replace_first_fixed(.x, "Renew ", "Renews ")),
#                                                     T ~ fn_type_desc_chr))
ready4::write_env_objs_to_dv(list(fn_types_lup = fn_types_lup),
                             descriptions_chr = c("Function types lookup table"),
                             ds_url_1L_chr = character(0),
                             piggyback_to_1L_chr = "ready4-dev/ready4",
                             publish_dv_1L_lgl = F)
# x <- ready4fun::write_new_fn_types(x,
#                                    fn_type_desc_chr = "Updates a specified slot of a class instance with new values.")
