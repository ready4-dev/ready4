test_that("Tibble of modelling project data collections is retrieved from GitHub repository",{
  expect_no_error(dvs_tb <- get_datasets_tb("ready4-dev/ready4"))
  expect_true(tibble::is_tibble(dvs_tb))
  expect_true(all(names(dvs_tb) == c("Dataverse", "Name", "Description", "Creator", "Contents", "Datasets_Meta")))
  expect_true("ready4fw" %in% dvs_tb$Dataverse)
}
)
test_that("Tibble of modelling project libraries is retrieved from GitHub repository",{
  expect_no_error(libraries_tb <- get_libraries_tb())
  expect_true(tibble::is_tibble(libraries_tb))
  expect_true(all(names(libraries_tb) == c("pt_ns_chr", "Type", "Section", "Link", "Library", "Vignettes" ,
                                     "Reference", "Vignettes_URLs", "Citation", "manual_urls_ls", "code_urls_ls",
                                     "Authors" , "Title", "DOI_chr")))
}
)
test_that("Item retrieved correctly from lookup table",{
  expect_no_error(type_1L_chr <- get_from_lup_obj(get_libraries_tb(), match_value_xx = "ready4", match_var_nm_1L_chr = "pt_ns_chr", target_var_nm_1L_chr = "Section"))
  expect_true(type_1L_chr == "Framework")
  expect_no_error(name_1L_chr <- get_from_lup_obj(get_datasets_tb(), match_value_xx = "TTU", match_var_nm_1L_chr = "Dataverse", target_var_nm_1L_chr = "Name"))
  expect_true(name_1L_chr == "Transfer to Utility")
}
)

# "get_from_lup_obj","get_libraries_tb",

