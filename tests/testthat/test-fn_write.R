
test_that("Local workspace directories are created",{
  expect_no_error(write_ws(tempdir(), consent_1L_chr = "Y"))
  expect_true(dir.exists(paste0(tempdir(),"/ready4")))
  expect_true(dir.exists(paste0(tempdir(),"/ready4/Code")))
  expect_true(dir.exists(paste0(tempdir(),"/ready4/Data")))
  expect_true(dir.exists(paste0(tempdir(),"/ready4/Documentation")))
  expect_true(dir.exists(paste0(tempdir(),"/ready4/Insight")))
}
)
test_that("RMDs are copied",{
  expect_no_error(write_to_copy_rmds(dir_path_1L_chr = tempdir(),
                                     fl_nm_1L_chr = "RMDs",
                                     rmds_dir_1L_chr = pkgload:::shim_system.file("MD_RMDs", package = "ready4"),
                                     consent_1L_chr = "Y"))
  expect_true(file.exists(paste0(tempdir(),"/RMDs/index.Rmd")))
  expect_true(file.exists(paste0(tempdir(),"/RMDs/index_Body.Rmd")))
}
)
test_that("Rmarkdown files are copied",{
  expect_no_error(write_to_copy_rmds(dir_path_1L_chr = tempdir(),
                                     fl_nm_1L_chr = "Rmarkdown",
                                     rmds_dir_1L_chr = pkgload:::shim_system.file("MD_Rmarkdown", package = "ready4"),
                                     consent_1L_chr = "Y"))
  expect_true(file.exists(paste0(tempdir(),"/Rmarkdown/index.en.Rmarkdown")))
}
)
# Don't include because hugodown not included inpkg dependencies
# test_that("Documentation website post is rendered from RMDs",{
#   expect_no_error(write_to_render_post("RMDs", path_to_main_dir_1L_chr = tempdir(), consent_1L_chr = "Y"))
#   expect_true(file.exists(paste0(tempdir(),"/RMDs/index.md")))
# }
# )
# test_that("Documentation website post is rendered from Rmarkdown",{
#   expect_no_error(write_to_render_post("Rmarkdown", path_to_main_dir_1L_chr = tempdir(), consent_1L_chr = "Y", is_rmd_1L_lgl = F))
#   expect_true(file.exists(paste0(tempdir(),"/Rmarkdown/index.en.md")))
# }
# )
test_that("Specified individual sub-directory is deleted",{
  expect_no_error(write_to_delete_dirs(paste0(tempdir(),"/ready4/Code"), consent_1L_chr = "Y"))
  expect_no_error(write_to_delete_dirs(paste0(tempdir(),"/RMDs"), consent_1L_chr = "Y"))
  expect_no_error(write_to_delete_dirs(paste0(tempdir(),"/Rmarkdown"), consent_1L_chr = "Y"))
  expect_true(dir.exists(paste0(tempdir(),"/ready4")))
  expect_false(dir.exists(paste0(tempdir(),"/ready4/Code")))
  expect_false(dir.exists(paste0(tempdir(),"/RMDs")))
  expect_false(dir.exists(paste0(tempdir(),"/Rmarkdow")))
}
)
test_that("Multiple specified sub-directories are deleted",{
  expect_no_error(write_to_delete_dirs(paste0(tempdir(),c("/ready4/Documentation","/ready4/Insight")), consent_1L_chr = "Y"))
  expect_true(dir.exists(paste0(tempdir(),"/ready4/Data")))
  expect_false(dir.exists(paste0(tempdir(),"/ready4/Documentation")))
  expect_false(dir.exists(paste0(tempdir(),"/ready4/Insight")))
}
)
test_that("Directory deleted when containing sub-directory ",{
  expect_no_error(write_to_delete_dirs(paste0(tempdir(),c("/ready4")), consent_1L_chr = "Y"))
  expect_false(dir.exists(paste0(tempdir(),"/ready4")))
}
)
