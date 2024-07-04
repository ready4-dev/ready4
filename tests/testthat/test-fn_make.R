test_that("Valid tibble of release history of ready4 framework libraries is generated",{
  expect_no_error(fw_tb <- make_code_releases_tbl("Framework", gh_repo_1L_chr = "ready4-dev/ready4", as_kbl_1L_lgl = F))
  skip_on_cran()
  #expect_true(!is.null(fw_tb))
  if(!is.null(fw_tb)){
    expect_true(tibble::is_tibble(fw_tb))
    expect_true(all(names(fw_tb) == c("Package", "Release", "Date", "Description", "URL")))
    expect_true(identical(fw_tb$Package %>% unique() %>% sort(), c("ready4", "ready4class", "ready4fun", "ready4show", "ready4use"))) # update once ready4pack released
    expect_true(nrow(fw_tb) > 5)
  }
}
)
test_that("Kable of release history of subroutines in a GitHub repository is generated",{
  expect_no_error(subroutines_kbl <- make_code_releases_tbl("Subroutine", gh_repo_1L_chr = "ready4-dev/ready4"))
  skip_on_cran()
  #expect_true(!is.null(subroutines_kbl))
  if(!is.null(subroutines_kbl)){
  expect_true(class(subroutines_kbl)[1] == "kableExtra")
  }
}
)
test_that("Valid tibble of release history of data collection is generated",{
  expect_no_error(dataset_tb <- make_ds_releases_tbl("10.7910/DVN/RIQTKK", as_kbl_1L_lgl = FALSE))
  skip_on_cran()
  #expect_true(!is.null(dataset_tb))
  if(!is.null(dataset_tb)){
  expect_true(tibble::is_tibble(dataset_tb))
  expect_true(all(names(dataset_tb) == c("Date", "Dataset", "DOI",  "Version", "Number of files")))
  expect_true("ready4 Framework Abbreviations and Definitions" %in% dataset_tb$Dataset)
  }
}
)
test_that("Kable of release history of data collection is generated",{
  expect_no_error(dataset_kbl <- make_ds_releases_tbl("10.7910/DVN/RIQTKK"))
  skip_on_cran()
  #expect_true(!is.null(dataset_kbl))
  if(!is.null(dataset_kbl)){
  expect_true(class(dataset_kbl)[1] == "kableExtra")
    }
}
)
test_that("Valid transformations of tibble of modelling project data collections",{
  expect_no_error(dvs_tb <- get_datasets_tb("ready4-dev/ready4"))
  expect_no_error(dvs_2_tb <- make_datasets_tb("ready4", dvs_tb = dvs_tb))
  skip_on_cran()
  # expect_true(!is.null(dvs_tb))
  # expect_true(!is.null(dvs_2_tb))
  if(!is.null(dvs_tb) && !is.null(dvs_2_tb)){
    expect_true(tibble::is_tibble(dvs_2_tb))
    expect_true(identical(dvs_tb, dvs_2_tb))
    expect_true(all(names(dvs_2_tb) == c("Dataverse", "Name", "Description", "Creator", "Contents", "Datasets_Meta")))
    expect_true("ready4fw" %in% dvs_2_tb$Dataverse)
    expect_no_error(real_tb <- make_datasets_tb("ready4", dvs_tb = dvs_2_tb, what_1L_chr = "real"))
    expect_true(setdiff(dvs_2_tb$Dataverse, real_tb$Dataverse)=="fakes")
    expect_no_error(fakes_tb <- make_datasets_tb("ready4", dvs_tb = dvs_2_tb, what_1L_chr = "fakes"))
    expect_true(fakes_tb$Dataverse=="fakes")
    expect_no_error(datasets_tb <- make_datasets_tb("ready4", dvs_tb = dvs_tb, type_1L_chr = "datasets"))
    expect_true(tibble::is_tibble(datasets_tb))
    expect_true(all(names(datasets_tb) == c("Title", "Description", "Dataverse", "DOI")))
    expect_true("ready4 Framework Abbreviations and Definitions" %in% datasets_tb$Title)
    expect_no_error(ds_fakes_tb <- make_datasets_tb("ready4", dvs_tb = dvs_tb, type_1L_chr = "datasets", what_1L_chr = "fakes"))
    expect_true(length(setdiff(datasets_tb$Title, ds_fakes_tb$Title))>1)
    expect_no_error(ds_real_tb <- make_datasets_tb("ready4", dvs_tb = dvs_tb, type_1L_chr = "datasets", what_1L_chr = "real"))
    expect_true(length(setdiff(datasets_tb$Title,ds_real_tb$Title))>1)
  }

}
)
