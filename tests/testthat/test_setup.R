context("Set up")
file_fl <- file()
write(paste(c("Y"), collapse = "\n"), file_fl)
options("prompt_opts.con" = file_fl)
write_ws(
  getwd()
  )
close(file_fl) # close the file
options("prompt_opts.con" = stdin())
test_that("Set up directories are created",{
  expect_true(dir.exists(paste0(getwd(),"/ready4")))
  expect_true(dir.exists(paste0(getwd(),"/ready4/Code")))
  expect_true(dir.exists(paste0(getwd(),"/ready4/Data")))
  expect_true(dir.exists(paste0(getwd(),"/ready4/Documentation")))
  expect_true(dir.exists(paste0(getwd(),"/ready4/Insight")))
}
)

