# From : https://github.com/ThinkR-open/prepare-for-cran
# Prepare for CRAN ----

# Update dependencies in DESCRIPTION
# install.packages('attachment', repos = 'https://thinkr-open.r-universe.dev')
# attachment::att_amend_desc()
unlink("dev",T)
citation_chr <- readLines("inst/CITATION")
citation_chr[3] <- stringr::str_replace(citation_chr[3], "citEntry", "bibentry")
citation_chr[4] <- stringr::str_replace(citation_chr[4], "entry", "bibtype")
citation_chr[8] <- stringr::str_replace(citation_chr[8], "2021", "2023")
citation_chr[12] <- stringr::str_replace(citation_chr[12], "2021", "2023")
citation_chr  %>%
  writeLines(con = "inst/CITATION")
readLines("README.md") %>%
  stringr::str_replace_all("svg\\)]\\(https://codecov.io","svg\\)]\\(https://app.codecov.io") %>%
  writeLines(con = "README.md")
c(readLines("R/imp_fns.R"),
  " ",
  "#' NSE equals function",
  "#'",
  "#' Import of non standard evaluation equals function for use in dplyr calls.",
  "#'",
  "#' @importFrom rlang :=",
  "#' @name :=",
  "#' @rdname nseequals",
  "#' @export",
  "#' @keywords internal",
  "NULL",
  " ",
  "#' Dot Data function",
  "#'",
  "#' Import of .data function for use in dataset manipulation within functions.",
  "#'",
  "#' @importFrom rlang .data",
  "#' @name .data",
  "#' @rdname dotdata",
  "#' @export",
  "#' @keywords internal",
  "NULL"
) %>%
  writeLines("R/imp_fns.R")
# Run tests and examples
devtools::test()
devtools::run_examples()
# autotest::autotest_package(test = TRUE)

# Check package as CRAN
rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"))
# Add note that top level CITATION is intentional
# https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
# Check content
# install.packages('checkhelper', repos = 'https://thinkr-open.r-universe.dev')
checkhelper::find_missing_tags()
# _Check that you let the house clean after the check, examples and tests
all_files_remaining <- checkhelper::check_clean_userspace()
all_files_remaining

# Check spelling
# usethis::use_spell_check()
spelling::spell_check_package()

# Check URL are correct
# install.packages('urlchecker', repos = 'https://r-lib.r-universe.dev')
urlchecker::url_check()
urlchecker::url_update()

# check on other distributions
# _rhub
devtools::check_rhub()
rhub::check_on_windows(check_args = "--force-multiarch")
rhub::check_on_solaris()
# _win devel
devtools::check_win_devel()

# Check reverse dependencies
# remotes::install_github("r-lib/revdepcheck")
install.packages('revdepcheck', repos = 'https://r-lib.r-universe.dev')
usethis::use_git_ignore("revdep/")
usethis::use_build_ignore("revdep/")

devtools::revdep()
library(revdepcheck)
# In another session
id <- rstudioapi::terminalExecute("Rscript -e 'revdepcheck::revdep_check(num_workers = 4)'")
rstudioapi::terminalKill(id)
# See outputs
revdep_details(revdep = "pkg")
revdep_summary()                 # table of results by package
revdep_report() # in revdep/
# Clean up when on CRAN
revdep_reset()

# Update NEWS
# Bump version manually and add list of changes

# Add comments for CRAN
usethis::use_cran_comments(open = rlang::is_interactive())

# Upgrade version number
usethis::use_version(which = c("patch", "minor", "major", "dev")[1])

# Verify you're ready for release, and release
devtools::release()
