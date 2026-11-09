if (interactive()) {
  # Likely to take more than one minute to execute.
    if(requireNamespace("tidyRSS", quietly = TRUE)) {
      make_code_releases_tbl("Framework",
                             gh_repo_1L_chr = "ready4-dev/ready4")
      make_code_releases_tbl("Module",
                             gh_repo_1L_chr = "ready4-dev/ready4")
      make_code_releases_tbl("Program",
                             gh_repo_1L_chr = "ready4-dev/ready4")
      make_code_releases_tbl("Subroutine",
                             gh_repo_1L_chr = "ready4-dev/ready4")
    }
  }
