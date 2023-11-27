if (interactive()) {
  # Likely to take more than one minute to execute.
  if(requireNamespace("zen4R", quietly = TRUE)) {
    make_programs_tbl("Program",
                      gh_repo_1L_chr = "ready4-dev/ready4")
    make_programs_tbl("Subroutine",
                      gh_repo_1L_chr = "ready4-dev/ready4")
  }
  }
