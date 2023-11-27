if (interactive()) {
  # Method 1
  libraries_tb <- get_libraries_tb(gh_repo_1L_chr = "ready4-dev/ready4")
  ## Print framework libraries
  update_libraries_tb(libraries_tb,
                      url_stub_1L_chr = "https://ready4-dev.github.io/",
                      include_1L_chr = "framework") %>%
    print_packages()
  ## Print module libraries
  update_libraries_tb(libraries_tb,
                      url_stub_1L_chr = "https://ready4-dev.github.io/",
                      include_1L_chr = "modules") %>%
    print_packages()
  # Method 2
  ## Print framework libraries
  print_packages(gh_repo_1L_chr = "ready4-dev/ready4",
                 include_1L_chr = "framework")
  ## Print module libraries
  print_packages(gh_repo_1L_chr = "ready4-dev/ready4",
                 include_1L_chr = "modules")
}
