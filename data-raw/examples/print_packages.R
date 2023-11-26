if (interactive()) {
  libraries_tb <- get_libraries_tb(gh_repo_1L_chr = "ready4-dev/ready4")
  # Print framework libraries (method 1)
  update_libraries_tb(libraries_tb,
                      url_stub_1L_chr = "https://ready4-dev.github.io/",
                      include_1L_chr = "framework") %>%
  print_packages()
  # Print framework libraries (method 2)
  print_packages(gh_repo_1L_chr = "ready4-dev/ready4",
                 include_1L_chr = "framework")
  # Print module libraries (method 1)
  update_libraries_tb(libraries_tb,
                      url_stub_1L_chr = "https://ready4-dev.github.io/",
                      include_1L_chr = "modules") %>%
  print_packages()
  # Print module libraries (method 2)
  print_packages(gh_repo_1L_chr = "ready4-dev/ready4",
                 include_1L_chr = "modules")
}
