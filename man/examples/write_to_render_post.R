if (interactive()) {
  # Note, In addition to rmarkdown, the non CRAN package "hugodown" is also required.
  if(requireNamespace("rmarkdown", quietly = TRUE)) {
  # Example 1 - RMD files
  #
  # Copy template RMD files
  write_to_copy_rmds(dir_path_1L_chr = tempdir(),
                     fl_nm_1L_chr = "RMDs",
                     rmds_dir_1L_chr = system.file("MD_RMDs",
                                                   package = "ready4"))
  # Typically you would now edit these templates before proceeding.
  # Render post from RMD files.
  write_to_render_post("RMDs", path_to_main_dir_1L_chr = tempdir())
  #
  # Example 2 - Rmarkdown file
  #
  # Copy template Rmarkdown file
  write_to_copy_rmds(dir_path_1L_chr = tempdir(),
                     fl_nm_1L_chr = "Rmarkdown",
                     rmds_dir_1L_chr = system.file("MD_Rmarkdown",
                                                   package = "ready4"))
  # Typically you would now edit these templates before proceeding.
  # Render post from RMD files.
  write_to_render_post("Rmarkdown",
                       path_to_main_dir_1L_chr = tempdir(),
                       is_rmd_1L_lgl = F)
  }
  }
