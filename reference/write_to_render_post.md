# Write ready4 model documentation website page from an RMD or Rmarkdown file

write_to_render_post() is designed for help overcome practical
challenges of rendering RMD or Rmarkdown files to Markdown output in a
modelling project's Hugo Docsy documentation website. You must have
'hugodown' installed for this function to work.

## Usage

``` r
write_to_render_post(
  included_dirs_chr,
  path_to_main_dir_1L_chr,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  is_rmd_1L_lgl = TRUE,
  options_chr = c("Y", "N")
)
```

## Arguments

- included_dirs_chr:

  Included directories (a character vector)

- path_to_main_dir_1L_chr:

  Path to main directory (a character vector of length one)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- is_rmd_1L_lgl:

  Is Markdown (a logical vector of length one), Default: TRUE

- options_chr:

  Options (a character vector), Default: c("Y", "N")

## Value

No return value, called for side effects.

## See also

[`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)

## Examples

``` r
if (FALSE) { # interactive()
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
```
