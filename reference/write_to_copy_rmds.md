# Write a local copy of RMD or Rmarkdown files

write_to_copy_rmds() is used to copy template RMD or Rmarkdown files to
specified sub-directories of a model documentation website. These
template copies can then be manually edited before being rendered with
write_to_render_post().

## Usage

``` r
write_to_copy_rmds(
  dir_path_1L_chr,
  fl_nm_1L_chr,
  consent_1L_chr = "",
  rmds_dir_1L_chr = "R/RMD Templates",
  consent_indcs_int = 1L,
  options_chr = c("Y", "N"),
  return_1L_lgl = FALSE
)
```

## Arguments

- dir_path_1L_chr:

  Directory path (a character vector of length one)

- fl_nm_1L_chr:

  File name (a character vector of length one)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- rmds_dir_1L_chr:

  R Markdowns directory (a character vector of length one), Default:
  'R/RMD Templates'

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- options_chr:

  Options (a character vector), Default: c("Y", "N")

- return_1L_lgl:

  Return (a logical vector of length one), Default: FALSE

## Value

No return value, called for side effects.

## Examples

``` r
if (FALSE) { # interactive()
  write_to_copy_rmds(dir_path_1L_chr = tempdir(),
                     fl_nm_1L_chr = "RMDs",
                     rmds_dir_1L_chr = system.file("MD_RMDs",
                                                   package = "ready4"))
}
```
