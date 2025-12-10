# Write new files

write_new_files() is a Write function that writes a file to a specified
local directory. Specifically, this function implements an algorithm to
write new files. The function returns Object (an output object of
multiple potential types).

## Usage

``` r
write_new_files(
  paths_chr,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  custom_write_ls = NULL,
  fl_nm_1L_chr = NULL,
  options_chr = c("Y", "N"),
  source_paths_ls = NULL,
  text_ls = NULL,
  return_1L_lgl = FALSE
)
```

## Arguments

- paths_chr:

  Paths (a character vector)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- custom_write_ls:

  Custom write (a list), Default: NULL

- fl_nm_1L_chr:

  File name (a character vector of length one), Default: NULL

- options_chr:

  Options (a character vector), Default: c("Y", "N")

- source_paths_ls:

  Source paths (a list), Default: NULL

- text_ls:

  Text (a list), Default: NULL

- return_1L_lgl:

  Return (a logical vector of length one), Default: FALSE

## Value

Object (an output object of multiple potential types)
