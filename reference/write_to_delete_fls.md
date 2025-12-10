# Write to delete files

write_to_delete_fls() is a Write function that writes a file to a
specified local directory. Specifically, this function implements an
algorithm to write to delete files. The function returns Object (an
output object of multiple potential types).

## Usage

``` r
write_to_delete_fls(
  file_paths_chr,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  options_chr = c("Y", "N"),
  return_1L_lgl = FALSE
)
```

## Arguments

- file_paths_chr:

  File paths (a character vector)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- options_chr:

  Options (a character vector), Default: c("Y", "N")

- return_1L_lgl:

  Return (a logical vector of length one), Default: FALSE

## Value

Object (an output object of multiple potential types)
