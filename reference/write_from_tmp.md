# Write from temporary

write_from_tmp() is a Write function that writes a file to a specified
local directory. Specifically, this function implements an algorithm to
write from temporary. The function is called for its side effects and
does not return a value.

## Usage

``` r
write_from_tmp(
  tmp_paths_chr,
  dest_paths_chr,
  args_ls_ls = list(NULL),
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  edit_fn_ls = list(NULL),
  options_chr = c("Y", "N")
)
```

## Arguments

- tmp_paths_chr:

  Temporary paths (a character vector)

- dest_paths_chr:

  Destination paths (a character vector)

- args_ls_ls:

  Arguments (a list of lists), Default: list(NULL)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- edit_fn_ls:

  Edit (a list of functions), Default: list(NULL)

- options_chr:

  Options (a character vector), Default: c("Y", "N")

## Value

No return value, called for side effects.
