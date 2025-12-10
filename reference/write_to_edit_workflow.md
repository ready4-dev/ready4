# Write to edit workflow

write_to_edit_workflow() is a Write function that writes a file to a
specified local directory. Specifically, this function implements an
algorithm to write to edit workflow. The function is called for its side
effects and does not return a value.

## Usage

``` r
write_to_edit_workflow(
  fl_nm_1L_chr,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  dir_path_1L_chr = ".github/workflows",
  options_chr = c("Y", "N")
)
```

## Arguments

- fl_nm_1L_chr:

  File name (a character vector of length one)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- dir_path_1L_chr:

  Directory path (a character vector of length one), Default:
  '.github/workflows'

- options_chr:

  Options (a character vector), Default: c("Y", "N")

## Value

No return value, called for side effects.
