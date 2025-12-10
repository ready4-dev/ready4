# Write extra packages to actions

write_extra_pkgs_to_actions() is a Write function that writes a file to
a specified local directory. Specifically, this function implements an
algorithm to write extra packages to actions. The function is called for
its side effects and does not return a value.

## Usage

``` r
write_extra_pkgs_to_actions(
  path_to_dir_1L_chr,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  options_chr = c("Y", "N")
)
```

## Arguments

- path_to_dir_1L_chr:

  Path to directory (a character vector of length one)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- options_chr:

  Options (a character vector), Default: c("Y", "N")

## Value

No return value, called for side effects.
