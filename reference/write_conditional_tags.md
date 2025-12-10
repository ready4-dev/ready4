# Write conditional tags

write_conditional_tags() is a Write function that writes a file to a
specified local directory. Specifically, this function implements an
algorithm to write conditional tags. The function is called for its side
effects and does not return a value.

## Usage

``` r
write_conditional_tags(
  pkgs_chr,
  path_to_pkg_root_1L_chr,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  options_chr = c("Y", "N"),
  where_1L_chr = character(0)
)
```

## Arguments

- pkgs_chr:

  Packages (a character vector)

- path_to_pkg_root_1L_chr:

  Path to package root (a character vector of length one)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- options_chr:

  Options (a character vector), Default: c("Y", "N")

- where_1L_chr:

  Where (a character vector of length one), Default: character(0)

## Value

No return value, called for side effects.

## See also

[`usethis::use_package()`](https://usethis.r-lib.org/reference/use_package.html)
