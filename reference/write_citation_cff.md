# Write citation cff

write_citation_cff() is a Write function that writes a file to a
specified local directory. Specifically, this function implements an
algorithm to write citation cff. The function is called for its side
effects and does not return a value.

## Usage

``` r
write_citation_cff(
  pkg_desc_ls,
  citation_chr,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  options_chr = c("Y", "N"),
  publisher_1L_chr = "Zenodo"
)
```

## Arguments

- pkg_desc_ls:

  Package description (a list)

- citation_chr:

  Citation (a character vector)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- options_chr:

  Options (a character vector), Default: c("Y", "N")

- publisher_1L_chr:

  Publisher (a character vector of length one), Default: 'Zenodo'

## Value

No return value, called for side effects.
