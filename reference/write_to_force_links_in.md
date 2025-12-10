# Write to force links in

write_to_force_links_in() is a Write function that writes a file to a
specified local directory. Specifically, this function implements an
algorithm to write to force links in. The function is called for its
side effects and does not return a value.

## Usage

``` r
write_to_force_links_in(
  path_to_mkdn_1L_chr,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  options_chr = c("Y", "N"),
  shorten_doi_1L_lgl = TRUE
)
```

## Arguments

- path_to_mkdn_1L_chr:

  Path to markdown (a character vector of length one)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- options_chr:

  Options (a character vector), Default: c("Y", "N")

- shorten_doi_1L_lgl:

  Shorten digital object identifier (a logical vector of length one),
  Default: TRUE

## Value

No return value, called for side effects.
