# Write ready4 software development local directories

write_ws() creates a standardised directory structure as a local
development environment for modelling projects developed with the ready4
framework.

## Usage

``` r
write_ws(
  path_1L_chr,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  options_chr = c("Y", "N")
)
```

## Arguments

- path_1L_chr:

  Path (a character vector of length one)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- options_chr:

  Options (a character vector), Default: c("Y", "N")

## Value

No return value, called for side effects.

## Examples

``` r
if (FALSE) { # interactive()
  write_ws(tempdir())
}
```
