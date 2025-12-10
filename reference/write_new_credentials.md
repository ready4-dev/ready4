# Write new credentials

write_new_credentials() is a Write function that writes a file to a
specified local directory. Specifically, this function implements an
algorithm to write new credentials. The function is called for its side
effects and does not return a value.

## Usage

``` r
write_new_credentials(
  path_to_file_1L_chr,
  new_credentials_1L_chr,
  old_credentials_1L_chr,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  options_chr = c("Y", "N")
)
```

## Arguments

- path_to_file_1L_chr:

  Path to file (a character vector of length one)

- new_credentials_1L_chr:

  New credentials (a character vector of length one)

- old_credentials_1L_chr:

  Old credentials (a character vector of length one)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- options_chr:

  Options (a character vector), Default: c("Y", "N")

## Value

No return value, called for side effects.
