# Write to publish dataverse dataset

write_to_publish_dv_ds() is a Write function that writes a file to a
specified local directory. Specifically, this function implements an
algorithm to write to publish dataverse dataset. The function is called
for its side effects and does not return a value.

## Usage

``` r
write_to_publish_dv_ds(
  dv_ds_1L_chr,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  minor_1L_lgl = FALSE,
  options_chr = c("Y", "N")
)
```

## Arguments

- dv_ds_1L_chr:

  Dataverse dataset (a character vector of length one)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- minor_1L_lgl:

  Minor (a logical vector of length one), Default: FALSE

- options_chr:

  Options (a character vector), Default: c("Y", "N")

## Value

No return value, called for side effects.
