# Write examples

write_examples() is a Write function that writes a file to a specified
local directory. Specifically, this function implements an algorithm to
write examples. The function is called for its side effects and does not
return a value.

## Usage

``` r
write_examples(
  path_1L_chr,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  options_chr = c("Y", "N"),
  type_1L_chr = "fn"
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

- type_1L_chr:

  Type (a character vector of length one), Default: 'fn'

## Value

No return value, called for side effects.

## See also

[`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
