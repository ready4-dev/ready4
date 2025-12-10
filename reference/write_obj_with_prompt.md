# Write object with prompt

write_obj_with_prompt() is a Write function that writes a file to a
specified local directory. Specifically, this function implements an
algorithm to write object with prompt. The function is called for its
side effects and does not return a value.

## Usage

``` r
write_obj_with_prompt(
  object_xx,
  obj_nm_1L_chr,
  outp_dir_1L_chr,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  options_chr = c("Y", "N"),
  return_1L_lgl = FALSE
)
```

## Arguments

- object_xx:

  Object (an output object of multiple potential types)

- obj_nm_1L_chr:

  Object name (a character vector of length one)

- outp_dir_1L_chr:

  Output directory (a character vector of length one)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- options_chr:

  Options (a character vector), Default: c("Y", "N")

- return_1L_lgl:

  Return (a logical vector of length one), Default: FALSE

## Value

No return value, called for side effects.
