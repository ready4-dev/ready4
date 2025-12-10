# Write with consent

write_with_consent() is a Write function that writes a file to a
specified local directory. Specifically, this function implements an
algorithm to write with consent. The function returns Object (an output
object of multiple potential types).

## Usage

``` r
write_with_consent(
  consented_fn,
  prompt_1L_chr,
  consent_1L_chr = "",
  consented_args_ls = NULL,
  consent_indcs_int = 1L,
  consented_msg_1L_chr = character(0),
  declined_args_ls = NULL,
  declined_fn = NULL,
  declined_msg_1L_chr = "No files have been written.",
  force_from_opts_1L_chr = TRUE,
  options_chr = c("Y", "N"),
  return_1L_lgl = FALSE
)
```

## Arguments

- consented_fn:

  Consented (a function)

- prompt_1L_chr:

  Prompt (a character vector of length one)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consented_args_ls:

  Consented arguments (a list), Default: NULL

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- consented_msg_1L_chr:

  Consented message (a character vector of length one), Default:
  character(0)

- declined_args_ls:

  Declined arguments (a list), Default: NULL

- declined_fn:

  Declined (a function), Default: NULL

- declined_msg_1L_chr:

  Declined message (a character vector of length one), Default: 'No
  files have been written.'

- force_from_opts_1L_chr:

  Force from opts (a character vector of length one), Default: TRUE

- options_chr:

  Options (a character vector), Default: c("Y", "N")

- return_1L_lgl:

  Return (a logical vector of length one), Default: FALSE

## Value

Object (an output object of multiple potential types)
