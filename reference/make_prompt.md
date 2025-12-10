# Make prompt

make_prompt() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make prompt. The
function returns Response (a character vector of length one).

## Usage

``` r
make_prompt(prompt_1L_chr, options_chr = NULL, force_from_opts_1L_chr = FALSE)
```

## Arguments

- prompt_1L_chr:

  Prompt (a character vector of length one)

- options_chr:

  Options (a character vector), Default: NULL

- force_from_opts_1L_chr:

  Force from opts (a character vector of length one), Default: FALSE

## Value

Response (a character vector of length one)
