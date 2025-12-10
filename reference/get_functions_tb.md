# Get functions tibble

get_functions_tb() is a Get function that extracts data from an object.
Specifically, this function implements an algorithm to get functions
tibble. The function returns Functions (a tibble).

## Usage

``` r
get_functions_tb(
  gh_repo_1L_chr = "ready4-dev/ready4",
  gh_tag_1L_chr = "Documentation_0.0",
  return_1L_chr = "all"
)
```

## Arguments

- gh_repo_1L_chr:

  Github repository (a character vector of length one), Default:
  'ready4-dev/ready4'

- gh_tag_1L_chr:

  Github tag (a character vector of length one), Default:
  'Documentation_0.0'

- return_1L_chr:

  Return (a character vector of length one), Default: 'all'

## Value

Functions (a tibble)
