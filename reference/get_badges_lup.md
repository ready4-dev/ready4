# Get badges lookup table

get_badges_lup() is a Get function that extracts data from an object.
Specifically, this function implements an algorithm to get badges lookup
table. The function returns Ready4 badges (a lookup table).

## Usage

``` r
get_badges_lup(
  ends_with_1L_chr = "ready4_badges_lup.RDS",
  gh_repo_1L_chr = "ready4-dev/ready4",
  gh_tag_1L_chr = "Documentation_0.0"
)
```

## Arguments

- ends_with_1L_chr:

  Ends with (a character vector of length one), Default:
  'ready4_badges_lup.RDS'

- gh_repo_1L_chr:

  Github repository (a character vector of length one), Default:
  'ready4-dev/ready4'

- gh_tag_1L_chr:

  Github tag (a character vector of length one), Default:
  'Documentation_0.0'

## Value

Ready4 badges (a lookup table)
