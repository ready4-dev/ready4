# Get excluded repositories

get_excluded_repos() is a Get function that extracts data from an
object. Specifically, this function implements an algorithm to get
excluded repositories. The function returns Exclude (a character
vector).

## Usage

``` r
get_excluded_repos(
  gh_repo_1L_chr = "ready4-dev/ready4",
  gh_tag_1L_chr = "Documentation_0.0"
)
```

## Arguments

- gh_repo_1L_chr:

  Github repository (a character vector of length one), Default:
  'ready4-dev/ready4'

- gh_tag_1L_chr:

  Github tag (a character vector of length one), Default:
  'Documentation_0.0'

## Value

Exclude (a character vector)
