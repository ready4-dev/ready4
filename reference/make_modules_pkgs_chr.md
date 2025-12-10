# Make modules packages character vector

make_modules_pkgs_chr() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make modules
packages character vector. The function returns Modules packages (a
character vector).

## Usage

``` r
make_modules_pkgs_chr(
  gh_repo_1L_chr = "ready4-dev/ready4",
  gh_tag_1L_chr = "Documentation_0.0",
  sort_1L_lgl = FALSE,
  what_chr = "all"
)
```

## Arguments

- gh_repo_1L_chr:

  Github repository (a character vector of length one), Default:
  'ready4-dev/ready4'

- gh_tag_1L_chr:

  Github tag (a character vector of length one), Default:
  'Documentation_0.0'

- sort_1L_lgl:

  Sort (a logical vector of length one), Default: FALSE

- what_chr:

  What (a character vector), Default: 'all'

## Value

Modules packages (a character vector)
