# Make framework packages character vector

make_framework_pkgs_chr() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
framework packages character vector. The function returns Framework
packages (a character vector).

## Usage

``` r
make_framework_pkgs_chr(
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

Framework packages (a character vector)
