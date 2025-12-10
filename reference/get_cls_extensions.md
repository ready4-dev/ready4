# Get class extensions

get_cls_extensions() is a Get function that extracts data from an
object. Specifically, this function implements an algorithm to get class
extensions. The function returns Class extensions (a tibble).

## Usage

``` r
get_cls_extensions(
  pkg_extensions_tb,
  gh_repo_1L_chr = "ready4-dev/ready4",
  gh_tag_1L_chr = "Documentation_0.0",
  url_stub_1L_chr = "https://ready4-dev.github.io/",
  validate_1L_lgl = FALSE
)
```

## Arguments

- pkg_extensions_tb:

  Package extensions (a tibble)

- gh_repo_1L_chr:

  Github repository (a character vector of length one), Default:
  'ready4-dev/ready4'

- gh_tag_1L_chr:

  Github tag (a character vector of length one), Default:
  'Documentation_0.0'

- url_stub_1L_chr:

  Url stub (a character vector of length one), Default:
  'https://ready4-dev.github.io/'

- validate_1L_lgl:

  Validate (a logical vector of length one), Default: FALSE

## Value

Class extensions (a tibble)
