# Get badge urls

get_badge_urls() is a Get function that extracts data from an object.
Specifically, this function implements an algorithm to get badge urls.
The function returns Badge urls (a list).

## Usage

``` r
get_badge_urls(
  pkg_nm_1L_chr,
  project_badges_url_1L_chr = "https://img.shields.io/badge/ready4",
  url_stub_1L_chr = "https://ready4-dev.github.io/"
)
```

## Arguments

- pkg_nm_1L_chr:

  Package name (a character vector of length one)

- project_badges_url_1L_chr:

  Project badges url (a character vector of length one), Default:
  'https://img.shields.io/badge/ready4'

- url_stub_1L_chr:

  Url stub (a character vector of length one), Default:
  'https://ready4-dev.github.io/'

## Value

Badge urls (a list)
