# Get manual urls

get_manual_urls() is a Get function that extracts data from an object.
Specifically, this function implements an algorithm to get manual urls.
The function returns Urls (a character vector).

## Usage

``` r
get_manual_urls(
  pkg_nm_1L_chr = "ready4",
  pkg_url_1L_chr = "https://ready4-dev.github.io/ready4/index.html"
)
```

## Arguments

- pkg_nm_1L_chr:

  Package name (a character vector of length one), Default: 'ready4'

- pkg_url_1L_chr:

  Package url (a character vector of length one), Default:
  'https://ready4-dev.github.io/ready4/index.html'

## Value

Urls (a character vector)
