# Get method titles

get_mthd_titles() is a Get function that extracts data from an object.
Specifically, this function implements an algorithm to get method
titles. The function returns Method titles (a character vector).

## Usage

``` r
get_mthd_titles(
  mthd_nms_chr,
  pkg_nm_1L_chr = "ready4",
  path_1L_chr = character(0)
)
```

## Arguments

- mthd_nms_chr:

  Method names (a character vector)

- pkg_nm_1L_chr:

  Package name (a character vector of length one), Default: 'ready4'

- path_1L_chr:

  Path (a character vector of length one), Default: character(0)

## Value

Method titles (a character vector)
