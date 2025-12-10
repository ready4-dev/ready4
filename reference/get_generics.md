# Get generics

get_generics() is a Get function that extracts data from an object.
Specifically, this function implements an algorithm to get generics. The
function returns Generics (a character vector).

## Usage

``` r
get_generics(
  pkg_nm_1L_chr = "ready4",
  return_1L_chr = "all",
  exclude_mthds_for_chr = NA_character_,
  framework_only_1L_lgl = TRUE
)
```

## Arguments

- pkg_nm_1L_chr:

  Package name (a character vector of length one), Default: 'ready4'

- return_1L_chr:

  Return (a character vector of length one), Default: 'all'

- exclude_mthds_for_chr:

  Exclude methods for (a character vector), Default: 'NA'

- framework_only_1L_lgl:

  Framework only (a logical vector of length one), Default: TRUE

## Value

Generics (a character vector)
