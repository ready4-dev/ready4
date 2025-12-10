# Make libraries list

make_libraries_ls() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make libraries
list. The function returns Libraries (a list).

## Usage

``` r
make_libraries_ls(
  additions_tb = make_additions_tb(),
  libraries_tb = NULL,
  ns_var_nm_1L_chr = "pt_ns_chr"
)
```

## Arguments

- additions_tb:

  Additions (a tibble), Default: make_additions_tb()

- libraries_tb:

  Libraries (a tibble), Default: NULL

- ns_var_nm_1L_chr:

  Namespace variable name (a character vector of length one), Default:
  'pt_ns_chr'

## Value

Libraries (a list)
