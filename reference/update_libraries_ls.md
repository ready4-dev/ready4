# Update libraries list

update_libraries_ls() is an Update function that edits an object, while
preserving core object attributes. Specifically, this function
implements an algorithm to update libraries list. The function returns
Libraries (a list).

## Usage

``` r
update_libraries_ls(
  libraries_ls = NULL,
  additions_tb = make_additions_tb(),
  keep_chr = "all"
)
```

## Arguments

- libraries_ls:

  Libraries (a list), Default: NULL

- additions_tb:

  Additions (a tibble), Default: make_additions_tb()

- keep_chr:

  Keep (a character vector), Default: 'all'

## Value

Libraries (a list)
