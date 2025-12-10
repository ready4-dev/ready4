# Bind tables from local files

bind_tables_from_loc_files() is a Bind function that binds two objects
together to create a composite object. Specifically, this function
implements an algorithm to bind tables from local files. The function
returns Table (an output object of multiple potential types).

## Usage

``` r
bind_tables_from_loc_files(
  paths_chr,
  force_numeric_1L_lgl = FALSE,
  force_tb_1L_lgl = FALSE,
  heading_rows_1L_int = 1L
)
```

## Arguments

- paths_chr:

  Paths (a character vector)

- force_numeric_1L_lgl:

  Force numeric (a logical vector of length one), Default: FALSE

- force_tb_1L_lgl:

  Force tibble (a logical vector of length one), Default: FALSE

- heading_rows_1L_int:

  Heading rows (an integer vector of length one), Default: 1

## Value

Table (an output object of multiple potential types)
