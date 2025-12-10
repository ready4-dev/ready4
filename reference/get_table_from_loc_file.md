# Get table from local file

get_table_from_loc_file() is a Get function that extracts data from an
object. Specifically, this function implements an algorithm to get table
from local file. The function returns Table (an output object of
multiple potential types).

## Usage

``` r
get_table_from_loc_file(
  path_1L_chr,
  force_numeric_1L_lgl = FALSE,
  force_tb_1L_lgl = FALSE,
  heading_rows_1L_int = 1L
)
```

## Arguments

- path_1L_chr:

  Path (a character vector of length one)

- force_numeric_1L_lgl:

  Force numeric (a logical vector of length one), Default: FALSE

- force_tb_1L_lgl:

  Force tibble (a logical vector of length one), Default: FALSE

- heading_rows_1L_int:

  Heading rows (an integer vector of length one), Default: 1

## Value

Table (an output object of multiple potential types)

## See also

[`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)

[`readxl::read_excel()`](https://readxl.tidyverse.org/reference/read_excel.html)
and
[`readxl::read_xlsx()`](https://readxl.tidyverse.org/reference/read_excel.html)
