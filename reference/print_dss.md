# Print datasets

print_dss() is a Print function that prints output to console.
Specifically, this function implements an algorithm to print datasets.
The function is called for its side effects and does not return a value.

## Usage

``` r
print_dss(
  datasets_tb,
  filter_cdns_ls = NULL,
  scroll_height_1L_chr = character(0),
  scroll_width_1L_chr = character(0),
  toy_data_dv_1L_chr = "fakes",
  what_1L_chr = "all",
  ...
)
```

## Arguments

- datasets_tb:

  Datasets (a tibble)

- filter_cdns_ls:

  Filter conditions (a list), Default: NULL

- scroll_height_1L_chr:

  Scroll height (a character vector of length one), Default:
  character(0)

- scroll_width_1L_chr:

  Scroll width (a character vector of length one), Default: character(0)

- toy_data_dv_1L_chr:

  Toy data dataverse (a character vector of length one), Default:
  'fakes'

- what_1L_chr:

  What (a character vector of length one), Default: 'all'

- ...:

  Additional arguments

## Value

Datasets (a kable)
