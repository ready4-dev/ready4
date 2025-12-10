# Print dataverses

print_dvs() is a Print function that prints output to console.
Specifically, this function implements an algorithm to print dataverses.
The function is called for its side effects and does not return a value.

## Usage

``` r
print_dvs(
  dvs_tb,
  filter_cdns_ls = NULL,
  root_1L_chr = "https://dataverse.harvard.edu/dataverse/",
  scroll_height_1L_chr = character(0),
  scroll_width_1L_chr = character(0),
  toy_data_dv_1L_chr = "fakes",
  what_1L_chr = "all",
  ...
)
```

## Arguments

- dvs_tb:

  Dataverses (a tibble)

- filter_cdns_ls:

  Filter conditions (a list), Default: NULL

- root_1L_chr:

  Root (a character vector of length one), Default:
  'https://dataverse.harvard.edu/dataverse/'

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

Dataverses (a kable)
