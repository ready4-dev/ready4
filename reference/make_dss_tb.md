# Make datasets tibble

make_dss_tb() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make datasets
tibble. The function returns Datasets (a tibble).

## Usage

``` r
make_dss_tb(
  dvs_tb,
  filter_cdns_ls = list(),
  toy_data_dv_1L_chr = "fakes",
  what_1L_chr = "all"
)
```

## Arguments

- dvs_tb:

  Dataverses (a tibble)

- filter_cdns_ls:

  Filter conditions (a list), Default: list()

- toy_data_dv_1L_chr:

  Toy data dataverse (a character vector of length one), Default:
  'fakes'

- what_1L_chr:

  What (a character vector of length one), Default: 'all'

## Value

Datasets (a tibble)
