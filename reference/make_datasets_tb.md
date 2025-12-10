# Make a tabular summary of ready4 model data collections

make_datasts_tb() scrapes metadata from a specified Dataverse collection
to create a summary table of its contents. The contents table can detail
either subsidiary data collections or individual datasets from those
subsidiary data collections.

## Usage

``` r
make_datasets_tb(
  dv_nm_1L_chr = "ready4",
  dvs_tb = NULL,
  filter_cdns_ls = NULL,
  key_1L_chr = NULL,
  server_1L_chr = "dataverse.harvard.edu",
  toy_data_dv_1L_chr = "fakes",
  type_1L_chr = c("collections", "datasets"),
  what_1L_chr = "all"
)
```

## Arguments

- dv_nm_1L_chr:

  Dataverse name (a character vector of length one), Default: 'ready4'

- dvs_tb:

  Dataverses (a tibble), Default: NULL

- filter_cdns_ls:

  Filter conditions (a list), Default: NULL

- key_1L_chr:

  Key (a character vector of length one), Default: NULL

- server_1L_chr:

  Server (a character vector of length one), Default:
  'dataverse.harvard.edu'

- toy_data_dv_1L_chr:

  Toy data dataverse (a character vector of length one), Default:
  'fakes'

- type_1L_chr:

  Type (a character vector of length one), Default: c("collections",
  "datasets")

- what_1L_chr:

  What (a character vector of length one), Default: 'all'

## Value

Datasets (a tibble)

## Examples

``` r
if (FALSE) { # interactive()
  # Likely to take more than one minute to execute.
  make_datasets_tb("ready4")
  dvs_tb <- get_datasets_tb("ready4-dev/ready4")
  make_datasets_tb("ready4", dvs_tb = dvs_tb)
  make_datasets_tb("ready4", dvs_tb = dvs_tb, what_1L_chr = "real")
  make_datasets_tb("ready4", dvs_tb = dvs_tb, what_1L_chr = "fakes")
  make_datasets_tb("ready4", dvs_tb = dvs_tb, type_1L_chr = "datasets")
  make_datasets_tb("ready4", dvs_tb = dvs_tb, type_1L_chr = "datasets", what_1L_chr = "real")
  make_datasets_tb("ready4", dvs_tb = dvs_tb, type_1L_chr = "datasets", what_1L_chr = "fakes")
}
```
