# Make a tabular summary of release history of ready4 model data collections

make_ds_releases_tbl() scrapes metadata from Dataverse datasets for
which a valid Digital Object Identifier (DOI) has been supplied to
create a table summarising the entire release history of these datasets.

## Usage

``` r
make_ds_releases_tbl(
  ds_dois_chr,
  format_1L_chr = "%d-%b-%Y",
  key_1L_chr = NULL,
  server_1L_chr = "dataverse.harvard.edu",
  as_kbl_1L_lgl = TRUE,
  ...
)
```

## Arguments

- ds_dois_chr:

  Dataset digital object identifiers (a character vector)

- format_1L_chr:

  Format (a character vector of length one), Default: '%d-%b-%Y'

- key_1L_chr:

  Key (a character vector of length one), Default: NULL

- server_1L_chr:

  Server (a character vector of length one), Default:
  'dataverse.harvard.edu'

- as_kbl_1L_lgl:

  As kable (a logical vector of length one), Default: TRUE

- ...:

  Additional arguments

## Value

Dataset releases (an output object of multiple potential types)

## Examples

``` r
make_ds_releases_tbl("10.7910/DVN/RIQTKK", as_kbl_1L_lgl = FALSE)
#> # A tibble: 4 × 5
#>   Date        Dataset                            DOI   Version `Number of files`
#>   <chr>       <chr>                              <chr> <chr>               <int>
#> 1 23-Dec-2022 ready4 Framework Abbreviations an… http… 3.0                     1
#> 2 25-May-2022 ready4 Framework Abbreviations an… http… 2.1                     1
#> 3 14-Jan-2022 ready4 Framework Abbreviations an… http… 2.0                     1
#> 4 14-Jan-2022 ready4 Framework Abbreviations an… http… 1.0                     1
```
