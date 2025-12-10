# Get rds from dataverse

get_rds_from_dv() is a Get function that extracts data from an object.
Specifically, this function implements an algorithm to get rds from
dataverse. The function returns R object (an output object of multiple
potential types).

## Usage

``` r
get_rds_from_dv(
  file_nm_1L_chr,
  dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/RIQTKK",
  dv_url_pfx_1L_chr = character(0),
  key_1L_chr = NULL,
  not_chr_1L_lgl = NA,
  server_1L_chr = Sys.getenv("DATAVERSE_SERVER")
)
```

## Arguments

- file_nm_1L_chr:

  File name (a character vector of length one)

- dv_ds_nm_1L_chr:

  Dataverse dataset name (a character vector of length one), Default:
  'https://doi.org/10.7910/DVN/RIQTKK'

- dv_url_pfx_1L_chr:

  Dataverse url prefix (a character vector of length one), Default:
  character(0)

- key_1L_chr:

  Key (a character vector of length one), Default: NULL

- not_chr_1L_lgl:

  Not character vector (a logical vector of length one), Default: NA

- server_1L_chr:

  Server (a character vector of length one), Default:
  Sys.getenv("DATAVERSE_SERVER")

## Value

R object (an output object of multiple potential types)
