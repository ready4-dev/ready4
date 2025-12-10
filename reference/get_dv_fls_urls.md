# Get dataverse files urls

get_dv_fls_urls() is a Get function that extracts data from an object.
Specifically, this function implements an algorithm to get dataverse
files urls. The function returns Urls (a character vector).

## Usage

``` r
get_dv_fls_urls(
  file_nms_chr,
  dv_ds_nm_1L_chr,
  dv_url_pfx_1L_chr = character(0),
  server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
  key_1L_chr = NULL
)
```

## Arguments

- file_nms_chr:

  File names (a character vector)

- dv_ds_nm_1L_chr:

  Dataverse dataset name (a character vector of length one)

- dv_url_pfx_1L_chr:

  Dataverse url prefix (a character vector of length one), Default:
  character(0)

- server_1L_chr:

  Server (a character vector of length one), Default:
  Sys.getenv("DATAVERSE_SERVER")

- key_1L_chr:

  Key (a character vector of length one), Default: NULL

## Value

Urls (a character vector)
