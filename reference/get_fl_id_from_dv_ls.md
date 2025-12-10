# Get file identity from dataverse list

get_fl_id_from_dv_ls() is a Get function that extracts data from an
object. Specifically, this function implements an algorithm to get file
identity from dataverse list. The function returns Identity (a character
vector of length one).

## Usage

``` r
get_fl_id_from_dv_ls(ds_ls, fl_nm_1L_chr, nms_chr = NA_character_)
```

## Arguments

- ds_ls:

  Dataset (a list)

- fl_nm_1L_chr:

  File name (a character vector of length one)

- nms_chr:

  Names (a character vector), Default: 'NA'

## Value

Identity (a character vector of length one)
