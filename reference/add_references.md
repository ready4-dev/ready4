# Add references

add_references() is an Add function that updates an object by adding new
values to new or empty fields. Specifically, this function implements an
algorithm to add references. The function returns Dataset (a tibble).

## Usage

``` r
add_references(
  ds_tb,
  data_var_nm_1L_chr = "URL",
  data_url_var_nm_1L_chr = "REF_URL",
  reference_var_nm_1L_chr = "Reference"
)
```

## Arguments

- ds_tb:

  Dataset (a tibble)

- data_var_nm_1L_chr:

  Data variable name (a character vector of length one), Default: 'URL'

- data_url_var_nm_1L_chr:

  Data url variable name (a character vector of length one), Default:
  'REF_URL'

- reference_var_nm_1L_chr:

  Reference variable name (a character vector of length one), Default:
  'Reference'

## Value

Dataset (a tibble)
