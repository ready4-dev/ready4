# Add vignette links

add_vignette_links() is an Add function that updates an object by adding
new values to new or empty fields. Specifically, this function
implements an algorithm to add vignette links. The function returns
Package extensions (a tibble).

## Usage

``` r
add_vignette_links(
  pkg_extensions_tb,
  ns_var_nm_1L_chr = "pt_ns_chr",
  reference_var_nm_1L_chr = "Reference",
  url_stub_1L_chr = "https://ready4-dev.github.io/",
  vignette_var_nm_1L_chr = "Vignettes",
  vignette_url_var_nm_1L_chr = "Vignettes_URLs"
)
```

## Arguments

- pkg_extensions_tb:

  Package extensions (a tibble)

- ns_var_nm_1L_chr:

  Namespace variable name (a character vector of length one), Default:
  'pt_ns_chr'

- reference_var_nm_1L_chr:

  Reference variable name (a character vector of length one), Default:
  'Reference'

- url_stub_1L_chr:

  Url stub (a character vector of length one), Default:
  'https://ready4-dev.github.io/'

- vignette_var_nm_1L_chr:

  Vignette variable name (a character vector of length one), Default:
  'Vignettes'

- vignette_url_var_nm_1L_chr:

  Vignette url variable name (a character vector of length one),
  Default: 'Vignettes_URLs'

## Value

Package extensions (a tibble)
