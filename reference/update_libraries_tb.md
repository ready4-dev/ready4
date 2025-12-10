# Update libraries tibble

update_libraries_tb() is an Update function that edits an object, while
preserving core object attributes. Specifically, this function
implements an algorithm to update libraries tibble. The function returns
Libraries (a tibble).

## Usage

``` r
update_libraries_tb(
  libraries_tb = make_libraries_tb(),
  additions_tb = make_additions_tb(),
  module_pkgs_chr = character(0),
  include_1L_chr = "all",
  ns_var_nm_1L_chr = "pt_ns_chr",
  reference_var_nm_1L_chr = "Reference",
  url_stub_1L_chr = "https://ready4-dev.github.io/",
  vignette_var_nm_1L_chr = "Vignettes",
  vignette_url_var_nm_1L_chr = "Vignettes_URLs",
  what_chr = "all"
)
```

## Arguments

- libraries_tb:

  Libraries (a tibble), Default: make_libraries_tb()

- additions_tb:

  Additions (a tibble), Default: make_additions_tb()

- module_pkgs_chr:

  Module packages (a character vector), Default: character(0)

- include_1L_chr:

  Include (a character vector of length one), Default: 'all'

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

- what_chr:

  What (a character vector), Default: 'all'

## Value

Libraries (a tibble)
