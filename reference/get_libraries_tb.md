# Get a table of ready4 libraries

get_libraries_tb() retrieves a tabular summary of ready4 libraries that
have been developed within a specified GitHub organisation.

## Usage

``` r
get_libraries_tb(
  gh_repo_1L_chr = "ready4-dev/ready4",
  gh_tag_1L_chr = "Documentation_0.0"
)
```

## Arguments

- gh_repo_1L_chr:

  Github repository (a character vector of length one), Default:
  'ready4-dev/ready4'

- gh_tag_1L_chr:

  Github tag (a character vector of length one), Default:
  'Documentation_0.0'

## Value

Libraries (a tibble)

## Examples

``` r
get_libraries_tb("ready4-dev/ready4")
#> # A tibble: 17 × 14
#>    pt_ns_chr   Type     Section Link  Library Vignettes Reference Vignettes_URLs
#>    <chr>       <chr>    <chr>   <chr> <chr>   <list>    <list>    <list>        
#>  1 ready4use   Authori… Framew… http… "<a hr… <chr [3]> <int [2]> <chr [3]>     
#>  2 ready4show  Authori… Framew… http… "<a hr… <chr [1]> <int [2]> <chr [1]>     
#>  3 ready4fun   Authori… Framew… http… "<a hr… <chr [1]> <int [2]> <chr [1]>     
#>  4 ready4class Authori… Framew… http… "<a hr… <chr [1]> <int [2]> <chr [1]>     
#>  5 ready4pack  Authori… Framew… http… "<a hr… <chr [1]> <int [2]> <chr [1]>     
#>  6 ready4      Foundat… Framew… http… "<a hr… <chr [4]> <int [2]> <chr [4]>     
#>  7 youthvars   Descrip… People  http… "<a hr… <chr [2]> <int [2]> <chr [2]>     
#>  8 scorz       Descrip… People  http… "<a hr… <chr [2]> <int [2]> <chr [2]>     
#>  9 mychoice    Modelli… People  http… "<a hr… <chr [1]> <int [1]> <chr [1]>     
#> 10 TTU         Modelli… People  http… "<a hr… <chr [1]> <int [2]> <chr [1]>     
#> 11 heterodox   Modelli… People  http… "<a hr… <chr [1]> <int [1]> <chr [1]>     
#> 12 specific    Modelli… People  http… "<a hr… <chr [1]> <int [2]> <chr [1]>     
#> 13 youthu      Predict… People  http… "<a hr… <chr [1]> <int [2]> <chr [1]>     
#> 14 aus         Modelli… Places  http… "<a hr… <chr [1]> <int [1]> <chr [1]>     
#> 15 vicinity    Modelli… Places  http… "<a hr… <chr [1]> <int [1]> <chr [1]>     
#> 16 bimp        Modelli… Progra… http… "<a hr… <chr [1]> <int [1]> <chr [1]>     
#> 17 costly      Modelli… Progra… http… "<a hr… <chr [2]> <int [2]> <chr [2]>     
#> # ℹ 6 more variables: Citation <chr>, manual_urls_ls <list>,
#> #   code_urls_ls <list>, Authors <list>, Title <chr>, DOI_chr <chr>
```
