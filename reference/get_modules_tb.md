# Get a table of ready4 model modules

get_modules_tb() ingests 'modules_tb.RDS' (a table of ready4 model
modules) from a specified GitHub repository release.

## Usage

``` r
get_modules_tb(
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

Modules (a tibble)

## Examples

``` r
get_modules_tb("ready4-dev/ready4")
#> # A tibble: 70 × 5
#>    Class     Description                          Library Examples old_class_lgl
#>    <list>    <chr>                                <chr>   <list>   <lgl>        
#>  1 <chr [1]> Meta data for processing ACT popula… "<a hr… <chr>    FALSE        
#>  2 <chr [1]> Meta data for constructing Headspac… "<a hr… <chr>    FALSE        
#>  3 <chr [1]> Lookup tables for Australian geomet… "<a hr… <chr>    FALSE        
#>  4 <chr [1]> Meta data for constructing OYH Spec… "<a hr… <chr>    FALSE        
#>  5 <chr [1]> Meta data for constructing custom A… "<a hr… <chr>    FALSE        
#>  6 <chr [1]> Meta data for processing Tasmanian … "<a hr… <chr>    FALSE        
#>  7 <chr [1]> Collection of input, standards defi… "<a hr… <chr>    FALSE        
#>  8 <chr [1]> Collection of input, standards defi… "<a hr… <chr>    FALSE        
#>  9 <chr [1]> Collection of input, standards defi… "<a hr… <chr>    FALSE        
#> 10 <chr [1]> Original (non-standardised) dataset… "<a hr… <chr>    FALSE        
#> # ℹ 60 more rows
```
