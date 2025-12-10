# Get a table of methods associated with ready4 model modules

get_methods_tb() ingests 'methods_tb.RDS' (a table of methods associated
with ready4 model modules) from a specified GitHub repository release.

## Usage

``` r
get_methods_tb(
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

Methods (a tibble)

## Examples

``` r
get_methods_tb("ready4-dev/ready4")
#> # A tibble: 34 × 3
#>    Method           Purpose                                             Examples
#>    <chr>            <chr>                                               <list>  
#>  1 author           Author and save files                               <chr>   
#>  2 authorClasses    Author and document classes                         <chr>   
#>  3 authorData       Author and document datasets                        <chr>   
#>  4 authorFunctions  Author and document functions                       <chr>   
#>  5 authorReport     Author and save a report                            <chr>   
#>  6 authorSlot       Apply the author method to a model module slot      <chr>   
#>  7 characterize     Characterize model module data by generating (tabu… <chr>   
#>  8 characterizeSlot Apply the characterize method to a model module sl… <chr>   
#>  9 depict           Depict (plot) features of model module data         <chr>   
#> 10 depictSlot       Apply the depict method to a model module slot      <chr>   
#> # ℹ 24 more rows
```
