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
#> # A tibble: 8 × 5
#>   Class     Description                           Library Examples old_class_lgl
#>   <list>    <chr>                                 <chr>   <list>   <lgl>        
#> 1 <chr [1]> An S4 class for assigning attributes… "<a hr… <chr>    FALSE        
#> 2 <chr [1]> Coordinates the generation of synthe… "<a hr… <chr>    FALSE        
#> 3 <chr [1]> Represents a household structure use… "<a hr… <chr>    FALSE        
#> 4 <chr [1]> A dataset and metadata to support im… "<a hr… <chr>    FALSE        
#> 5 <chr [1]> A dataset and metadata to support im… "<a hr… <chr>    FALSE        
#> 6 <chr [1]> A dataset and metadata to support im… "<a hr… <chr>    FALSE        
#> 7 <chr [1]> A dataset and metadata to support im… "<a hr… <chr>    FALSE        
#> 8 <chr [1]> A dataset to be scored, its associat… "<a hr… <chr>    FALSE        
```
