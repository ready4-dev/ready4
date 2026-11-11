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
#>    Method           Purpose                                           Examples 
#>    <chr>            <chr>                                             <list>   
#>  1 author           Create and save reusable artefacts                <chr [1]>
#>  2 authorClasses    Create and document classes                       <chr [1]>
#>  3 authorData       Create and document datasets                      <chr [1]>
#>  4 authorFunctions  Create and document functions                     <chr [1]>
#>  5 authorReport     Create reports                                    <chr [1]>
#>  6 authorSlot       Apply the author method to a specified slot       <chr [1]>
#>  7 characterize     Generate descriptive summaries                    <chr [1]>
#>  8 characterizeSlot Apply the characterize method to a specified slot <chr [1]>
#>  9 depict           Visualise object features                         <chr [6]>
#> 10 depictSlot       Apply the depict method to a specified slot       <chr [1]>
#> # ℹ 24 more rows
```
