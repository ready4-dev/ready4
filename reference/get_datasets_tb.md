# Get datasets tibble

get_datasets_tb() is a Get function that extracts data from an object.
Specifically, this function implements an algorithm to get datasets
tibble. The function returns Datasets (a tibble).

## Usage

``` r
get_datasets_tb(
  gh_repo_1L_chr = "ready4-dev/ready4",
  gh_tag_1L_chr = "Documentation_0.0",
  rds_fl_name_1L_chr = "datasets_tb"
)
```

## Arguments

- gh_repo_1L_chr:

  Github repository (a character vector of length one), Default:
  'ready4-dev/ready4'

- gh_tag_1L_chr:

  Github tag (a character vector of length one), Default:
  'Documentation_0.0'

- rds_fl_name_1L_chr:

  Rds file name (a character vector of length one), Default:
  'datasets_tb'

## Value

Datasets (a tibble)

## Examples

``` r
get_datasets_tb("ready4-dev/ready4")
#> # A tibble: 7 × 6
#>   Dataverse         Name              Description Creator Contents Datasets_Meta
#>   <chr>             <chr>             <chr>       <chr>   <list>   <list>       
#> 1 fakes             Fake Data For In… Fake data … Orygen  <chr>    <list [5]>   
#> 2 firstbounce       First Bounce      A ready4 f… Orygen  <chr>    <NULL>       
#> 3 ready4fw          ready4 Framework  A collecti… Orygen  <chr>    <list [1]>   
#> 4 readyforwhatsnext readyforwhatsnext Data colle… Orygen  <chr>    <list [2]>   
#> 5 springtides       Springtides       A ready4 f… Orygen  <chr>    <list [1]>   
#> 6 springtolife      Spring To Life    A ready4 f… Orygen  <chr>    <list [1]>   
#> 7 TTU               Transfer to Util… A collecti… Orygen  <chr>    <list [1]>   
```
