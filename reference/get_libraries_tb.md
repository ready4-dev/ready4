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
#> # A tibble: 8 × 14
#>   pt_ns_chr  Type       Section Link  Library Vignettes Reference Vignettes_URLs
#>   <chr>      <chr>      <chr>   <chr> <chr>   <list>    <list>    <list>        
#> 1 ready4show Authoring… Framew… http… "<a hr… <chr [1]> <int [2]> <chr [1]>     
#> 2 ready4use  Authoring… Framew… http… "<a hr… <chr [4]> <int [2]> <chr [4]>     
#> 3 ready4     Foundation Framew… http… "<a hr… <chr [7]> <int [2]> <chr [7]>     
#> 4 scorz      Descripti… People  http… "<a hr… <chr [2]> <int [2]> <chr [2]>     
#> 5 youthvars  Descripti… People  http… "<a hr… <chr [2]> <int [2]> <chr [2]>     
#> 6 replica    Modelling… People  http… "<a hr… <chr [4]> <int [2]> <chr [4]>     
#> 7 mimic      Modelling… Progra… http… "<a hr… <chr [1]> <int [1]> <chr [1]>     
#> 8 serious    Modelling… Progra… http… "<a hr… <chr [1]> <int [1]> <chr [1]>     
#> # ℹ 6 more variables: Citation <chr>, manual_urls_ls <list>,
#> #   code_urls_ls <list>, Authors <chr>, Title <chr>, DOI_chr <chr>
```
