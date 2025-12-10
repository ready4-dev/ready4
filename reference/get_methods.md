# Get the methods associated with a ready4 model module

get_methods() retrieves the ready4 methods that are available for a
specified ready4 model module.

## Usage

``` r
get_methods(pkg_nm_1L_chr = "ready4", cls_nm_1L_chr = "Ready4Module")
```

## Arguments

- pkg_nm_1L_chr:

  Package name (a character vector of length one), Default: 'ready4'

- cls_nm_1L_chr:

  Class name (a character vector of length one), Default: 'Ready4Module'

## Value

Methods (a character vector)

## Examples

``` r
get_methods()
#>  [1] "authorSlot"        "characterizeSlot"  "depictSlot"       
#>  [4] "enhanceSlot"       "exhibitSlot"       "ingestSlot"       
#>  [7] "investigateSlot"   "manufactureSlot"   "metamorphoseSlot" 
#> [10] "procureSlot"       "prognosticateSlot" "ratifySlot"       
#> [13] "reckonSlot"        "renewSlot"         "shareSlot"        
```
