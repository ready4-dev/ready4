# Print a table of ready4 model modules

print_modules() formats the output of either get_modules_tb() or
make_modules_tb() as HTML.

## Usage

``` r
print_modules(
  modules_tb,
  scroll_height_1L_chr = character(0),
  scroll_width_1L_chr = character(0),
  what_1L_chr = "All",
  ...
)
```

## Arguments

- modules_tb:

  Modules (a tibble)

- scroll_height_1L_chr:

  Scroll height (a character vector of length one), Default:
  character(0)

- scroll_width_1L_chr:

  Scroll width (a character vector of length one), Default: character(0)

- what_1L_chr:

  What (a character vector of length one), Default: 'All'

- ...:

  Additional arguments

## Value

Modules (a kable)

## Examples

``` r
modules_tb <- get_modules_tb("ready4-dev/ready4")
# Print sub-modules
print_modules(modules_tb, what_1L_chr = "S3")
#> <table class="table table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
#>  <thead>
#>   <tr>
#>    <th style="text-align:left;"> Class </th>
#>    <th style="text-align:left;"> Description </th>
#>    <th style="text-align:left;"> Examples </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#> 
#>   </tr>
#> </tbody>
#> </table>
# Print full-modules
print_modules(modules_tb, what_1L_chr = "S4")
#> <table class="table table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
#>  <thead>
#>   <tr>
#>    <th style="text-align:left;"> Class </th>
#>    <th style="text-align:left;"> Description </th>
#>    <th style="text-align:left;"> Examples </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/replica/reference/ReplicaAdder-class.html">ReplicaAdder</a> </td>
#>    <td style="text-align:left;"> An S4 class for assigning attributes to synthetic agents using contingency tables and marginal distributions </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/replica/articles/V_02.html">2</a>, <a href="https://ready4-dev.github.io/replica/articles/V_04.html">4</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/replica/reference/ReplicaGrouper-class.html">ReplicaGrouper</a> </td>
#>    <td style="text-align:left;"> Coordinates the generation of synthetic households from a synthetic population using one or more ReplicaStructure objects </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/replica/articles/V_03.html">3</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/replica/reference/ReplicaStructure-class.html">ReplicaStructure</a> </td>
#>    <td style="text-align:left;"> Represents a household structure used during synthetic household generation </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/replica/articles/V_03.html">3</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/scorz/reference/ScorzAqol6-class.html">ScorzAqol6</a> </td>
#>    <td style="text-align:left;"> A dataset and metadata to support implementation of an AQoL-6D scoring algorithm </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/scorz/reference/ScorzAqol6Adol-class.html">ScorzAqol6Adol</a> </td>
#>    <td style="text-align:left;"> A dataset and metadata to support implementation of a scoring algorithm for the adolescent version of AQoL-6D </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/scorz/articles/V_01.html">5</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/scorz/reference/ScorzAqol6Adult-class.html">ScorzAqol6Adult</a> </td>
#>    <td style="text-align:left;"> A dataset and metadata to support implementation of a scoring algorithm for the adult version of AQoL-6D </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/scorz/reference/ScorzEuroQol5-class.html">ScorzEuroQol5</a> </td>
#>    <td style="text-align:left;"> A dataset and metadata to support implementation of an EQ-5D scoring algorithm </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/scorz/articles/V_02.html">6</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/scorz/reference/ScorzProfile-class.html">ScorzProfile</a> </td>
#>    <td style="text-align:left;"> A dataset to be scored, its associated metadata and details of the scoring instrument </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#> </tbody>
#> </table>
```
