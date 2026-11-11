# Print a table of methods associated with ready4 model modules

print_methods() formats the output of either get_methods_tb() or
make_methods_tb() as HTML.

## Usage

``` r
print_methods(
  methods_tb = NULL,
  exclude_mthds_for_chr = NA_character_,
  gh_repo_1L_chr = "ready4-dev/ready4",
  gh_tag_1L_chr = "Documentation_0.0",
  methods_chr = NULL,
  module_pkgs_chr = character(0),
  ns_var_nm_1L_chr = "pt_ns_chr",
  path_1L_chr = character(0),
  packages_tb = NULL,
  return_1L_chr = "all",
  scroll_height_1L_chr = character(0),
  scroll_width_1L_chr = character(0),
  ...
)
```

## Arguments

- methods_tb:

  Methods (a tibble), Default: NULL

- exclude_mthds_for_chr:

  Exclude methods for (a character vector), Default: 'NA'

- gh_repo_1L_chr:

  Github repository (a character vector of length one), Default:
  'ready4-dev/ready4'

- gh_tag_1L_chr:

  Github tag (a character vector of length one), Default:
  'Documentation_0.0'

- methods_chr:

  Methods (a character vector), Default: NULL

- module_pkgs_chr:

  Module packages (a character vector), Default: character(0)

- ns_var_nm_1L_chr:

  Namespace variable name (a character vector of length one), Default:
  'pt_ns_chr'

- path_1L_chr:

  Path (a character vector of length one), Default: character(0)

- packages_tb:

  Packages (a tibble), Default: NULL

- return_1L_chr:

  Return (a character vector of length one), Default: 'all'

- scroll_height_1L_chr:

  Scroll height (a character vector of length one), Default:
  character(0)

- scroll_width_1L_chr:

  Scroll width (a character vector of length one), Default: character(0)

- ...:

  Additional arguments

## Value

Methods (a kable)

## Examples

``` r
methods_tb <- get_methods_tb("ready4-dev/ready4")
print_methods(methods_tb)
#> <table class="table table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
#>  <thead>
#>   <tr>
#>    <th style="text-align:left;"> Method </th>
#>    <th style="text-align:left;"> Purpose </th>
#>    <th style="text-align:left;"> Examples </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/author-methods.html"> author </a>
#> </td>
#>    <td style="text-align:left;"> Create and save reusable artefacts </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/authorClasses-methods.html"> authorClasses </a>
#> </td>
#>    <td style="text-align:left;"> Create and document classes </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/authorData-methods.html"> authorData </a>
#> </td>
#>    <td style="text-align:left;"> Create and document datasets </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html">1</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/authorFunctions-methods.html"> authorFunctions </a>
#> </td>
#>    <td style="text-align:left;"> Create and document functions </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/authorReport-methods.html"> authorReport </a>
#> </td>
#>    <td style="text-align:left;"> Create reports </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html">1</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/authorSlot-methods.html"> authorSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the author method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/characterize-methods.html"> characterize </a>
#> </td>
#>    <td style="text-align:left;"> Generate descriptive summaries </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/characterizeSlot-methods.html"> characterizeSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the characterize method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/depict-methods.html"> depict </a>
#> </td>
#>    <td style="text-align:left;"> Visualise object features </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4use/articles/V_04.html">5</a> , <a href="https://ready4-dev.github.io/scorz/articles/V_01.html">13</a>    , <a href="https://ready4-dev.github.io/scorz/articles/V_02.html">14</a>    , <a href="https://ready4-dev.github.io/youthvars/articles/V_02.html">16</a>, <a href="https://ready4-dev.github.io/replica/articles/V_02.html">18</a>  , <a href="https://ready4-dev.github.io/replica/articles/V_04.html">20</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/depictSlot-methods.html"> depictSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the depict method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/enhance-methods.html"> enhance </a>
#> </td>
#>    <td style="text-align:left;"> Extend an object's structure </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/replica/articles/V_02.html">18</a>, <a href="https://ready4-dev.github.io/replica/articles/V_03.html">19</a>, <a href="https://ready4-dev.github.io/replica/articles/V_04.html">20</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/enhanceSlot-methods.html"> enhanceSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the enhance method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/exhibit-methods.html"> exhibit </a>
#> </td>
#>    <td style="text-align:left;"> Display object contents </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4use/articles/V_02.html">3</a> , <a href="https://ready4-dev.github.io/scorz/articles/V_01.html">13</a>    , <a href="https://ready4-dev.github.io/scorz/articles/V_02.html">14</a>    , <a href="https://ready4-dev.github.io/youthvars/articles/V_02.html">16</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/exhibitSlot-methods.html"> exhibitSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the exhibit method to a specified slot </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html">1</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/ingest-methods.html"> ingest </a>
#> </td>
#>    <td style="text-align:left;"> Import external data </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4use/articles/V_01.html">2</a> , <a href="https://ready4-dev.github.io/ready4use/articles/V_02.html">3</a> , <a href="https://ready4-dev.github.io/ready4use/articles/V_03.html">4</a> , <a href="https://ready4-dev.github.io/ready4use/articles/V_04.html">5</a> , <a href="https://ready4-dev.github.io/scorz/articles/V_01.html">13</a>    , <a href="https://ready4-dev.github.io/scorz/articles/V_02.html">14</a>    , <a href="https://ready4-dev.github.io/youthvars/articles/V_02.html">16</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/ingestSlot-methods.html"> ingestSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the ingest method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/investigate-methods.html"> investigate </a>
#> </td>
#>    <td style="text-align:left;"> Investigate inverse problems </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/investigateSlot-methods.html"> investigateSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the investigate method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/manufacture-methods.html"> manufacture </a>
#> </td>
#>    <td style="text-align:left;"> Create a derived object </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/replica/articles/V_03.html">19</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/manufactureSlot-methods.html"> manufactureSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the manufacture method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/metamorphose-methods.html"> metamorphose </a>
#> </td>
#>    <td style="text-align:left;"> Transform a Ready4 object into another Ready4 object </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/metamorphoseSlot-methods.html"> metamorphoseSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the metamorphose method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/procure-methods.html"> procure </a>
#> </td>
#>    <td style="text-align:left;"> Retrieve information from an object </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/replica/articles/V_02.html">18</a>, <a href="https://ready4-dev.github.io/replica/articles/V_03.html">19</a>, <a href="https://ready4-dev.github.io/replica/articles/V_04.html">20</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/procureSlot-methods.html"> procureSlot </a>
#> </td>
#>    <td style="text-align:left;"> Retrieve the value stored in a specified slot </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html">1</a>, <a href="https://ready4-dev.github.io/ready4use/articles/V_04.html">5</a> , <a href="https://ready4-dev.github.io/scorz/articles/V_01.html">13</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/prognosticate-methods.html"> prognosticate </a>
#> </td>
#>    <td style="text-align:left;"> Generate predictions </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/prognosticateSlot-methods.html"> prognosticateSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the prognosticate method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/ratify-methods.html"> ratify </a>
#> </td>
#>    <td style="text-align:left;"> Validate and reconcile object contents </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/youthvars/articles/V_02.html">16</a>, <a href="https://ready4-dev.github.io/replica/articles/V_02.html">18</a>  , <a href="https://ready4-dev.github.io/replica/articles/V_04.html">20</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/ratifySlot-methods.html"> ratifySlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the ratify method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/reckon-methods.html"> reckon </a>
#> </td>
#>    <td style="text-align:left;"> Compute derived quantities </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/reckonSlot-methods.html"> reckonSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the reckon method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/renew-methods.html"> renew </a>
#> </td>
#>    <td style="text-align:left;"> Update object contents </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4use/articles/V_01.html">2</a> , <a href="https://ready4-dev.github.io/ready4use/articles/V_02.html">3</a> , <a href="https://ready4-dev.github.io/ready4use/articles/V_04.html">5</a> , <a href="https://ready4-dev.github.io/scorz/articles/V_01.html">13</a>    , <a href="https://ready4-dev.github.io/scorz/articles/V_02.html">14</a>    , <a href="https://ready4-dev.github.io/youthvars/articles/V_02.html">16</a>, <a href="https://ready4-dev.github.io/replica/articles/V_03.html">19</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/renewSlot-methods.html"> renewSlot </a>
#> </td>
#>    <td style="text-align:left;"> Update the value stored in a specified slot </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html">1</a>, <a href="https://ready4-dev.github.io/ready4use/articles/V_04.html">5</a> , <a href="https://ready4-dev.github.io/scorz/articles/V_01.html">13</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/share-methods.html"> share </a>
#> </td>
#>    <td style="text-align:left;"> Share data or outputs </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4use/articles/V_01.html">2</a> , <a href="https://ready4-dev.github.io/scorz/articles/V_01.html">13</a>    , <a href="https://ready4-dev.github.io/scorz/articles/V_02.html">14</a>    , <a href="https://ready4-dev.github.io/youthvars/articles/V_02.html">16</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/shareSlot-methods.html"> shareSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the share method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#> </tbody>
#> </table>
print_methods(methods_tb, return_1L_chr = "core")
#> <table class="table table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
#>  <thead>
#>   <tr>
#>    <th style="text-align:left;"> Method </th>
#>    <th style="text-align:left;"> Purpose </th>
#>    <th style="text-align:left;"> Examples </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/author-methods.html"> author </a>
#> </td>
#>    <td style="text-align:left;"> Create and save reusable artefacts </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/characterize-methods.html"> characterize </a>
#> </td>
#>    <td style="text-align:left;"> Generate descriptive summaries </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/depict-methods.html"> depict </a>
#> </td>
#>    <td style="text-align:left;"> Visualise object features </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4use/articles/V_04.html">5</a> , <a href="https://ready4-dev.github.io/scorz/articles/V_01.html">13</a>    , <a href="https://ready4-dev.github.io/scorz/articles/V_02.html">14</a>    , <a href="https://ready4-dev.github.io/youthvars/articles/V_02.html">16</a>, <a href="https://ready4-dev.github.io/replica/articles/V_02.html">18</a>  , <a href="https://ready4-dev.github.io/replica/articles/V_04.html">20</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/enhance-methods.html"> enhance </a>
#> </td>
#>    <td style="text-align:left;"> Extend an object's structure </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/replica/articles/V_02.html">18</a>, <a href="https://ready4-dev.github.io/replica/articles/V_03.html">19</a>, <a href="https://ready4-dev.github.io/replica/articles/V_04.html">20</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/exhibit-methods.html"> exhibit </a>
#> </td>
#>    <td style="text-align:left;"> Display object contents </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4use/articles/V_02.html">3</a> , <a href="https://ready4-dev.github.io/scorz/articles/V_01.html">13</a>    , <a href="https://ready4-dev.github.io/scorz/articles/V_02.html">14</a>    , <a href="https://ready4-dev.github.io/youthvars/articles/V_02.html">16</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/ingest-methods.html"> ingest </a>
#> </td>
#>    <td style="text-align:left;"> Import external data </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4use/articles/V_01.html">2</a> , <a href="https://ready4-dev.github.io/ready4use/articles/V_02.html">3</a> , <a href="https://ready4-dev.github.io/ready4use/articles/V_03.html">4</a> , <a href="https://ready4-dev.github.io/ready4use/articles/V_04.html">5</a> , <a href="https://ready4-dev.github.io/scorz/articles/V_01.html">13</a>    , <a href="https://ready4-dev.github.io/scorz/articles/V_02.html">14</a>    , <a href="https://ready4-dev.github.io/youthvars/articles/V_02.html">16</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/investigate-methods.html"> investigate </a>
#> </td>
#>    <td style="text-align:left;"> Investigate inverse problems </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/manufacture-methods.html"> manufacture </a>
#> </td>
#>    <td style="text-align:left;"> Create a derived object </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/replica/articles/V_03.html">19</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/metamorphose-methods.html"> metamorphose </a>
#> </td>
#>    <td style="text-align:left;"> Transform a Ready4 object into another Ready4 object </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/procure-methods.html"> procure </a>
#> </td>
#>    <td style="text-align:left;"> Retrieve information from an object </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/replica/articles/V_02.html">18</a>, <a href="https://ready4-dev.github.io/replica/articles/V_03.html">19</a>, <a href="https://ready4-dev.github.io/replica/articles/V_04.html">20</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/prognosticate-methods.html"> prognosticate </a>
#> </td>
#>    <td style="text-align:left;"> Generate predictions </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/ratify-methods.html"> ratify </a>
#> </td>
#>    <td style="text-align:left;"> Validate and reconcile object contents </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/youthvars/articles/V_02.html">16</a>, <a href="https://ready4-dev.github.io/replica/articles/V_02.html">18</a>  , <a href="https://ready4-dev.github.io/replica/articles/V_04.html">20</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/reckon-methods.html"> reckon </a>
#> </td>
#>    <td style="text-align:left;"> Compute derived quantities </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/renew-methods.html"> renew </a>
#> </td>
#>    <td style="text-align:left;"> Update object contents </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4use/articles/V_01.html">2</a> , <a href="https://ready4-dev.github.io/ready4use/articles/V_02.html">3</a> , <a href="https://ready4-dev.github.io/ready4use/articles/V_04.html">5</a> , <a href="https://ready4-dev.github.io/scorz/articles/V_01.html">13</a>    , <a href="https://ready4-dev.github.io/scorz/articles/V_02.html">14</a>    , <a href="https://ready4-dev.github.io/youthvars/articles/V_02.html">16</a>, <a href="https://ready4-dev.github.io/replica/articles/V_03.html">19</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/share-methods.html"> share </a>
#> </td>
#>    <td style="text-align:left;"> Share data or outputs </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4use/articles/V_01.html">2</a> , <a href="https://ready4-dev.github.io/scorz/articles/V_01.html">13</a>    , <a href="https://ready4-dev.github.io/scorz/articles/V_02.html">14</a>    , <a href="https://ready4-dev.github.io/youthvars/articles/V_02.html">16</a> </td>
#>   </tr>
#> </tbody>
#> </table>
print_methods(methods_tb, return_1L_chr = "slot")
#> <table class="table table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
#>  <thead>
#>   <tr>
#>    <th style="text-align:left;"> Method </th>
#>    <th style="text-align:left;"> Purpose </th>
#>    <th style="text-align:left;"> Examples </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/authorSlot-methods.html"> authorSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the author method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/characterizeSlot-methods.html"> characterizeSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the characterize method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/depictSlot-methods.html"> depictSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the depict method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/enhanceSlot-methods.html"> enhanceSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the enhance method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/exhibitSlot-methods.html"> exhibitSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the exhibit method to a specified slot </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html">1</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/ingestSlot-methods.html"> ingestSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the ingest method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/investigateSlot-methods.html"> investigateSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the investigate method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/manufactureSlot-methods.html"> manufactureSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the manufacture method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/metamorphoseSlot-methods.html"> metamorphoseSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the metamorphose method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/procureSlot-methods.html"> procureSlot </a>
#> </td>
#>    <td style="text-align:left;"> Retrieve the value stored in a specified slot </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html">1</a>, <a href="https://ready4-dev.github.io/ready4use/articles/V_04.html">5</a> , <a href="https://ready4-dev.github.io/scorz/articles/V_01.html">13</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/prognosticateSlot-methods.html"> prognosticateSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the prognosticate method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/ratifySlot-methods.html"> ratifySlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the ratify method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/reckonSlot-methods.html"> reckonSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the reckon method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/renewSlot-methods.html"> renewSlot </a>
#> </td>
#>    <td style="text-align:left;"> Update the value stored in a specified slot </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html">1</a>, <a href="https://ready4-dev.github.io/ready4use/articles/V_04.html">5</a> , <a href="https://ready4-dev.github.io/scorz/articles/V_01.html">13</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/shareSlot-methods.html"> shareSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the share method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#> </tbody>
#> </table>
print_methods(methods_tb, return_1L_chr = "extended")
#> <table class="table table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
#>  <thead>
#>   <tr>
#>    <th style="text-align:left;"> Method </th>
#>    <th style="text-align:left;"> Purpose </th>
#>    <th style="text-align:left;"> Examples </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/authorClasses-methods.html"> authorClasses </a>
#> </td>
#>    <td style="text-align:left;"> Create and document classes </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/authorData-methods.html"> authorData </a>
#> </td>
#>    <td style="text-align:left;"> Create and document datasets </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html">1</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/authorFunctions-methods.html"> authorFunctions </a>
#> </td>
#>    <td style="text-align:left;"> Create and document functions </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/authorReport-methods.html"> authorReport </a>
#> </td>
#>    <td style="text-align:left;"> Create reports </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html">1</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/authorSlot-methods.html"> authorSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the author method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/characterizeSlot-methods.html"> characterizeSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the characterize method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/depictSlot-methods.html"> depictSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the depict method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/enhanceSlot-methods.html"> enhanceSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the enhance method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/exhibitSlot-methods.html"> exhibitSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the exhibit method to a specified slot </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html">1</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/ingestSlot-methods.html"> ingestSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the ingest method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/investigateSlot-methods.html"> investigateSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the investigate method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/manufactureSlot-methods.html"> manufactureSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the manufacture method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/metamorphoseSlot-methods.html"> metamorphoseSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the metamorphose method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/procureSlot-methods.html"> procureSlot </a>
#> </td>
#>    <td style="text-align:left;"> Retrieve the value stored in a specified slot </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html">1</a>, <a href="https://ready4-dev.github.io/ready4use/articles/V_04.html">5</a> , <a href="https://ready4-dev.github.io/scorz/articles/V_01.html">13</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/prognosticateSlot-methods.html"> prognosticateSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the prognosticate method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/ratifySlot-methods.html"> ratifySlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the ratify method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/reckonSlot-methods.html"> reckonSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the reckon method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/renewSlot-methods.html"> renewSlot </a>
#> </td>
#>    <td style="text-align:left;"> Update the value stored in a specified slot </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html">1</a>, <a href="https://ready4-dev.github.io/ready4use/articles/V_04.html">5</a> , <a href="https://ready4-dev.github.io/scorz/articles/V_01.html">13</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/shareSlot-methods.html"> shareSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the share method to a specified slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#> </tbody>
#> </table>
```
