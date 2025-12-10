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
#>    <td style="text-align:left;"> Author and save files </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4fun/articles/V_01.html" style="     ">5</a>  , <a href="https://ready4-dev.github.io/ready4class/articles/V_01.html" style="     ">6</a>, <a href="https://ready4-dev.github.io/ready4pack/articles/V_01.html" style="     ">7</a> , <a href="https://ready4-dev.github.io/TTU/articles/V_01.html" style="     ">16</a>       , <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/authorClasses-methods.html"> authorClasses </a>
#> </td>
#>    <td style="text-align:left;"> Author and document classes </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/authorData-methods.html"> authorData </a>
#> </td>
#>    <td style="text-align:left;"> Author and document datasets </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html" style="     ">4</a>, <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/authorFunctions-methods.html"> authorFunctions </a>
#> </td>
#>    <td style="text-align:left;"> Author and document functions </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/authorReport-methods.html"> authorReport </a>
#> </td>
#>    <td style="text-align:left;"> Author and save a report </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html" style="     ">4</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/authorSlot-methods.html"> authorSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the author method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/characterize-methods.html"> characterize </a>
#> </td>
#>    <td style="text-align:left;"> Characterize model module data by generating (tabular) descriptive statistics </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/characterizeSlot-methods.html"> characterizeSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the characterize method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/depict-methods.html"> depict </a>
#> </td>
#>    <td style="text-align:left;"> Depict (plot) features of model module data </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/youthvars/articles/V_02.html" style="     ">13</a>, <a href="https://ready4-dev.github.io/scorz/articles/V_01.html" style="     ">14</a>    , <a href="https://ready4-dev.github.io/scorz/articles/V_02.html" style="     ">15</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/depictSlot-methods.html"> depictSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the depict method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/enhance-methods.html"> enhance </a>
#> </td>
#>    <td style="text-align:left;"> Enhance a model module by adding new elements </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/enhanceSlot-methods.html"> enhanceSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the enhance method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/exhibit-methods.html"> exhibit </a>
#> </td>
#>    <td style="text-align:left;"> Exhibit features of model module data by printing them to the R console </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4use/articles/V_02.html" style="     ">2</a>  , <a href="https://ready4-dev.github.io/ready4fun/articles/V_01.html" style="     ">5</a>  , <a href="https://ready4-dev.github.io/ready4class/articles/V_01.html" style="     ">6</a>, <a href="https://ready4-dev.github.io/youthvars/articles/V_02.html" style="     ">13</a> , <a href="https://ready4-dev.github.io/scorz/articles/V_01.html" style="     ">14</a>     , <a href="https://ready4-dev.github.io/scorz/articles/V_02.html" style="     ">15</a>     , <a href="https://ready4-dev.github.io/TTU/articles/V_01.html" style="     ">16</a>       , <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a>  , <a href="https://ready4-dev.github.io/youthu/articles/V_01.html" style="     ">18</a>    , <a href="https://ready4-dev.github.io/costly/articles/V_01.html" style="     ">19</a>    , <a href="https://ready4-dev.github.io/costly/articles/V_02.html" style="     ">20</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/exhibitSlot-methods.html"> exhibitSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the exhibit method to a model module slot </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html" style="     ">4</a>, <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a> , <a href="https://ready4-dev.github.io/costly/articles/V_01.html" style="     ">19</a>   , <a href="https://ready4-dev.github.io/costly/articles/V_02.html" style="     ">20</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/ingest-methods.html"> ingest </a>
#> </td>
#>    <td style="text-align:left;"> Ingest data </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4use/articles/V_01.html" style="     ">1</a>  , <a href="https://ready4-dev.github.io/ready4use/articles/V_02.html" style="     ">2</a>  , <a href="https://ready4-dev.github.io/ready4use/articles/V_03.html" style="     ">3</a>  , <a href="https://ready4-dev.github.io/ready4class/articles/V_01.html" style="     ">6</a>, <a href="https://ready4-dev.github.io/youthvars/articles/V_02.html" style="     ">13</a> , <a href="https://ready4-dev.github.io/scorz/articles/V_01.html" style="     ">14</a>     , <a href="https://ready4-dev.github.io/scorz/articles/V_02.html" style="     ">15</a>     , <a href="https://ready4-dev.github.io/TTU/articles/V_01.html" style="     ">16</a>       , <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a>  , <a href="https://ready4-dev.github.io/youthu/articles/V_01.html" style="     ">18</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/ingestSlot-methods.html"> ingestSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the ingest method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/investigate-methods.html"> investigate </a>
#> </td>
#>    <td style="text-align:left;"> Investigate solutions to an inverse problem </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/TTU/articles/V_01.html" style="     ">16</a>     , <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/investigateSlot-methods.html"> investigateSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the investigate method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/manufacture-methods.html"> manufacture </a>
#> </td>
#>    <td style="text-align:left;"> Manufacture a new object </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/manufactureSlot-methods.html"> manufactureSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the manufacture method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/metamorphose-methods.html"> metamorphose </a>
#> </td>
#>    <td style="text-align:left;"> Metamorphose a model module to a model module of a different (non-inheriting) class </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/metamorphoseSlot-methods.html"> metamorphoseSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the metamorphose method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/procure-methods.html"> procure </a>
#> </td>
#>    <td style="text-align:left;"> Procure items from a dataset </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4fun/articles/V_01.html" style="     ">5</a>, <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/procureSlot-methods.html"> procureSlot </a>
#> </td>
#>    <td style="text-align:left;"> Procure (get) data from a slot </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html" style="     ">4</a>, <a href="https://ready4-dev.github.io/scorz/articles/V_01.html" style="     ">14</a>    , <a href="https://ready4-dev.github.io/TTU/articles/V_01.html" style="     ">16</a>      , <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/prognosticate-methods.html"> prognosticate </a>
#> </td>
#>    <td style="text-align:left;"> Prognosticate (make predictions) by solving a forward problem </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/prognosticateSlot-methods.html"> prognosticateSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the prognosticate method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/ratify-methods.html"> ratify </a>
#> </td>
#>    <td style="text-align:left;"> Ratify that input or output data meet validity criteria </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/youthvars/articles/V_02.html" style="     ">13</a>, <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a> , <a href="https://ready4-dev.github.io/costly/articles/V_01.html" style="     ">19</a>   , <a href="https://ready4-dev.github.io/costly/articles/V_02.html" style="     ">20</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/ratifySlot-methods.html"> ratifySlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the ratify method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/reckon-methods.html"> reckon </a>
#> </td>
#>    <td style="text-align:left;"> Reckon (calculate) a value </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/reckonSlot-methods.html"> reckonSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the reckon method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/renew-methods.html"> renew </a>
#> </td>
#>    <td style="text-align:left;"> Renew (update) values </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4use/articles/V_01.html" style="     ">1</a> , <a href="https://ready4-dev.github.io/ready4use/articles/V_02.html" style="     ">2</a> , <a href="https://ready4-dev.github.io/youthvars/articles/V_02.html" style="     ">13</a>, <a href="https://ready4-dev.github.io/scorz/articles/V_01.html" style="     ">14</a>    , <a href="https://ready4-dev.github.io/scorz/articles/V_02.html" style="     ">15</a>    , <a href="https://ready4-dev.github.io/TTU/articles/V_01.html" style="     ">16</a>      , <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a> , <a href="https://ready4-dev.github.io/costly/articles/V_01.html" style="     ">19</a>   , <a href="https://ready4-dev.github.io/costly/articles/V_02.html" style="     ">20</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/renewSlot-methods.html"> renewSlot </a>
#> </td>
#>    <td style="text-align:left;"> Renew (set) the values of data in a module slot </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html" style="     ">4</a>, <a href="https://ready4-dev.github.io/scorz/articles/V_01.html" style="     ">14</a>    , <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a> , <a href="https://ready4-dev.github.io/costly/articles/V_01.html" style="     ">19</a>   , <a href="https://ready4-dev.github.io/costly/articles/V_02.html" style="     ">20</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/share-methods.html"> share </a>
#> </td>
#>    <td style="text-align:left;"> Share data via an online repository </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4use/articles/V_01.html" style="     ">1</a> , <a href="https://ready4-dev.github.io/youthvars/articles/V_02.html" style="     ">13</a>, <a href="https://ready4-dev.github.io/scorz/articles/V_01.html" style="     ">14</a>    , <a href="https://ready4-dev.github.io/scorz/articles/V_02.html" style="     ">15</a>    , <a href="https://ready4-dev.github.io/TTU/articles/V_01.html" style="     ">16</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/shareSlot-methods.html"> shareSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the share method to a model module slot </td>
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
#>    <td style="text-align:left;"> Author and save files </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4fun/articles/V_01.html" style="     ">5</a>  , <a href="https://ready4-dev.github.io/ready4class/articles/V_01.html" style="     ">6</a>, <a href="https://ready4-dev.github.io/ready4pack/articles/V_01.html" style="     ">7</a> , <a href="https://ready4-dev.github.io/TTU/articles/V_01.html" style="     ">16</a>       , <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/characterize-methods.html"> characterize </a>
#> </td>
#>    <td style="text-align:left;"> Characterize model module data by generating (tabular) descriptive statistics </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/depict-methods.html"> depict </a>
#> </td>
#>    <td style="text-align:left;"> Depict (plot) features of model module data </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/youthvars/articles/V_02.html" style="     ">13</a>, <a href="https://ready4-dev.github.io/scorz/articles/V_01.html" style="     ">14</a>    , <a href="https://ready4-dev.github.io/scorz/articles/V_02.html" style="     ">15</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/enhance-methods.html"> enhance </a>
#> </td>
#>    <td style="text-align:left;"> Enhance a model module by adding new elements </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/exhibit-methods.html"> exhibit </a>
#> </td>
#>    <td style="text-align:left;"> Exhibit features of model module data by printing them to the R console </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4use/articles/V_02.html" style="     ">2</a>  , <a href="https://ready4-dev.github.io/ready4fun/articles/V_01.html" style="     ">5</a>  , <a href="https://ready4-dev.github.io/ready4class/articles/V_01.html" style="     ">6</a>, <a href="https://ready4-dev.github.io/youthvars/articles/V_02.html" style="     ">13</a> , <a href="https://ready4-dev.github.io/scorz/articles/V_01.html" style="     ">14</a>     , <a href="https://ready4-dev.github.io/scorz/articles/V_02.html" style="     ">15</a>     , <a href="https://ready4-dev.github.io/TTU/articles/V_01.html" style="     ">16</a>       , <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a>  , <a href="https://ready4-dev.github.io/youthu/articles/V_01.html" style="     ">18</a>    , <a href="https://ready4-dev.github.io/costly/articles/V_01.html" style="     ">19</a>    , <a href="https://ready4-dev.github.io/costly/articles/V_02.html" style="     ">20</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/ingest-methods.html"> ingest </a>
#> </td>
#>    <td style="text-align:left;"> Ingest data </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4use/articles/V_01.html" style="     ">1</a>  , <a href="https://ready4-dev.github.io/ready4use/articles/V_02.html" style="     ">2</a>  , <a href="https://ready4-dev.github.io/ready4use/articles/V_03.html" style="     ">3</a>  , <a href="https://ready4-dev.github.io/ready4class/articles/V_01.html" style="     ">6</a>, <a href="https://ready4-dev.github.io/youthvars/articles/V_02.html" style="     ">13</a> , <a href="https://ready4-dev.github.io/scorz/articles/V_01.html" style="     ">14</a>     , <a href="https://ready4-dev.github.io/scorz/articles/V_02.html" style="     ">15</a>     , <a href="https://ready4-dev.github.io/TTU/articles/V_01.html" style="     ">16</a>       , <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a>  , <a href="https://ready4-dev.github.io/youthu/articles/V_01.html" style="     ">18</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/investigate-methods.html"> investigate </a>
#> </td>
#>    <td style="text-align:left;"> Investigate solutions to an inverse problem </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/TTU/articles/V_01.html" style="     ">16</a>     , <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/manufacture-methods.html"> manufacture </a>
#> </td>
#>    <td style="text-align:left;"> Manufacture a new object </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/metamorphose-methods.html"> metamorphose </a>
#> </td>
#>    <td style="text-align:left;"> Metamorphose a model module to a model module of a different (non-inheriting) class </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/procure-methods.html"> procure </a>
#> </td>
#>    <td style="text-align:left;"> Procure items from a dataset </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4fun/articles/V_01.html" style="     ">5</a>, <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/prognosticate-methods.html"> prognosticate </a>
#> </td>
#>    <td style="text-align:left;"> Prognosticate (make predictions) by solving a forward problem </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/ratify-methods.html"> ratify </a>
#> </td>
#>    <td style="text-align:left;"> Ratify that input or output data meet validity criteria </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/youthvars/articles/V_02.html" style="     ">13</a>, <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a> , <a href="https://ready4-dev.github.io/costly/articles/V_01.html" style="     ">19</a>   , <a href="https://ready4-dev.github.io/costly/articles/V_02.html" style="     ">20</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/reckon-methods.html"> reckon </a>
#> </td>
#>    <td style="text-align:left;"> Reckon (calculate) a value </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/renew-methods.html"> renew </a>
#> </td>
#>    <td style="text-align:left;"> Renew (update) values </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4use/articles/V_01.html" style="     ">1</a> , <a href="https://ready4-dev.github.io/ready4use/articles/V_02.html" style="     ">2</a> , <a href="https://ready4-dev.github.io/youthvars/articles/V_02.html" style="     ">13</a>, <a href="https://ready4-dev.github.io/scorz/articles/V_01.html" style="     ">14</a>    , <a href="https://ready4-dev.github.io/scorz/articles/V_02.html" style="     ">15</a>    , <a href="https://ready4-dev.github.io/TTU/articles/V_01.html" style="     ">16</a>      , <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a> , <a href="https://ready4-dev.github.io/costly/articles/V_01.html" style="     ">19</a>   , <a href="https://ready4-dev.github.io/costly/articles/V_02.html" style="     ">20</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/share-methods.html"> share </a>
#> </td>
#>    <td style="text-align:left;"> Share data via an online repository </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4use/articles/V_01.html" style="     ">1</a> , <a href="https://ready4-dev.github.io/youthvars/articles/V_02.html" style="     ">13</a>, <a href="https://ready4-dev.github.io/scorz/articles/V_01.html" style="     ">14</a>    , <a href="https://ready4-dev.github.io/scorz/articles/V_02.html" style="     ">15</a>    , <a href="https://ready4-dev.github.io/TTU/articles/V_01.html" style="     ">16</a> </td>
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
#>    <td style="text-align:left;"> Apply the author method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/characterizeSlot-methods.html"> characterizeSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the characterize method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/depictSlot-methods.html"> depictSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the depict method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/enhanceSlot-methods.html"> enhanceSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the enhance method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/exhibitSlot-methods.html"> exhibitSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the exhibit method to a model module slot </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html" style="     ">4</a>, <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a> , <a href="https://ready4-dev.github.io/costly/articles/V_01.html" style="     ">19</a>   , <a href="https://ready4-dev.github.io/costly/articles/V_02.html" style="     ">20</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/ingestSlot-methods.html"> ingestSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the ingest method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/investigateSlot-methods.html"> investigateSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the investigate method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/manufactureSlot-methods.html"> manufactureSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the manufacture method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/metamorphoseSlot-methods.html"> metamorphoseSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the metamorphose method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/procureSlot-methods.html"> procureSlot </a>
#> </td>
#>    <td style="text-align:left;"> Procure (get) data from a slot </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html" style="     ">4</a>, <a href="https://ready4-dev.github.io/scorz/articles/V_01.html" style="     ">14</a>    , <a href="https://ready4-dev.github.io/TTU/articles/V_01.html" style="     ">16</a>      , <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/prognosticateSlot-methods.html"> prognosticateSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the prognosticate method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/ratifySlot-methods.html"> ratifySlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the ratify method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/reckonSlot-methods.html"> reckonSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the reckon method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/renewSlot-methods.html"> renewSlot </a>
#> </td>
#>    <td style="text-align:left;"> Renew (set) the values of data in a module slot </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html" style="     ">4</a>, <a href="https://ready4-dev.github.io/scorz/articles/V_01.html" style="     ">14</a>    , <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a> , <a href="https://ready4-dev.github.io/costly/articles/V_01.html" style="     ">19</a>   , <a href="https://ready4-dev.github.io/costly/articles/V_02.html" style="     ">20</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/shareSlot-methods.html"> shareSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the share method to a model module slot </td>
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
#>    <td style="text-align:left;"> Author and document classes </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/authorData-methods.html"> authorData </a>
#> </td>
#>    <td style="text-align:left;"> Author and document datasets </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html" style="     ">4</a>, <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/authorFunctions-methods.html"> authorFunctions </a>
#> </td>
#>    <td style="text-align:left;"> Author and document functions </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/authorReport-methods.html"> authorReport </a>
#> </td>
#>    <td style="text-align:left;"> Author and save a report </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html" style="     ">4</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/authorSlot-methods.html"> authorSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the author method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/characterizeSlot-methods.html"> characterizeSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the characterize method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/depictSlot-methods.html"> depictSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the depict method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/enhanceSlot-methods.html"> enhanceSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the enhance method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/exhibitSlot-methods.html"> exhibitSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the exhibit method to a model module slot </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html" style="     ">4</a>, <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a> , <a href="https://ready4-dev.github.io/costly/articles/V_01.html" style="     ">19</a>   , <a href="https://ready4-dev.github.io/costly/articles/V_02.html" style="     ">20</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/ingestSlot-methods.html"> ingestSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the ingest method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/investigateSlot-methods.html"> investigateSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the investigate method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/manufactureSlot-methods.html"> manufactureSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the manufacture method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/metamorphoseSlot-methods.html"> metamorphoseSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the metamorphose method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/procureSlot-methods.html"> procureSlot </a>
#> </td>
#>    <td style="text-align:left;"> Procure (get) data from a slot </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html" style="     ">4</a>, <a href="https://ready4-dev.github.io/scorz/articles/V_01.html" style="     ">14</a>    , <a href="https://ready4-dev.github.io/TTU/articles/V_01.html" style="     ">16</a>      , <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/prognosticateSlot-methods.html"> prognosticateSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the prognosticate method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/ratifySlot-methods.html"> ratifySlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the ratify method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/reckonSlot-methods.html"> reckonSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the reckon method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/renewSlot-methods.html"> renewSlot </a>
#> </td>
#>    <td style="text-align:left;"> Renew (set) the values of data in a module slot </td>
#>    <td style="text-align:left;"> <a href="https://ready4-dev.github.io/ready4show/articles/V_01.html" style="     ">4</a>, <a href="https://ready4-dev.github.io/scorz/articles/V_01.html" style="     ">14</a>    , <a href="https://ready4-dev.github.io/specific/articles/V_01.html" style="     ">17</a> , <a href="https://ready4-dev.github.io/costly/articles/V_01.html" style="     ">19</a>   , <a href="https://ready4-dev.github.io/costly/articles/V_02.html" style="     ">20</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://ready4-dev.github.io/ready4/reference/shareSlot-methods.html"> shareSlot </a>
#> </td>
#>    <td style="text-align:left;"> Apply the share method to a model module slot </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#> </tbody>
#> </table>
```
