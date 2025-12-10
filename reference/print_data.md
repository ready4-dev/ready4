# Print a table of ready4 model data collections

print_data() formats the output of either get_datasts_tb() or
make_datasts_tb() as HTML. The type of output can be customised to
display Dataverse data collections or Dataverse datasets. Similarly
output can be restricted to real or toy datasets.

## Usage

``` r
print_data(
  datasets_tb,
  by_dv_1L_lgl = FALSE,
  filter_cdns_ls = NULL,
  root_1L_chr = "https://dataverse.harvard.edu/dataverse/",
  scroll_height_1L_chr = character(0),
  scroll_width_1L_chr = character(0),
  toy_data_dv_1L_chr = "fakes",
  what_1L_chr = "all",
  ...
)
```

## Arguments

- datasets_tb:

  Datasets (a tibble)

- by_dv_1L_lgl:

  By dataverse (a logical vector of length one), Default: FALSE

- filter_cdns_ls:

  Filter conditions (a list), Default: NULL

- root_1L_chr:

  Root (a character vector of length one), Default:
  'https://dataverse.harvard.edu/dataverse/'

- scroll_height_1L_chr:

  Scroll height (a character vector of length one), Default:
  character(0)

- scroll_width_1L_chr:

  Scroll width (a character vector of length one), Default: character(0)

- toy_data_dv_1L_chr:

  Toy data dataverse (a character vector of length one), Default:
  'fakes'

- what_1L_chr:

  What (a character vector of length one), Default: 'all'

- ...:

  Additional arguments

## Value

Datasets (a kable)

## Examples

``` r
datasets_tb <- get_datasets_tb("ready4-dev/ready4")
print_data(datasets_tb, by_dv_1L_lgl = TRUE)
#> <table class="table table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
#>  <thead>
#>   <tr>
#>    <th style="text-align:left;"> Dataverse </th>
#>    <th style="text-align:left;"> Name </th>
#>    <th style="text-align:left;"> Description </th>
#>    <th style="text-align:left;"> Creator </th>
#>    <th style="text-align:left;"> Datasets </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://dataverse.harvard.edu/dataverse/fakes"> fakes </a>
#> </td>
#>    <td style="text-align:left;"> Fake Data For Instruction And Illustration </td>
#>    <td style="text-align:left;"> Fake data used to illustrate toolkits developed with the ready4 open science framework. </td>
#>    <td style="text-align:left;"> Orygen </td>
#>    <td style="text-align:left;"> <a href="https://doi.org/10.7910/DVN/D74QMP" style="     ">1</a>, <a href="https://doi.org/10.7910/DVN/612HDC" style="     ">2</a>, <a href="https://doi.org/10.7910/DVN/HJXYKQ" style="     ">3</a>, <a href="https://doi.org/10.7910/DVN/W95KED" style="     ">4</a>, <a href="https://doi.org/10.7910/DVN/GW7ZKC" style="     ">5</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://dataverse.harvard.edu/dataverse/firstbounce"> firstbounce </a>
#> </td>
#>    <td style="text-align:left;"> First Bounce </td>
#>    <td style="text-align:left;"> A ready4 framework model of platforms. Aims to identify opportunities to improve the efficiency and equity of mental health services. </td>
#>    <td style="text-align:left;"> Orygen </td>
#>    <td style="text-align:left;">  </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://dataverse.harvard.edu/dataverse/ready4fw"> ready4fw </a>
#> </td>
#>    <td style="text-align:left;"> ready4 Framework </td>
#>    <td style="text-align:left;"> A collection of datasets that support implementation of the ready4 framework for open science computational models of mental health systems. </td>
#>    <td style="text-align:left;"> Orygen </td>
#>    <td style="text-align:left;"> <a href="https://doi.org/10.7910/DVN/RIQTKK" style="     ">6</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://dataverse.harvard.edu/dataverse/readyforwhatsnext"> readyforwhatsnext </a>
#> </td>
#>    <td style="text-align:left;"> readyforwhatsnext </td>
#>    <td style="text-align:left;"> Data collections for the readyforwhatsnext mental health systems model. </td>
#>    <td style="text-align:left;"> Orygen </td>
#>    <td style="text-align:left;"> <a href="https://doi.org/10.7910/DVN/QBZFQV" style="     ">7</a>, <a href="https://doi.org/10.7910/DVN/JHSCDJ" style="     ">8</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://dataverse.harvard.edu/dataverse/springtides"> springtides </a>
#> </td>
#>    <td style="text-align:left;"> Springtides </td>
#>    <td style="text-align:left;"> A ready4 framework model of places. Synthesises geometry (boundary, coordinate) and spatial attribute (e.g. population counts, environmental characteristics, service identifier and model coefficients associated with areas) data. </td>
#>    <td style="text-align:left;"> Orygen </td>
#>    <td style="text-align:left;"> <a href="https://doi.org/10.7910/DVN/V3OKZV" style="     ">9</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://dataverse.harvard.edu/dataverse/springtolife"> springtolife </a>
#> </td>
#>    <td style="text-align:left;"> Spring To Life </td>
#>    <td style="text-align:left;"> A ready4 framework model of people. Models the characteristics, behaviours, relationships and outcomes of groups of individuals relevant to policymakers and service planners aiming to improve population mental health. </td>
#>    <td style="text-align:left;"> Orygen </td>
#>    <td style="text-align:left;"> <a href="https://doi.org/10.7910/DVN/VGPIPS" style="     ">10</a> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"><a href="https://dataverse.harvard.edu/dataverse/TTU"> TTU </a>
#> </td>
#>    <td style="text-align:left;"> Transfer to Utility </td>
#>    <td style="text-align:left;"> A collection of transfer to utility datasets developed with the ready4 open science framework. </td>
#>    <td style="text-align:left;"> Orygen </td>
#>    <td style="text-align:left;"> <a href="https://doi.org/10.7910/DVN/DKDIB0" style="     ">11</a> </td>
#>   </tr>
#> </tbody>
#> </table>
print_data(datasets_tb, what_1L_chr = "real")
#> <table class="table table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
#>  <thead>
#>   <tr>
#>    <th style="text-align:left;"> Title </th>
#>    <th style="text-align:left;"> Description </th>
#>    <th style="text-align:left;"> Dataverse </th>
#>    <th style="text-align:left;"> DOI </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:left;"> ready4 Framework Abbreviations and Definitions </td>
#>    <td style="text-align:left;"> This dataset contains resources that help ready4 Framework Developers adopt common standards and workflows. </td>
#>    <td style="text-align:left;"> ready4fw </td>
#>    <td style="text-align:left;"> https://doi.org/10.7910/DVN/RIQTKK </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> readyforwhatsnext posters </td>
#>    <td style="text-align:left;"> A collection of poster summaries about the readyforwhatsnext project and its outputs. </td>
#>    <td style="text-align:left;"> readyforwhatsnext </td>
#>    <td style="text-align:left;"> https://doi.org/10.7910/DVN/QBZFQV </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> Australian demographic input parameters for Springtides model </td>
#>    <td style="text-align:left;"> Geometry, spatial attribute and metadata inputs for the demographic module of the readyforwhatsnext model. The demographic module is a systems dynamics spatial simulation of area demographic characteristics. The current version of the model is quite rudimentary and is designed to be extended by other models developped with the ready4 open science mental health modelling tools. </td>
#>    <td style="text-align:left;"> readyforwhatsnext </td>
#>    <td style="text-align:left;"> https://doi.org/10.7910/DVN/JHSCDJ </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> Springtides reports for Local Government Areas in the North West of Melbourne </td>
#>    <td style="text-align:left;"> This dataset is a collection of reports generated by a development version of the Springtides Model Of Places. Each report summarises prevalence projections for a specified mental disorder / mental health condition for a Local Government Area that is wholly or partially within the catchment area of the Orygen youth mental health service in North West Melbourne. As these reports were generated by a development version of the Springtides Model, these projections should be regarded as exploratory. </td>
#>    <td style="text-align:left;"> springtides </td>
#>    <td style="text-align:left;"> https://doi.org/10.7910/DVN/V3OKZV </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> Modelling the online helpseeking choice of socially anxious young people </td>
#>    <td style="text-align:left;"> Models to predict the online helpseeking choices of socially anxious young people in Australia and replication code and documentation to implement the discrete choice experiment that generated the models.
#> 
#> All study outputs were created with the aid of the mychoice R package (https://ready4-dev.github.io/mychoice). </td>
#>    <td style="text-align:left;"> springtolife </td>
#>    <td style="text-align:left;"> https://doi.org/10.7910/DVN/VGPIPS </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> Transfer to AQoL-6D Utility Mapping Algorithms </td>
#>    <td style="text-align:left;"> Catalogues of models (and the programs that produced them) that can be used in conjunction with the youthu R package to predict AQoL-6D health utility (and thus, derive QALYs) from measures collected in youth mental health services. </td>
#>    <td style="text-align:left;"> TTU </td>
#>    <td style="text-align:left;"> https://doi.org/10.7910/DVN/DKDIB0 </td>
#>   </tr>
#> </tbody>
#> </table>
print_data(datasets_tb, what_1L_chr = "fakes")
#> <table class="table table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
#>  <thead>
#>   <tr>
#>    <th style="text-align:left;"> Title </th>
#>    <th style="text-align:left;"> Description </th>
#>    <th style="text-align:left;"> Dataverse </th>
#>    <th style="text-align:left;"> DOI </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:left;"> TTU (Transfer to Utility) R package - AQoL-6D vignette output </td>
#>    <td style="text-align:left;"> This dataset has been generated from fake data as an instructional aid. It is not to be used to inform decision making. </td>
#>    <td style="text-align:left;"> fakes </td>
#>    <td style="text-align:left;"> https://doi.org/10.7910/DVN/D74QMP </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> TTU (Transfer to Utility) R package - EQ-5D vignette output </td>
#>    <td style="text-align:left;"> This dataset is provided as a teaching aid. It is the output of tools from the TTU R package, applied to a synthetic dataset  (Fake Data) of psychological distress and psychological wellbeing. It is not to be used to support decision-making. </td>
#>    <td style="text-align:left;"> fakes </td>
#>    <td style="text-align:left;"> https://doi.org/10.7910/DVN/612HDC </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> Synthetic (fake) youth mental health datasets and data dictionaries </td>
#>    <td style="text-align:left;"> The datasets in this collection are entirely fake. They were developed principally to demonstrate the workings of a number of utility scoring and mapping algorithms. However, they may be of more general use to others. In some limited cases, some of the included files could be used in exploratory simulation based analyses. However, you should read the metadata descriptors for each file to inform yourself of the validity and limitations of each fake dataset. To open the RDS format files included in this dataset, the R package ready4use needs to be installed (see https://ready4-dev.github.io/ready4use/ ). It is also recommended that you install the youthvars package ( https://ready4-dev.github.io/youthvars/) as this provides useful tools for inspecting and validating each dataset. </td>
#>    <td style="text-align:left;"> fakes </td>
#>    <td style="text-align:left;"> https://doi.org/10.7910/DVN/HJXYKQ </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> ready4use R package vignette output </td>
#>    <td style="text-align:left;"> This dataset is provided so that others can compare the output they generate when implementing vignette code with that generated by the authors. </td>
#>    <td style="text-align:left;"> fakes </td>
#>    <td style="text-align:left;"> https://doi.org/10.7910/DVN/W95KED </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> Specific R Package - AQoL-6D Vignette Output </td>
#>    <td style="text-align:left;"> This dataset is provided so that others can apply the algorithms we have developed, consistent with the principles of the ready4 open science framework for data synthesis and simulation in mental health. </td>
#>    <td style="text-align:left;"> fakes </td>
#>    <td style="text-align:left;"> https://doi.org/10.7910/DVN/GW7ZKC </td>
#>   </tr>
#> </tbody>
#> </table>
```
