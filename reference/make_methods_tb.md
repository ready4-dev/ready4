# Make a tabular summary of methods associated with ready model modules

make_methods_tb() scrapes the documentation websites of all libraries of
ready4 modules in a specified GitHub organisation and then creates a
tabular summary of vignette examples of ready4 module methods.

## Usage

``` r
make_methods_tb(
  packages_tb = NULL,
  exclude_mthds_for_chr = NA_character_,
  framework_only_1L_lgl = TRUE,
  gh_repo_1L_chr = "ready4-dev/ready4",
  gh_tag_1L_chr = "Documentation_0.0",
  module_pkgs_chr = character(0),
  ns_var_nm_1L_chr = "pt_ns_chr",
  path_1L_chr = character(0),
  return_1L_chr = "all"
)
```

## Arguments

- packages_tb:

  Packages (a tibble), Default: NULL

- exclude_mthds_for_chr:

  Exclude methods for (a character vector), Default: 'NA'

- framework_only_1L_lgl:

  Framework only (a logical vector of length one), Default: TRUE

- gh_repo_1L_chr:

  Github repository (a character vector of length one), Default:
  'ready4-dev/ready4'

- gh_tag_1L_chr:

  Github tag (a character vector of length one), Default:
  'Documentation_0.0'

- module_pkgs_chr:

  Module packages (a character vector), Default: character(0)

- ns_var_nm_1L_chr:

  Namespace variable name (a character vector of length one), Default:
  'pt_ns_chr'

- path_1L_chr:

  Path (a character vector of length one), Default: character(0)

- return_1L_chr:

  Return (a character vector of length one), Default: 'all'

## Value

Methods (a tibble)

## Examples

``` r
if (FALSE) { # interactive()
  # Likely to take more than one minute to execute.
  make_methods_tb(gh_repo_1L_chr = "ready4-dev/ready4")
}
```
