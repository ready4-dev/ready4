# Make a tabular summary of ready4 model modules and sub-modules

make_modules_tb() scrapes the documentation websites of all libraries of
ready4 modules in a specified GitHub organisation and then creates a
tabular summary of the modules included in those libraries and vignette
examples of their use.

## Usage

``` r
make_modules_tb(
  pkg_extensions_tb = NULL,
  cls_extensions_tb = NULL,
  gh_repo_1L_chr = "ready4-dev/ready4",
  gh_tag_1L_chr = "Documentation_0.0",
  module_pkgs_chr = character(0),
  include_1L_chr = "modules",
  ns_var_nm_1L_chr = "pt_ns_chr",
  url_stub_1L_chr = "https://ready4-dev.github.io/",
  what_chr = "all"
)
```

## Arguments

- pkg_extensions_tb:

  Package extensions (a tibble), Default: NULL

- cls_extensions_tb:

  Class extensions (a tibble), Default: NULL

- gh_repo_1L_chr:

  Github repository (a character vector of length one), Default:
  'ready4-dev/ready4'

- gh_tag_1L_chr:

  Github tag (a character vector of length one), Default:
  'Documentation_0.0'

- module_pkgs_chr:

  Module packages (a character vector), Default: character(0)

- include_1L_chr:

  Include (a character vector of length one), Default: 'modules'

- ns_var_nm_1L_chr:

  Namespace variable name (a character vector of length one), Default:
  'pt_ns_chr'

- url_stub_1L_chr:

  Url stub (a character vector of length one), Default:
  'https://ready4-dev.github.io/'

- what_chr:

  What (a character vector), Default: 'all'

## Value

Modules (a tibble)

## Examples

``` r
if (FALSE) { # interactive()
  # Likely to take more than one minute to execute.
  make_modules_tb(gh_repo_1L_chr = "ready4-dev/ready4")
}
```
