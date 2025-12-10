# Write library metadata

write_library_metadata() is a Write function that writes a file to a
specified local directory. Specifically, this function implements an
algorithm to write library metadata. The function is called for its side
effects and does not return a value.

## Usage

``` r
write_library_metadata(
  additions_tb = make_additions_tb(),
  libraries_ls = NULL,
  libraries_tb = NULL,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  exclude_mthds_for_chr = NA_character_,
  gh_repo_1L_chr = "ready4-dev/ready4",
  gh_tag_1L_chr = "Documentation_0.0",
  include_1L_chr = "all",
  module_pkgs_chr = character(0),
  options_chr = c("Y", "N"),
  ns_var_nm_1L_chr = "pt_ns_chr",
  path_1L_chr = character(0),
  reference_var_nm_1L_chr = "Reference",
  return_1L_chr = "all",
  url_stub_1L_chr = "https://ready4-dev.github.io/",
  vignette_var_nm_1L_chr = "Vignettes",
  vignette_url_var_nm_1L_chr = "Vignettes_URLs",
  what_chr = "all"
)
```

## Arguments

- additions_tb:

  Additions (a tibble), Default: make_additions_tb()

- libraries_ls:

  Libraries (a list), Default: NULL

- libraries_tb:

  Libraries (a tibble), Default: NULL

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- exclude_mthds_for_chr:

  Exclude methods for (a character vector), Default: 'NA'

- gh_repo_1L_chr:

  Github repository (a character vector of length one), Default:
  'ready4-dev/ready4'

- gh_tag_1L_chr:

  Github tag (a character vector of length one), Default:
  'Documentation_0.0'

- include_1L_chr:

  Include (a character vector of length one), Default: 'all'

- module_pkgs_chr:

  Module packages (a character vector), Default: character(0)

- options_chr:

  Options (a character vector), Default: c("Y", "N")

- ns_var_nm_1L_chr:

  Namespace variable name (a character vector of length one), Default:
  'pt_ns_chr'

- path_1L_chr:

  Path (a character vector of length one), Default: character(0)

- reference_var_nm_1L_chr:

  Reference variable name (a character vector of length one), Default:
  'Reference'

- return_1L_chr:

  Return (a character vector of length one), Default: 'all'

- url_stub_1L_chr:

  Url stub (a character vector of length one), Default:
  'https://ready4-dev.github.io/'

- vignette_var_nm_1L_chr:

  Vignette variable name (a character vector of length one), Default:
  'Vignettes'

- vignette_url_var_nm_1L_chr:

  Vignette url variable name (a character vector of length one),
  Default: 'Vignettes_URLs'

- what_chr:

  What (a character vector), Default: 'all'

## Value

No return value, called for side effects.
