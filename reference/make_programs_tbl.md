# Make a tabular summary of programs using ready4 model modules

make_programs_tbl() scrapes the GitHub organisation and Zenodo community
associated specified for a ready4 model implementation to create a
tabular summary of programs and sub-routines associated with that
implementation.

## Usage

``` r
make_programs_tbl(
  what_1L_chr = c("Program", "Subroutine", "Program_and_Subroutine"),
  as_kbl_1L_lgl = FALSE,
  exclude_chr = character(0),
  format_1L_chr = "%d-%b-%Y",
  gh_repo_1L_chr = "ready4-dev/ready4",
  gh_tag_1L_chr = "Documentation_0.0",
  tidy_desc_1L_lgl = TRUE,
  url_stub_1L_chr = "https://ready4-dev.github.io/",
  zenodo_1L_chr = "ready4",
  ...
)
```

## Arguments

- what_1L_chr:

  What (a character vector of length one), Default: c("Program",
  "Subroutine", "Program_and_Subroutine")

- as_kbl_1L_lgl:

  As kable (a logical vector of length one), Default: FALSE

- exclude_chr:

  Exclude (a character vector), Default: character(0)

- format_1L_chr:

  Format (a character vector of length one), Default: '%d-%b-%Y'

- gh_repo_1L_chr:

  Github repository (a character vector of length one), Default:
  'ready4-dev/ready4'

- gh_tag_1L_chr:

  Github tag (a character vector of length one), Default:
  'Documentation_0.0'

- tidy_desc_1L_lgl:

  Tidy description (a logical vector of length one), Default: TRUE

- url_stub_1L_chr:

  Url stub (a character vector of length one), Default:
  'https://ready4-dev.github.io/'

- zenodo_1L_chr:

  Zenodo (a character vector of length one), Default: 'ready4'

- ...:

  Additional arguments

## Value

Programs (an output object of multiple potential types)

## See also

[`zen4R::ZenodoManager()`](https://rdrr.io/pkg/zen4R/man/ZenodoManager.html)

## Examples

``` r
if (FALSE) { # interactive()
  # Likely to take more than one minute to execute.
  if(requireNamespace("zen4R", quietly = TRUE)) {
    make_programs_tbl("Program",
                      gh_repo_1L_chr = "ready4-dev/ready4")
    make_programs_tbl("Subroutine",
                      gh_repo_1L_chr = "ready4-dev/ready4")
  }
}
```
