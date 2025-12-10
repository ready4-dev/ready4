# Get data from the internet with graceful failure

get_gracefully() attempts to retrieve objects from the internet but
returns NULL and an informative message if there is no internet
connection or the specified resource could not be found.

## Usage

``` r
get_gracefully(
  url_1L_chr,
  args_ls = NULL,
  fn = readRDS,
  not_chr_1L_lgl = NA_character_,
  tests_chr = character(0)
)
```

## Arguments

- url_1L_chr:

  Url (a character vector of length one)

- args_ls:

  Arguments (a list), Default: NULL

- fn:

  Function (a function), Default: readRDS

- not_chr_1L_lgl:

  Not character vector (a logical vector of length one), Default: 'NA'

- tests_chr:

  Tests (a character vector), Default: character(0)

## Value

Object (an output object of multiple potential types)

## Examples

``` r
if (FALSE) { # interactive()
  # Likely execution time greater than current CRAN limit.
get_gracefully(paste0("https://github.com/ready4-dev/ready4/",
                      "releases/download/Documentation_0.0/ready4_badges_lup.RDS"))
  get_gracefully("DOES NOT EXIST")
if(requireNamespace("dataverse", quietly = TRUE)) {
  get_gracefully("https://doi.org/10.7910/DVN/RIQTKK", fn = dataverse::dataset_files,
                 args_ls = list(key = NULL, server = "dataverse.harvard.edu"))
    get_gracefully("https://doi.org/10.7910/DVN/RIQTKK", fn = dataverse::dataset_files,
                   args_ls = list(key = NULL, server = "DOES_NOT_EXIST"))
    get_gracefully("DOES_NOT_EXIST", fn = dataverse::dataset_files,
                   args_ls = list(key = NULL, server = "dataverse.harvard.edu"))

}
if (requireNamespace("gh", quietly = TRUE)) {
  get_gracefully("/orgs/ready4-dev/repos", fn = gh::gh, args_ls=list(type = "public"))
      get_gracefully("DOES_NOT_EXIST", fn = gh::gh, args_ls=list(type = "public"))
}
if(requireNamespace("piggyback", quietly = TRUE)) {
  get_gracefully(NULL, fn = piggyback::pb_download_url,
                 args_ls = list(repo = "ready4-dev/ready4",
                                tag = "Documentation_0.0",
                                .token = ""))
    get_gracefully(NULL, fn = piggyback::pb_download_url,
                   args_ls = list(repo = "DOES_NOT_EXIST",
                                  tag = "DOES_NOT_EXIST",
                                  .token = ""))
}
if(requireNamespace("rvest", quietly = TRUE)) {
  get_gracefully("https://ready4-dev.github.io/ready4/index.html", fn=rvest::read_html)
    get_gracefully("DOES_NOT_EXIST", fn=rvest::read_html)
}

if(requireNamespace("tidyRSS", quietly = TRUE)) {
  get_gracefully("https://github.com/ready4-dev/ready4/releases.atom",
                 fn = tidyRSS::tidyfeed)
    get_gracefully("DOES_NOT_EXIST", fn = tidyRSS::tidyfeed)
}
}
```
