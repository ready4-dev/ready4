# Write files to dataverse

write_fls_to_dv() is a Write function that writes a file to a specified
local directory. Specifically, this function implements an algorithm to
write files to dataverse. The function returns Identities (an integer
vector).

## Usage

``` r
write_fls_to_dv(
  file_paths_chr,
  ds_url_1L_chr,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  descriptions_chr = NULL,
  ds_ls = NULL,
  key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
  options_chr = c("Y", "N"),
  server_1L_chr = Sys.getenv("DATAVERSE_SERVER")
)
```

## Arguments

- file_paths_chr:

  File paths (a character vector)

- ds_url_1L_chr:

  Dataset url (a character vector of length one)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- descriptions_chr:

  Descriptions (a character vector), Default: NULL

- ds_ls:

  Dataset (a list), Default: NULL

- key_1L_chr:

  Key (a character vector of length one), Default:
  Sys.getenv("DATAVERSE_KEY")

- options_chr:

  Options (a character vector), Default: c("Y", "N")

- server_1L_chr:

  Server (a character vector of length one), Default:
  Sys.getenv("DATAVERSE_SERVER")

## Value

Identities (an integer vector)
