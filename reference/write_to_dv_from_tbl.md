# Write to dataverse from table

write_to_dv_from_tbl() is a Write function that writes a file to a
specified local directory. Specifically, this function implements an
algorithm to write to dataverse from table. The function returns File
identities (an integer vector).

## Usage

``` r
write_to_dv_from_tbl(
  files_tb,
  ds_url_1L_chr,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  data_dir_rt_1L_chr = ".",
  key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
  options_chr = c("Y", "N"),
  server_1L_chr = Sys.getenv("DATAVERSE_SERVER")
)
```

## Arguments

- files_tb:

  Files (a tibble)

- ds_url_1L_chr:

  Dataset url (a character vector of length one)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- data_dir_rt_1L_chr:

  Data directory root (a character vector of length one), Default: '.'

- key_1L_chr:

  Key (a character vector of length one), Default:
  Sys.getenv("DATAVERSE_KEY")

- options_chr:

  Options (a character vector), Default: c("Y", "N")

- server_1L_chr:

  Server (a character vector of length one), Default:
  Sys.getenv("DATAVERSE_SERVER")

## Value

File identities (an integer vector)
