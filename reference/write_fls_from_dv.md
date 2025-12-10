# Write files from dataverse

write_fls_from_dv() is a Write function that writes a file to a
specified local directory. Specifically, this function implements an
algorithm to write files from dataverse. The function is called for its
side effects and does not return a value.

## Usage

``` r
write_fls_from_dv(
  files_tb,
  fl_ids_int,
  ds_url_1L_chr,
  local_dv_dir_1L_chr,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
  options_chr = c("Y", "N"),
  server_1L_chr = Sys.getenv("DATAVERSE_SERVER")
)
```

## Arguments

- files_tb:

  Files (a tibble)

- fl_ids_int:

  File identities (an integer vector)

- ds_url_1L_chr:

  Dataset url (a character vector of length one)

- local_dv_dir_1L_chr:

  Local dataverse directory (a character vector of length one)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- key_1L_chr:

  Key (a character vector of length one), Default:
  Sys.getenv("DATAVERSE_KEY")

- options_chr:

  Options (a character vector), Default: c("Y", "N")

- server_1L_chr:

  Server (a character vector of length one), Default:
  Sys.getenv("DATAVERSE_SERVER")

## Value

No return value, called for side effects.
