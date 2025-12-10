# Write dataverse file to local

write_dv_fl_to_loc() is a Write function that writes a file to a
specified local directory. Specifically, this function implements an
algorithm to write dataverse file to local. The function is called for
its side effects and does not return a value.

## Usage

``` r
write_dv_fl_to_loc(
  ds_url_1L_chr = character(0),
  dest_path_1L_chr,
  repo_fl_fmt_1L_chr,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  fl_nm_1L_chr = NA_character_,
  fl_id_1L_int = NA_integer_,
  key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
  options_chr = c("Y", "N"),
  save_type_1L_chr = "original",
  server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
  ds_ui_1L_chr = deprecated()
)
```

## Arguments

- ds_url_1L_chr:

  Dataset url (a character vector of length one), Default: character(0)

- dest_path_1L_chr:

  Destination path (a character vector of length one)

- repo_fl_fmt_1L_chr:

  Repository file format (a character vector of length one)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- fl_nm_1L_chr:

  File name (a character vector of length one), Default: 'NA'

- fl_id_1L_int:

  File identity (an integer vector of length one), Default: NA

- key_1L_chr:

  Key (a character vector of length one), Default:
  Sys.getenv("DATAVERSE_KEY")

- options_chr:

  Options (a character vector), Default: c("Y", "N")

- save_type_1L_chr:

  Save type (a character vector of length one), Default: 'original'

- server_1L_chr:

  Server (a character vector of length one), Default:
  Sys.getenv("DATAVERSE_SERVER")

- ds_ui_1L_chr:

  Dataset user interface (a character vector of length one), Default:
  deprecated()

## Value

No return value, called for side effects.
