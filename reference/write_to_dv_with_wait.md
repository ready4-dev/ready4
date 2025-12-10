# Write to dataverse with wait

write_to_dv_with_wait() is a Write function that writes a file to a
specified local directory. Specifically, this function implements an
algorithm to write to dataverse with wait. The function returns Dataset
(a list).

## Usage

``` r
write_to_dv_with_wait(
  dss_tb,
  dv_nm_1L_chr,
  ds_url_1L_chr,
  parent_dv_dir_1L_chr,
  paths_to_dirs_chr,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  make_local_copy_1L_lgl = FALSE,
  options_chr = c("Y", "N"),
  paths_are_rltv_1L_lgl = TRUE,
  inc_fl_types_chr = NA_character_,
  key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
  server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
  wait_time_in_secs_int = 5L
)
```

## Arguments

- dss_tb:

  Datasets (a tibble)

- dv_nm_1L_chr:

  Dataverse name (a character vector of length one)

- ds_url_1L_chr:

  Dataset url (a character vector of length one)

- parent_dv_dir_1L_chr:

  Parent dataverse directory (a character vector of length one)

- paths_to_dirs_chr:

  Paths to directories (a character vector)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- make_local_copy_1L_lgl:

  Make local copy (a logical vector of length one), Default: FALSE

- options_chr:

  Options (a character vector), Default: c("Y", "N")

- paths_are_rltv_1L_lgl:

  Paths are relative (a logical vector of length one), Default: TRUE

- inc_fl_types_chr:

  Include file types (a character vector), Default: 'NA'

- key_1L_chr:

  Key (a character vector of length one), Default:
  Sys.getenv("DATAVERSE_KEY")

- server_1L_chr:

  Server (a character vector of length one), Default:
  Sys.getenv("DATAVERSE_SERVER")

- wait_time_in_secs_int:

  Wait time in secs (an integer vector), Default: 5

## Value

Dataset (a list)
