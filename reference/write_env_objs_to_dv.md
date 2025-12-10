# Write environment objects to dataverse

write_env_objs_to_dv() is a Write function that writes a file to a
specified local directory. Specifically, this function implements an
algorithm to write environment objects to dataverse. The function
returns File identities (an integer vector).

## Usage

``` r
write_env_objs_to_dv(
  env_objects_ls,
  descriptions_chr,
  ds_url_1L_chr,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
  options_chr = c("Y", "N"),
  publish_dv_1L_lgl = FALSE,
  piggyback_desc_1L_chr = "Documentation",
  piggyback_tag_1L_chr = "Documentation_0.0",
  piggyback_to_1L_chr = character(0),
  prerelease_1L_lgl = TRUE,
  server_1L_chr = Sys.getenv("DATAVERSE_SERVER")
)
```

## Arguments

- env_objects_ls:

  Environment objects (a list)

- descriptions_chr:

  Descriptions (a character vector)

- ds_url_1L_chr:

  Dataset url (a character vector of length one)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- key_1L_chr:

  Key (a character vector of length one), Default:
  Sys.getenv("DATAVERSE_KEY")

- options_chr:

  Options (a character vector), Default: c("Y", "N")

- publish_dv_1L_lgl:

  Publish dataverse (a logical vector of length one), Default: FALSE

- piggyback_desc_1L_chr:

  Piggyback description (a character vector of length one), Default:
  'Documentation'

- piggyback_tag_1L_chr:

  Piggyback tag (a character vector of length one), Default:
  'Documentation_0.0'

- piggyback_to_1L_chr:

  Piggyback to (a character vector of length one), Default: character(0)

- prerelease_1L_lgl:

  Prerelease (a logical vector of length one), Default: TRUE

- server_1L_chr:

  Server (a character vector of length one), Default:
  Sys.getenv("DATAVERSE_SERVER")

## Value

File identities (an integer vector)
