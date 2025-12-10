# Write files to repository

write_fls_to_repo() is a Write function that writes a file to a
specified local directory. Specifically, this function implements an
algorithm to write files to repository. The function returns Identities
(an integer vector).

## Usage

``` r
write_fls_to_repo(
  paths_chr,
  descriptions_chr,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  ds_url_1L_chr = character(0),
  ds_ls = NULL,
  key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
  options_chr = c("Y", "N"),
  piggyback_desc_1L_chr = "Documentation",
  piggyback_tag_1L_chr = "Documentation_0.0",
  piggyback_to_1L_chr = character(0),
  prerelease_1L_lgl = TRUE,
  server_1L_chr = Sys.getenv("DATAVERSE_SERVER")
)
```

## Arguments

- paths_chr:

  Paths (a character vector)

- descriptions_chr:

  Descriptions (a character vector)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- ds_url_1L_chr:

  Dataset url (a character vector of length one), Default: character(0)

- ds_ls:

  Dataset (a list), Default: NULL

- key_1L_chr:

  Key (a character vector of length one), Default:
  Sys.getenv("DATAVERSE_KEY")

- options_chr:

  Options (a character vector), Default: c("Y", "N")

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

Identities (an integer vector)
