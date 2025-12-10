# Write project output directories

write_prj_outp_dirs() is a Write function that writes a file to a
specified local directory. Specifically, this function implements an
algorithm to write project output directories. The function returns New
paths (a list).

## Usage

``` r
write_prj_outp_dirs(
  prj_dirs_chr,
  output_data_dir_1L_chr,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  options_chr = c("Y", "N"),
  paths_ls = NULL
)
```

## Arguments

- prj_dirs_chr:

  Project directories (a character vector)

- output_data_dir_1L_chr:

  Output data directory (a character vector of length one)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- options_chr:

  Options (a character vector), Default: c("Y", "N")

- paths_ls:

  Paths (a list), Default: NULL

## Value

New paths (a list)
