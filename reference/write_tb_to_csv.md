# Write tibble to comma separated variables file

write_tb_to_csv() is a Write function that writes a file to a specified
local directory. Specifically, this function implements an algorithm to
write tibble to comma separated variables file. The function is called
for its side effects and does not return a value.

## Usage

``` r
write_tb_to_csv(
  tbs_r4,
  slot_nm_1L_chr,
  r4_name_1L_chr,
  lup_dir_1L_chr,
  pfx_1L_chr,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  options_chr = c("Y", "N")
)
```

## Arguments

- tbs_r4:

  Tibbles (a ready4 module)

- slot_nm_1L_chr:

  Slot name (a character vector of length one)

- r4_name_1L_chr:

  Ready4 module name (a character vector of length one)

- lup_dir_1L_chr:

  Lookup table directory (a character vector of length one)

- pfx_1L_chr:

  Prefix (a character vector of length one)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- options_chr:

  Options (a character vector), Default: c("Y", "N")

## Value

No return value, called for side effects.
