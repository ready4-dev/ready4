# Add lookup tables

add_lups() is an Add function that updates an object by adding new
values to new or empty fields. Specifically, this function implements an
algorithm to add lookup tables. The function is called for its side
effects and does not return a value.

## Usage

``` r
add_lups(
  template_lup,
  new_lup,
  key_var_nm_1L_chr,
  priority_lup_for_dupls_1L_chr = "template"
)
```

## Arguments

- template_lup:

  Template (a lookup table)

- new_lup:

  New (a lookup table)

- key_var_nm_1L_chr:

  Key variable name (a character vector of length one)

- priority_lup_for_dupls_1L_chr:

  Priority lookup table for duplicates (a character vector of length
  one), Default: 'template'

## Value

Combined (lookup tables)

## See also

[`Hmisc::label()`](https://rdrr.io/pkg/Hmisc/man/label.html)
