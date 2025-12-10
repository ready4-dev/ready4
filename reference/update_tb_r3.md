# Update tibble ready4 submodule

update_tb_r3() is an Update function that edits an object, while
preserving core object attributes. Specifically, this function
implements an algorithm to update tibble ready4 submodule. The function
returns Tibble ready4 submodule (a ready4 submodule extension of
tibble).

## Usage

``` r
update_tb_r3(
  tb_r3,
  case_when_false_1L_chr = NA_character_,
  case_when_true_1L_chr = NA_character_,
  case_when_true_ls = NULL,
  case_when_var_1L_chr = NA_character_,
  filter_cdn_1L_chr = NA_character_,
  fn = NULL,
  fn_env_ls = NULL,
  slice_indcs_int = NA_integer_,
  tf_false_val_1L_lgl = FALSE
)
```

## Arguments

- tb_r3:

  Tibble ready4 submodule (a ready4 submodule extension of tibble)

- case_when_false_1L_chr:

  Case when false (a character vector of length one), Default: 'NA'

- case_when_true_1L_chr:

  Case when true (a character vector of length one), Default: 'NA'

- case_when_true_ls:

  Case when true (a list), Default: NULL

- case_when_var_1L_chr:

  Case when variable (a character vector of length one), Default: 'NA'

- filter_cdn_1L_chr:

  Filter condition (a character vector of length one), Default: 'NA'

- fn:

  Function (a function), Default: NULL

- fn_env_ls:

  Function (a list of environments), Default: NULL

- slice_indcs_int:

  Slice indices (an integer vector), Default: NA

- tf_false_val_1L_lgl:

  Transform false value (a logical vector of length one), Default: FALSE

## Value

Tibble ready4 submodule (a ready4 submodule extension of tibble)
