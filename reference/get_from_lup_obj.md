# Get a value from a lookup table

get_from_lup_obj() retrieves from a lookup table (a data.frame) the
values in a target column for cases where values in a second column
match a specified value.

## Usage

``` r
get_from_lup_obj(
  data_lookup_tb,
  match_value_xx,
  match_var_nm_1L_chr,
  target_var_nm_1L_chr,
  evaluate_1L_lgl = FALSE
)
```

## Arguments

- data_lookup_tb:

  Data lookup (a tibble)

- match_value_xx:

  Match value (an output object of multiple potential types)

- match_var_nm_1L_chr:

  Match variable name (a character vector of length one)

- target_var_nm_1L_chr:

  Target variable name (a character vector of length one)

- evaluate_1L_lgl:

  Evaluate (a logical vector of length one), Default: FALSE

## Value

Cell value (an output object of multiple potential types)

## Examples

``` r
lookup_tb <- tibble::tibble(Name = c("Sajid","Siobhan"),
                            Treat = c("Cake", "Chocolate"))
get_from_lup_obj(lookup_tb, match_value_xx = "Siobhan",
                 match_var_nm_1L_chr = "Name", target_var_nm_1L_chr = "Treat")
#> [1] "Chocolate"
get_from_lup_obj(lookup_tb, match_value_xx = "Cake",
                 match_var_nm_1L_chr = "Treat", target_var_nm_1L_chr = "Name")
#> [1] "Sajid"
```
