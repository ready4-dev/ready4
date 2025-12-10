# Add rows from function arguments

add_rows_from_fn_args() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add rows from function arguments. The
function returns Table (a ready4 submodule).

## Usage

``` r
add_rows_from_fn_args(tbl_r3, fn, fn_env_ls)
```

## Arguments

- tbl_r3:

  Table (a ready4 submodule)

- fn:

  Function (a function)

- fn_env_ls:

  Function (a list of environments)

## Value

Table (a ready4 submodule)

## See also

[`Hmisc::label()`](https://rdrr.io/pkg/Hmisc/man/label.html)
