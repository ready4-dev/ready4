# Add scroll box

add_scroll_box() is an Add function that updates an object by adding new
values to new or empty fields. Specifically, this function implements an
algorithm to add scroll box. The function is called for its side effects
and does not return a value.

## Usage

``` r
add_scroll_box(
  table_kbl,
  scroll_height_1L_chr = character(0),
  scroll_width_1L_chr = character(0),
  ...
)
```

## Arguments

- table_kbl:

  PARAM_DESCRIPTION

- scroll_height_1L_chr:

  Scroll height (a character vector of length one), Default:
  character(0)

- scroll_width_1L_chr:

  Scroll width (a character vector of length one), Default: character(0)

- ...:

  Additional arguments

## Value

Table (a kable)
