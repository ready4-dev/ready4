# Make files tibble

make_files_tb() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make files
tibble. The function returns Files (a tibble).

## Usage

``` r
make_files_tb(paths_to_dirs_chr, recode_ls, inc_fl_types_chr = NA_character_)
```

## Arguments

- paths_to_dirs_chr:

  Paths to directories (a character vector)

- recode_ls:

  Recode (a list)

- inc_fl_types_chr:

  Include file types (a character vector), Default: 'NA'

## Value

Files (a tibble)
