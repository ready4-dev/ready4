# Rowbind tibbles in ready4 module object

rowbind_tbs_in_r4_obj() is a Rowbind function that performs custom
rowbind operations on table objects. Specifically, this function
implements an algorithm to rowbind tibbles in ready4 module object. The
function returns Tibbles (a ready4 module).

## Usage

``` r
rowbind_tbs_in_r4_obj(tbs_r4, slot_nm_1L_chr, second_tbs_r4, r4_name_1L_chr)
```

## Arguments

- tbs_r4:

  Tibbles (a ready4 module)

- slot_nm_1L_chr:

  Slot name (a character vector of length one)

- second_tbs_r4:

  Second tibbles (a ready4 module)

- r4_name_1L_chr:

  Ready4 module name (a character vector of length one)

## Value

Tibbles (a ready4 module)
