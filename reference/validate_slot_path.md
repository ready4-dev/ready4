# validate_slot_path

Internal helper to validate a single or nested slot reference.

## Usage

``` r
validate_slot_path(x, slot_nm_1L_chr)
```

## Arguments

- x:

  An object that inherits from Ready4.

- slot_nm_1L_chr:

  Name of the slot path to validate (e.g., "slotA" or "slotA@slotB").

## Value

invisible(TRUE) if the path is fully valid. Throws an error otherwise.
