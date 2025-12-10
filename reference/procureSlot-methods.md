# Procure (get) data from a slot

procureSlot() is a "getter" method that, depending on input arguments,
retrieves either data contained in a specified model module slot (the
default behaviour) or the value returned by applying the procure method
to the specified slot.

procureSlot method applied to Ready4Module

## Usage

``` r
procureSlot(x, slot_nm_1L_chr, ...)

# S4 method for class 'Ready4Module'
procureSlot(x, slot_nm_1L_chr, use_procure_mthd_1L_lgl = FALSE, ...)
```

## Arguments

- x:

  An object of class Ready4Module

- slot_nm_1L_chr:

  Slot name (a length one character vector)

- ...:

  Additional arguments

- use_procure_mthd_1L_lgl:

  Use procure method (a length one logical vector)

## Value

A model module (an instance of a class that inherits from Ready4Module)
of the same class as that supplied to the method or an instance of a
class contained in that Ready4Module's slots.

A ready4 model module (an instance of a class that inherits from
Ready4Module) of the same class as that supplied to the method or an
instance of a class contained in that Ready4Module's slots.

## Examples

``` r
X <- Ready4Module()
procureSlot(X, "dissemination_1L_chr")
#> [1] NA
```
