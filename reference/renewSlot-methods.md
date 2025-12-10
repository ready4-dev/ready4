# Renew (set) the values of data in a module slot

renewSlot() is a "setter" method that renews (sets) the value of a
specified model module slot with either the value returned by applying
the renew method to that slot (the default behaviour) or a supplied new
value.

renewSlot method applied to Ready4Module

## Usage

``` r
renewSlot(x, slot_nm_1L_chr, new_val_xx = "use_renew_mthd", ...)

# S4 method for class 'Ready4Module'
renewSlot(x, slot_nm_1L_chr, new_val_xx = "use_renew_mthd", ...)
```

## Arguments

- x:

  An object of class Ready4Module

- slot_nm_1L_chr:

  Slot name (a length one character vector)

- new_val_xx:

  New value (slot dependent object type), Default 'use_renew_mthd'

- ...:

  Additional arguments

## Value

A model module (an instance of a class that inherits from Ready4Module)
of the same class as that supplied to the method.

A ready4 model module (an instance of a class that inherits from
Ready4Module) of the same class as that supplied to the method.

## Examples

``` r
X <- Ready4Module()
X <- renewSlot(X, "dissemination_1L_chr", new_val_xx = "Some new text.")
```
