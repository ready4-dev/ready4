# Apply the depict method to a model module slot

depictSlot() is a convenience method that applies the depict method to a
specified slot of a model module.

depictSlot method applied to Ready4Module

## Usage

``` r
depictSlot(x, slot_nm_1L_chr, ...)

# S4 method for class 'Ready4Module'
depictSlot(x, slot_nm_1L_chr, ...)
```

## Arguments

- x:

  An object of class Ready4Module

- slot_nm_1L_chr:

  Slot name (a length one character vector)

- ...:

  Additional arguments

## Value

Either a model module (an instance of a class that inherits from
Ready4Module) of the same class as that supplied to the method or no
value (when called for side effects only).

Either a ready4 model module (an instance of a class that inherits from
Ready4Module) of the same class as that supplied to the method or no
value (when called for side effects only).
