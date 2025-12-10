# Apply the share method to a model module slot

shareSlot() is a convenience method that applies the share method to a
specified slot of a model module.

shareSlot method applied to Ready4Module

## Usage

``` r
shareSlot(x, slot_nm_1L_chr, ...)

# S4 method for class 'Ready4Module'
shareSlot(x, slot_nm_1L_chr, ...)
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
value (when called purely for side effects).

Either a ready4 model module (an instance of a class that inherits from
Ready4Module) of the same class as that supplied to the method or no
value (when called purely for side effects).
