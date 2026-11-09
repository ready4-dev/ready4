# Apply the investigate method to a specified slot

investigateSlot() is a convenience method that applies the investigate
method to a specified slot of a Ready4 object.

Apply the investigate method to the value stored in a specified slot of
a Ready4 object.

## Usage

``` r
investigateSlot(x, slot_nm_1L_chr, ...)

# S4 method for class 'Ready4'
investigateSlot(x, slot_nm_1L_chr, ...)
```

## Arguments

- x:

  An object that inherits from Ready4.

- slot_nm_1L_chr:

  Name of the slot to which the investigate method should be applied (a
  character vector of length one).

- ...:

  Additional arguments passed to investigate.

## Value

The object returned by applying investigate to the specified slot.

The object returned by applying investigate to the specified slot.

## Details

Apply the investigate method to a specified slot of a Ready4 object.

The Ready4 class itself is slotless. This method is therefore intended
primarily for classes that inherit from Ready4 and define one or more
slots.
