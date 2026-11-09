# Apply the reckon method to a specified slot

reckonSlot() is a convenience method that applies the reckon method to a
specified slot of a Ready4 object.

Apply the reckon method to the value stored in a specified slot of a
Ready4 object.

## Usage

``` r
reckonSlot(x, slot_nm_1L_chr, ...)

# S4 method for class 'Ready4'
reckonSlot(x, slot_nm_1L_chr, ...)
```

## Arguments

- x:

  An object that inherits from Ready4.

- slot_nm_1L_chr:

  Name of the slot to which the reckon method should be applied (a
  character vector of length one).

- ...:

  Additional arguments passed to reckon.

## Value

The object returned by applying reckon to the specified slot.

The object returned by applying reckon to the specified slot.

## Details

Apply the reckon method to a specified slot of a Ready4 object.

The Ready4 class itself is slotless. This method is therefore intended
primarily for classes that inherit from Ready4 and define one or more
slots.
