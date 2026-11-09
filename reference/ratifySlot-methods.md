# Apply the ratify method to a specified slot

ratifySlot() is a convenience method that applies the ratify method to a
specified slot of a Ready4 object and updates the object with the
result.

Apply the ratify method to the value stored in a specified slot of a
Ready4 object and assign the resulting value back to that slot.

## Usage

``` r
ratifySlot(x, slot_nm_1L_chr, ...)

# S4 method for class 'Ready4'
ratifySlot(x, slot_nm_1L_chr, ...)
```

## Arguments

- x:

  An object that inherits from Ready4.

- slot_nm_1L_chr:

  Name of the slot to which the ratify method should be applied (a
  character vector of length one).

- ...:

  Additional arguments passed to ratify.

## Value

An updated version of the supplied object.

An updated version of the supplied object.

## Details

Apply the ratify method to a specified slot of a Ready4 object and
update the object with the result.

The Ready4 class itself is slotless. This method is therefore intended
primarily for classes that inherit from Ready4 and define one or more
slots.
