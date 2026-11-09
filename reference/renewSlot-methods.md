# Update the value stored in a specified slot

renewSlot() updates the value stored in a specified slot of a Ready4
object. If no replacement value is supplied, the renew method is first
applied to the current slot value and the result is written back to the
slot.

Update the value stored in a specified slot of a Ready4 object. If no
replacement value is supplied, the renew method is first applied to the
slot value and the result is written back to the slot.

## Usage

``` r
renewSlot(x, slot_nm_1L_chr, new_val_xx = "use_renew_mthd", ...)

# S4 method for class 'Ready4'
renewSlot(x, slot_nm_1L_chr, new_val_xx = "use_renew_mthd", ...)
```

## Arguments

- x:

  An object that inherits from Ready4.

- slot_nm_1L_chr:

  Name of the slot to update (a character vector of length one).

- new_val_xx:

  New value to assign to the specified slot. If equal to
  "use_renew_mthd", the renew method is applied to the current slot
  value and the result is assigned to the slot.

- ...:

  Additional arguments passed to renew when new_val_xx is
  "use_renew_mthd".

## Value

An updated version of the supplied object.

An updated version of the supplied object.

## Details

Update the value stored in a specified slot of a Ready4 object.

The Ready4 class itself is slotless. This method is therefore intended
primarily for classes that inherit from Ready4 and define one or more
slots.

procureSlot and renewSlot provide the core slot access mechanisms used
by many Ready4 slot methods.
