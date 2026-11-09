# Retrieve the value stored in a specified slot

procureSlot() retrieves the value stored in a specified slot of a Ready4
object. Optionally, the procure method can be applied to the retrieved
value before it is returned.

Retrieve the value stored in a specified slot of a Ready4 object.
Optionally apply the procure method to the retrieved value before
returning it.

## Usage

``` r
procureSlot(x, slot_nm_1L_chr, ...)

# S4 method for class 'Ready4'
procureSlot(x, slot_nm_1L_chr, use_procure_mthd_1L_lgl = FALSE, ...)
```

## Arguments

- x:

  An object that inherits from Ready4.

- slot_nm_1L_chr:

  Name of the slot to retrieve (a character vector of length one).

- ...:

  Additional arguments passed to procure when use_procure_mthd_1L_lgl is
  TRUE.

- use_procure_mthd_1L_lgl:

  Logical indicating whether the procure method should be applied to the
  slot value before it is returned.

## Value

The value stored in the specified slot, or the object returned by
applying procure to that value.

The value stored in the specified slot, or the object returned by
applying procure to that value.

## Details

Retrieve the value stored in a specified slot of a Ready4 object.

The Ready4 class itself is slotless. This method is therefore intended
primarily for classes that inherit from Ready4 and define one or more
slots.

procureSlot and renewSlot provide the core slot access mechanisms used
by many Ready4 slot methods.
