# Author and document functions

authorFunctions() is a method that authors and saves R package files
files necessary for creating and documenting functions that implement
model module algorithms.

## Usage

``` r
authorFunctions(x, ...)
```

## Arguments

- x:

  A model module (an instance of a class that inherits from
  Ready4Module) or submodule (any S3 class instance)

- ...:

  Additional arguments

## Value

Either a model module (an instance of a class that inherits from
Ready4Module) or submodule (any S3 class instance) of the same class as
that supplied to the method or no return value (when called for
side-effects only).
