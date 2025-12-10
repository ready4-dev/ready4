# Author and document classes

authorClasses() is a method that authors and saves R package files for
creating and documenting classes to describe the data structures of
model modules.

## Usage

``` r
authorClasses(x, ...)
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
