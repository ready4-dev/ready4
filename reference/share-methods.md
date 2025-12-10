# Share data via an online repository

share() is a method that uploads data contained in a model module to an
online repository. If requested, the method will also publish the
updated repository.

## Usage

``` r
share(x, ...)
```

## Arguments

- x:

  A model module (an instance of a class that inherits from
  Ready4Module) or submodule (any S3 class instance)

- ...:

  Additional arguments

## Value

A model module (an instance of a class that inherits from Ready4Module)
or submodule (any S3 class instance) of the same class as that supplied
to the method or no return value (when called for side-effects only).
