# Exhibit features of model module data by printing them to the R console

exhibit() is a method that prints to console selected features of data
contained in a model module.

## Usage

``` r
exhibit(x, ...)
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
