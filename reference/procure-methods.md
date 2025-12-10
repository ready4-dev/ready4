# Procure data from a model module

procure() is a "getter" method that retrieves data contained within a
model module or sub-module.

## Usage

``` r
procure(x, ...)
```

## Arguments

- x:

  A model module (an instance of a class that inherits from
  Ready4Module) or submodule (any S3 class instance)

- ...:

  Additional arguments

## Value

An object of the same class as that supplied to the method or of one of
the same classes that constitute the input object's slots or elements.
