# Manufacture a new object

manufacture() is a method that used data contained in a model module or
submodule to create a new object (other than a model module).

## Usage

``` r
manufacture(x, ...)
```

## Arguments

- x:

  A model module (an instance of a class that inherits from
  Ready4Module) or submodule (any S3 class instance)

- ...:

  Additional arguments

## Value

An object other than a model module (an instance of a class that
inherits from Ready4Module).
