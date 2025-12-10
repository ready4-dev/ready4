# Enhance a model module by adding new elements

enhance() is a method that adds new data fields (columns for tabular
data, elements for arrays) and values to a model module by transforming
it into a module of an inheriting class.

## Usage

``` r
enhance(x, ...)
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
to the method.
