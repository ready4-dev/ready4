# Author and document datasets

authorData() is a method that authors, documents and saves model module
datasets.

## Usage

``` r
authorData(x, ...)
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
