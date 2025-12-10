# Investigate solutions to an inverse problem

investigate() is a method that applies an algorithm to data contained in
a model module in order to solve an inverse problem (ie, identify a
statistical model that can generate approximations of that data).

## Usage

``` r
investigate(x, ...)
```

## Arguments

- x:

  A model module (an instance of a class that inherits from
  Ready4Module) or submodule (any S3 class instance)

- ...:

  Additional arguments

## Value

A model module (an instance of a class that inherits from Ready4Module)
or submodule (any S3 class instance).
