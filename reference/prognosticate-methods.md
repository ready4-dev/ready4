# Prognosticate (make predictions) by solving a forward problem

prognosticate() is a method that applies an algorithm to data contained
in a model module to solve a forward problem (i.e., use simulation and
statistical methods to make predictions).

## Usage

``` r
prognosticate(x, ...)
```

## Arguments

- x:

  A model module (an instance of a class that inherits from
  Ready4Module) or submodule (any S3 class instance)

- ...:

  Additional arguments

## Value

A model module (an instance of a class that inherits from Ready4Module).
