# Ratify that input or output data meet validity criteria

ratify() is a method that validates that a model module or submodule
conforms to specified internal consistency criteria, potentially
updating the invalid values in the model module so that these criteria
are met.

## Usage

``` r
ratify(x, ...)
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
