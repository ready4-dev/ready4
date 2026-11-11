# Extract class metadata from a pkgdown reference index

get_pkgdown_classes_tb() extracts S3 and S4 class names from a pkgdown
reference index page and returns the results in a standardised tibble.

S3 classes are identified from entries listed under the "S3 Classes"
heading and are recorded with old_class_lgl set to TRUE. S4 classes are
identified from entries listed under the "S4 Classes" heading and are
recorded with old_class_lgl set to FALSE.

## Usage

``` r
get_pkgdown_classes_tb(
  pkg_nm_1L_chr,
  url_stub_1L_chr = "https://ready4-dev.github.io/"
)
```

## Arguments

- pkg_nm_1L_chr:

  Package name (a character vector of length one).

- url_stub_1L_chr:

  URL stub (a character vector of length one), Default:
  "https://ready4-dev.github.io/".

## Value

A tibble with three columns:

- type_chr:

  Class name.

- pt_ns_chr:

  Package name.

- old_class_lgl:

  Logical indicator of class type. TRUE denotes an S3 class and FALSE
  denotes an S4 class.

## Details

The function constructs a pkgdown reference index URL of the form:

`paste0(url_stub_1L_chr, pkg_nm_1L_chr, "/reference/index.html")`

and extracts class metadata from the "S3 Classes" and "S4 Classes"
sections of that page.

## Examples

``` r
if (FALSE) { # \dontrun{
get_pkgdown_classes_tb("ready4use")
get_pkgdown_classes_tb("replica")
} # }
```
