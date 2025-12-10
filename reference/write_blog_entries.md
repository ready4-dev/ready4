# Write blog entries

write_blog_entries() is a Write function that writes a file to a
specified local directory. Specifically, this function implements an
algorithm to write blog entries. The function is called for its side
effects and does not return a value.

## Usage

``` r
write_blog_entries(
  dir_path_1L_chr,
  fl_nm_1L_chr,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  options_chr = c("Y", "N")
)
```

## Arguments

- dir_path_1L_chr:

  Directory path (a character vector of length one)

- fl_nm_1L_chr:

  File name (a character vector of length one)

- consent_1L_chr:

  Consent (a character vector of length one), Default: ”

- consent_indcs_int:

  Consent indices (an integer vector), Default: 1

- options_chr:

  Options (a character vector), Default: c("Y", "N")

## Value

No return value, called for side effects.

## See also

[`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
