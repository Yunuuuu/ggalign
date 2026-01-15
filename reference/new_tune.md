# Change the shape of the input object

- `new_tune`: Creates a new object by wrapping it in a scalar list with
  the specified attributes and class.

- `tune_data`: Retrieves the original input data.

## Usage

``` r
new_tune(x, ..., class = character())

tune_data(x)
```

## Arguments

- x:

  An R object.

- ...:

  Additional attributes passed to
  [`structure()`](https://rdrr.io/r/base/structure.html).

- class:

  A character vector specifying the class name to be added.
