# Order observations based on weights

**\[stable\]**

Ordering observations based on summary weights or a specified ordering
character or integer index.

## Usage

``` r
align_order(
  weights = rowMeans,
  ...,
  reverse = FALSE,
  strict = TRUE,
  data = NULL,
  active = NULL
)
```

## Arguments

- weights:

  A summary function which accepts a data and returns the weights for
  each observations. Alternatively, you can provide an ordering index as
  either an integer or a character. Since characters have been
  designated as character indices, if you wish to specify a function
  name as a string, you must enclose it with
  [`I()`](https://rdrr.io/r/base/AsIs.html).

- ...:

  \<[dyn-dots](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Additional arguments passed to function provided in `weights`
  argument.

- reverse:

  A boolean value. Should the sort order be in reverse?

- strict:

  A boolean value indicates whether the order should be strict. If
  previous groups has been established, and strict is `FALSE`, this will
  reorder the observations in each group.

- data:

  A `matrix`, `data frame`, or atomic vector used as the input for the
  `weights` function. Alternatively, you can specify a `function`
  (including purrr-like lambda syntax) that will be applied to the
  layout matrix, transforming it as necessary for weight calculations.
  By default, it will inherit from the layout matrix.

- active:

  A [`active()`](https://yunuuuu.github.io/ggalign/reference/active.md)
  object that defines the context settings when added to a layout.

## Discrete Axis Alignment

It is important to note that we consider rows as observations, meaning
`vec_size(data)`/`NROW(data)` must match the number of observations
along the axis used for alignment (x-axis for a vertical stack layout,
y-axis for a horizontal stack layout).

## Examples

``` r
ggheatmap(matrix(rnorm(81), nrow = 9)) +
    anno_left() +
    align_order(I("rowMeans"))
#> → heatmap built with `geom_tile()`
```
