# Reorders layout observations based on specific statistics.

Reorders layout observations based on specific statistics.

## Usage

``` r
align_order2(
  stat,
  ...,
  reverse = FALSE,
  strict = TRUE,
  data = NULL,
  active = NULL
)
```

## Arguments

- stat:

  A statistical function which accepts a data and returns the statistic,
  which we'll call
  [`order2()`](https://yunuuuu.github.io/ggalign/reference/order2.md) to
  extract the ordering information.

- ...:

  \<[dyn-dots](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Additional arguments passed to function provided in `stat` argument.

- reverse:

  A boolean value. Should the sort order be in reverse?

- strict:

  A boolean value indicates whether the order should be strict. If
  previous groups has been established, and strict is `FALSE`, this will
  reorder the observations in each group.

- data:

  A `matrix`, `data frame`, or atomic vector used as the input for the
  `stat` function. Alternatively, you can specify a `function`
  (including purrr-like lambda syntax) that will be applied to the
  layout matrix, transforming it as necessary for statistic
  calculations. By default, it will inherit from the layout matrix.

- active:

  A [`active()`](https://yunuuuu.github.io/ggalign/reference/active.md)
  object that defines the context settings when added to a layout.

## Details

**\[experimental\]**

The `align_order2()` function differs from
[`align_order()`](https://yunuuuu.github.io/ggalign/reference/align_order.md)
in that the `weights` argument in
[`align_order()`](https://yunuuuu.github.io/ggalign/reference/align_order.md)
must return atomic weights for each observation. In contrast, the `stat`
argument in `align_order2()` can return more complex structures, such as
[hclust](https://rdrr.io/r/stats/hclust.html) or
[dendrogram](https://rdrr.io/r/stats/dendrogram.html), among others.

Typically, you can achieve the functionality of `align_order2()` using
[`align_order()`](https://yunuuuu.github.io/ggalign/reference/align_order.md)
by manually extracting the ordering information from the statistic.

## Discrete Axis Alignment

It is important to note that we consider rows as observations, meaning
`vec_size(data)`/`NROW(data)` must match the number of observations
along the axis used for alignment (x-axis for a vertical stack layout,
y-axis for a horizontal stack layout).

## See also

[`order2()`](https://yunuuuu.github.io/ggalign/reference/order2.md)

## Examples

``` r
ggheatmap(matrix(rnorm(81), nrow = 9)) +
    anno_left() +
    align_order2(hclust2)
#> → heatmap built with `geom_tile()`
```
