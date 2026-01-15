# Split observations by k-means clustering groups.

**\[stable\]**

Aligns and groups observations based on k-means clustering, enabling
observation splits by cluster groups.

## Usage

``` r
align_kmeans(..., data = NULL, active = NULL)
```

## Arguments

- ...:

  Arguments passed on to
  [`stats::kmeans`](https://rdrr.io/r/stats/kmeans.html)

  `iter.max`

  :   the maximum number of iterations allowed.

  `nstart`

  :   if `centers` is a number, how many random sets should be chosen?

  `algorithm`

  :   character: may be abbreviated. Note that `"Lloyd"` and `"Forgy"`
      are alternative names for one algorithm.

  `trace`

  :   logical or integer number, currently only used in the default
      method (`"Hartigan-Wong"`): if positive (or true), tracing
      information on the progress of the algorithm is produced. Higher
      values may produce more tracing information.

- data:

  A numeric matrix to be used by k-means. By default, it will inherit
  from the layout matrix.

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
    anno_top() +
    align_kmeans(3L)
#> → heatmap built with `geom_tile()`
```
