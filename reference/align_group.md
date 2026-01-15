# Group and align observations based on a group vector

**\[stable\]**

Splits observations into groups, with slice ordering based on group
levels.

## Usage

``` r
align_group(group, active = NULL)
```

## Arguments

- group:

  A character define the groups of the observations.

- active:

  A [`active()`](https://yunuuuu.github.io/ggalign/reference/active.md)
  object that defines the context settings when added to a layout.

## Examples

``` r
set.seed(1L)
small_mat <- matrix(rnorm(81), nrow = 9)
ggheatmap(small_mat) +
    anno_top() +
    align_group(sample(letters[1:4], ncol(small_mat), replace = TRUE))
#> → heatmap built with `geom_tile()`
```
