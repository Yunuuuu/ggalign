# Add ggplot to layout without alignment

**\[experimental\]**

The `ggfree()` function allows you to incorporate a ggplot object into
your layout. Unlike
[`ggalign()`](https://yunuuuu.github.io/ggalign/reference/ggalign.md),
which aligns every axis value precisely, `ggfree()` focuses on
integrating plots into the layout without enforcing strict axis
alignment.

## Usage

``` r
ggfree(data = waiver(), ..., size = NULL, active = NULL)

# Default S3 method
ggfree(data = waiver(), mapping = aes(), ..., size = NULL, active = NULL)
```

## Arguments

- data:

  The following options can be used:

  - `NULL`: No data is set.

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html):
    Inherits the data from the layout matrix.

  - A `function` (including purrr-like lambda syntax): Applied to the
    layout matrix to transform the data before use. To transform the
    final plot data, please use
    [`scheme_data()`](https://yunuuuu.github.io/ggalign/reference/scheme_data.md).

  - A `matrix`, `data.frame`, or atomic vector.

- ...:

  \<[dyn-dots](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Additional arguments passed to
  [`fortify_data_frame()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.md).

- size:

  The relative size of the plot, can be specified as a
  [`unit()`](https://rdrr.io/r/grid/unit.html). Note that for
  [`circle_layout()`](https://yunuuuu.github.io/ggalign/reference/circle_layout.md),
  all size values will be interpreted as relative sizes, as this layout
  type adjusts based on the available space in the circular arrangement.

- active:

  A [`active()`](https://yunuuuu.github.io/ggalign/reference/active.md)
  object that defines the context settings when added to a layout.

- mapping:

  Default list of aesthetic mappings to use for plot. If not specified,
  must be supplied in each layer added to the plot.

## ggplot2 specification

`ggalign` initializes a ggplot object. The underlying data is created
using
[`fortify_data_frame()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.md).
Please refer to this method for more details.

When used in
[`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)/[`ggheatmap()`](https://yunuuuu.github.io/ggalign/reference/heatmap_layout.md),
if the data is inherited from the
[`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)
and the other direction aligns discrete variables, following columns
will be added:

- `.extra_panel`: Provides the panel information for the column (left or
  right annotation) or row (top or bottom annotation).

- `.extra_index`: The index information for the column (left or right
  annotation) or row (top or bottom annotation).

## Examples

``` r
ggheatmap(matrix(rnorm(56), nrow = 7)) +
    anno_top() +
    align_dendro() +
    ggfree(mtcars, aes(wt, mpg)) +
    geom_point()
#> → heatmap built with `geom_tile()`
```
