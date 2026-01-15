# Add ggplot by Aligning discrete or continuous variable

**\[stable\]**

`ggalign()` is similar to `ggplot` in that it initializes a `ggplot`
data and `mapping`. `ggalign()` allowing you to provide data in various
formats, including matrices, data frames, or simple vectors. By default,
it will inherit from the layout. If a function, it will apply with the
layout matrix. `ggalign()` focuses on integrating plots into a layout by
aligning the axes.

## Usage

``` r
ggalign(
  data = waiver(),
  mapping = aes(),
  ...,
  size = NULL,
  active = NULL,
  no_axes = deprecated()
)
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

- mapping:

  Default list of aesthetic mappings to use for plot. If not specified,
  must be supplied in each layer added to the plot.

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

- no_axes:

  **\[deprecated\]** Please add
  [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
  directly to the ggplot instead.

## ggplot2 specification

`ggalign` initializes a ggplot object. The underlying data is created
using
[`fortify_data_frame()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.md).
Please refer to it for more details.

When aligning discrete variables, `ggalign()` always applies a default
mapping for the axis of the data index in the layout. Specifically:

- `aes(y = .data$.y)` is used for the horizontal
  [`stack_layout()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md)
  (including left and right annotations).

- `aes(x = .data$.x)` is used for the vertical
  [`stack_layout()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md)
  (including top and bottom annotations) and
  [`circle_layout()`](https://yunuuuu.github.io/ggalign/reference/circle_layout.md).

The following columns will be added to the data frame to align discrete
variables:

- `.panel`: The panel for the aligned axis. Refers to the `x-axis` for
  vertical
  [`stack_layout()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md)
  (including top and bottom annotations), and the `y-axis` for
  horizontal
  [`stack_layout()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md)
  (including left and right annotations).

- `.names`
  ([`vec_names()`](https://vctrs.r-lib.org/reference/vec_names.html))
  and `.index`
  ([`vec_size()`](https://vctrs.r-lib.org/reference/vec_size.html)/[`NROW()`](https://rdrr.io/r/base/nrow.html)):
  Character names (if available) and the integer index of the original
  data.

- `.x`/`.y` and `.discrete_x`/`.discrete_y`: Integer indices for `x`/`y`
  coordinates, and a factor of the data labels (only applicable when
  names exist).

It is recommended to use `.x`/`.y`, or `.discrete_x`/`.discrete_y` as
the `x`/`y` mapping.

If the data inherits from
[`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)/[`ggheatmap()`](https://yunuuuu.github.io/ggalign/reference/heatmap_layout.md),
additional columns will be added:

- `.extra_panel`: Provides the panel information for the column (left or
  right annotation) or row (top or bottom annotation).

- `.extra_index`: The index information for the column (left or right
  annotation) or row (top or bottom annotation).

## Discrete Axis Alignment

It is important to note that we consider rows as observations, meaning
`vec_size(data)`/`NROW(data)` must match the number of observations
along the axis used for alignment (x-axis for a vertical stack layout,
y-axis for a horizontal stack layout).

## Examples

``` r
ggheatmap(matrix(rnorm(81), nrow = 9)) +
    anno_top() +
    ggalign() +
    geom_point(aes(y = value))
#> → heatmap built with `geom_tile()`


ggheatmap(matrix(rnorm(81), nrow = 9)) +
    anno_top(size = 0.5) +
    align_dendro(k = 3L) +
    ggalign(data = NULL, size = 0.2) +
    geom_tile(aes(y = 1L, fill = .panel))
#> → heatmap built with `geom_tile()`

```
