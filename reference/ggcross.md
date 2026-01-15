# Connect two layout crosswise

`ggcross` resets the layout ordering index of a
[`stack_cross()`](https://yunuuuu.github.io/ggalign/reference/stack_cross.md).
This allows you to add other `align_*` objects to define a new layout
ordering index. Any objects added after `ggcross` will use this updated
layout ordering index. This feature is particularly useful for creating
`tanglegram` visualizations. `ggcross()` is an alias of `ggcross()`.

## Usage

``` r
ggcross(mapping = aes(), size = NULL, active = NULL, no_axes = deprecated())
```

## Arguments

- mapping:

  Default list of aesthetic mappings to use for plot. If not specified,
  must be supplied in each layer added to the plot.

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

`ggcross()` initializes a ggplot `data` and `mapping`.

`ggcross()` always applies a default mapping for the axis of the data
index in the layout. This mapping is `aes(y = .data$.y)` for horizontal
stack layout (including left and right annotation) and
`aes(x = .data$.x)` for vertical stack layout (including top and bottom
annotation).

The data in the underlying `ggplot` object will contain following
columns:

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

- `.hand`: a factor indicates the index groups.

- `.x`/`.y` and `.discrete_x`/`.discrete_y`: Integer indices for `x`/`y`
  coordinates, and a factor of the data labels (only applicable when
  names exist).

It is recommended to use `.x`/`.y`, or `.discrete_x`/`.discrete_y` as
the `x`/`y` mapping.
