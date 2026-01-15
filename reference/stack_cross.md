# Arrange plots crosswise horizontally or vertically

**\[experimental\]**

The `stack_cross` function is derived from
[`stack_discrete()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md)
and allows for different layout ordering indices within a single layout.

Two aliases are provided for convenience:

- `stack_crossv`: A special case of `stack_cross` that sets
  `direction = "v"` for vertical alignment.

- `stack_crossh`: A special case of `stack_cross` that sets
  `direction = "h"` for horizontal alignment.

## Usage

``` r
stack_cross(direction, data = NULL, ..., theme = NULL, sizes = NA)

stack_crossv(data = NULL, ...)

stack_crossh(data = NULL, ...)
```

## Arguments

- direction:

  A string indicating the direction of the stack layout, either
  `"h"`(`horizontal`) or `"v"`(`vertical`).

- data:

  Default dataset to use for the layout. If not specified, it must be
  supplied in each plot added to the layout,
  [`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md)
  will be used to convert the data to a matrix.

- ...:

  Additional arguments passed to
  [`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md).

- theme:

  A [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
  object used to customize various elements of the layout, including
  `guides`, `title`, `subtitle`, `caption`, `margins`, `panel.border`,
  and `background`. By default, the theme will inherit from the parent
  `layout`. It also controls the panel spacing for all plots in the
  layout.

- sizes:

  A numeric value or a [`unit`](https://rdrr.io/r/grid/unit.html)
  object. When used for the
  [`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)
  annotation, it must be of length `1`. When used in the
  [`stack_layout()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md)
  with a nested
  [`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md),
  it should be of length `3`, specifying the relative heights (for
  `direction = "h"`) or widths (for `direction = "v"`) to be applied to
  the layout.

## See also

[`ggcross()`](https://yunuuuu.github.io/ggalign/reference/ggcross.md)
