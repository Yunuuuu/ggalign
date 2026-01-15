# Arrange plots horizontally or vertically

**\[stable\]**

If `limits` is provided, a continuous variable will be required and
aligned in the direction specified (`stack_continuous`). Otherwise, a
discrete variable will be required and aligned (`stack_discrete`).

Several aliases are provided for convenience:

- `stack_vertical`: A special case of `stack_layout` that sets
  `direction = "v"`.

- `stack_horizontal`: A special case of `stack_layout` that sets
  `direction = "h"`.

- `stack_discretev`: A special case of `stack_discrete` that sets
  `direction = "v"`.

- `stack_discreteh`: A special case of `stack_discrete` that sets
  `direction = "h"`.

- `stack_continuousv()`: A special case of `stack_free` that sets
  `direction = "v"`.

- `stack_continuoush()`: A special case of `stack_free` that sets
  `direction = "h"`.

For historical reasons, the following aliases are available:

- `stack_align` is an alias for `stack_discrete`.

- `stack_alignv` is an alias for `stack_discretev`.

- `stack_alignh` is an alias for `stack_discreteh`.

- `stack_free` is an alias for `stack_continuous`.

- `stack_freev` is an alias for `stack_continuousv`.

- `stack_freeh` is an alias for `stack_continuoush`.

## Usage

``` r
stack_layout(
  direction,
  data = NULL,
  ...,
  theme = NULL,
  sizes = NA,
  limits = waiver()
)

stack_horizontal(data = NULL, ..., limits = waiver())

stack_vertical(data = NULL, ..., limits = waiver())

stack_discrete(direction, data = NULL, ..., theme = NULL, sizes = NA)

stack_discretev(data = NULL, ...)

stack_discreteh(data = NULL, ...)

stack_continuous(
  direction,
  data = NULL,
  ...,
  limits = NULL,
  theme = NULL,
  sizes = NA
)

stack_continuousv(data = NULL, ...)

stack_continuoush(data = NULL, ...)
```

## Arguments

- direction:

  A string indicating the direction of the stack layout, either
  `"h"`(`horizontal`) or `"v"`(`vertical`).

- data:

  Default dataset to use for the layout. If not specified, it must be
  supplied in each plot added to the layout:

  - If `limits` is not provided,
    [`fortify_matrix()`](https://yunuuuu.github.io/ggalign/reference/fortify_matrix.md)
    will be used to get a matrix.

  - If `limits` is specified,
    [`fortify_data_frame()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.md)
    will be used to get a data frame.

- ...:

  Additional arguments passed to
  [`fortify_data_frame()`](https://yunuuuu.github.io/ggalign/reference/fortify_data_frame.md)
  or
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
  `stack_layout()` with a nested
  [`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md),
  it should be of length `3`, specifying the relative heights (for
  `direction = "h"`) or widths (for `direction = "v"`) to be applied to
  the layout.

- limits:

  A
  [`continuous_limits()`](https://yunuuuu.github.io/ggalign/reference/continuous_limits.md)
  object specifying the left/lower limit and the right/upper limit of
  the scale. Used to align the continuous axis.

## Value

A `StackLayout` object.

## Examples

``` r
set.seed(123)
small_mat <- matrix(rnorm(56), nrow = 7L)

stack_horizontal(small_mat) + align_dendro()


# this is the same with:
stack_discrete("h", small_mat) + align_dendro()


stack_discreteh(small_mat) + align_dendro()


# For vertical layout:
stack_vertical(small_mat) + align_dendro()

```
