# Determine the active context of stack layout

**\[stable\]**

`stack_active` is an alias for `stack_switch()`, which sets
`what = NULL` by default.

## Usage

``` r
stack_switch(sizes = NULL, what = waiver(), ...)

stack_active(sizes = NULL, ...)
```

## Arguments

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

- what:

  What should get activated for the stack layout? A single number or
  string of the plot elements in the layout. If `NULL`, will remove any
  active context, this is useful when the active context is a
  [`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md)
  object, where any `align_*()` will be added to the
  [`quad_layout()`](https://yunuuuu.github.io/ggalign/reference/quad_layout.md).
  By removing the active context, we can add `align_*()` into the
  [`stack_layout()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md).

- ...:

  These dots are for future extensions and must be empty.

## Value

A `stack_switch` object which can be added to
[`stack_layout()`](https://yunuuuu.github.io/ggalign/reference/stack_layout.md).

## Examples

``` r
stack_discrete("h", matrix(1:9, nrow = 3L)) +
    ggheatmap() +
    # ggheamtap will set the active context, directing following addition
    # into the heatmap plot area. To remove the heatmap active context,
    # we can use `stack_active()` which will direct subsequent addition into
    # the stack
    stack_active() +
    # here we add a dendrogram to the stack.
    align_dendro()
#> → heatmap built with `geom_tile()`
```
